server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -18.455845, lat = -3.26, zoom = 2) %>%
      addProviderTiles(providers$OpenStreetMap.France, options = providerTileOptions(noWrap = TRUE))
  })
  
  observe({
    click <- mapClick()
    
    if (is.null(click)) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        clearPopups() %>%
        addPopups(
          lng = -71.10, lat = 42.37, "Click location on map to see geolocation enabled events, food and vendors",
          options = popupOptions(closeButton = FALSE)
        )
    }
    else {
      withProgress(message = "Application loading", value = 0, {
        meetupIcon <- makeIcon(
          iconUrl = "https://cdn-images-1.medium.com/max/1600/1*EK8I8k19h9V04NnyO59C_A.png",
          iconWidth = 45, iconHeight = 40,
          iconAnchorX = 0, iconAnchorY = 0
        )
        
        eventBrightIcon <- makeIcon(
          iconUrl = "https://cdn.evbstatic.com/s3-build/perm_001/e04503/django/images/favicons/favicon-194x194.png",
          iconWidth = 30, iconHeight = 30,
          iconAnchorX = 0, iconAnchorY = 0
        )
        
        googleIcon <- makeIcon(
          # iconUrl = "http://www.ditoweb.com/wp-content/uploads/2016/05/geolocation-api.png",
          iconUrl = "http://www.ceda.cz/files/logo/google/google_2016/icon_placesapi.png",
          iconWidth = 40, iconHeight = 40,
          iconAnchorX = 0, iconAnchorY = 0
        )
        
        price <- c(NA, 0, 1, 2, 3, 4)
        level <- c("NA", "Free", "Inexpensive", "Moderate", "Expensive", "Very Expensive")
        googlePrices <- data.frame(price, level)
        
        yelpIcon <- makeIcon(
          iconUrl = "https://networkprogramming.files.wordpress.com/2016/09/yelp.png",
          iconWidth = 40, iconHeight = 40,
          iconAnchorX = 0, iconAnchorY = 0
        )
        
        click <- mapClick()
        
        clat <- click$lat
        clng <- click$lng
        
        
        distance <- input$miles * (1609.344)
        
        incProgress(0.3, detail = "Getting api data")
        
        url <- paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", clat, ",", clng, "&radius=", distance, "&types=", input$variable, "&key=KEY", sep = "")
        document <- fromJSON(txt = url)
        dat <- data.frame(
          document$results$geometry$location$lng, document$results$geometry$location$lat,
          document$results$name
        )
        
        eventBCategoryUrl <- "https://www.eventbriteapi.com/v3/categories/?&token=TOKEN"
        eventCategories <- fromJSON(txt = eventBCategoryUrl)
        eventCategories2 <- data.frame(eventCategories$categories$id, eventCategories$categories$name)
        colnames(eventCategories2) <- c("id", "name")
        eventCategories2$name <- tolower(eventCategories2$name)
        
        radiusYelp <- input$miles / 1609.344
        url2 <- modify_url(
          "https://api.yelp.com", path = c("v3", "businesses", "search"),
          query = list(
            term = input$yelpSearch, latitude = clat, longitude = clng, radius_filter = radiusYelp,
            limit = 10
          )
        )
        res <- GET(url2, add_headers("Authorization" = paste("bearer", "API_KEY")))
        ct <- content(res)
        
        incProgress(0.4, detail = "Gathering data")
        
        if (input$meetupTopic == "") {
          eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&token=TOKEN&expand=venue", sep = "")
          json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
          resEventB <- fromJSON(txt = eventBUrl)
          res <- fromJSON(txt = json.url)
        }
        else {
          eventID <- eventCategories2[str_detect(eventCategories2$name, input$meetupTopic), ]
          eventid <- eventID$id
          json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
          errorRes <- try(fromJSON(txt = json.url), TRUE)
          if (length(eventid) == 0 & length(grep("open.connection", errorRes)) == 1) # if error in both meetup and eventbrite apis
          {
            eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&token=TOKEN&expand=venue", sep = "")
            json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&fields=group_photos;key=KEY", sep = "")
            resEventB <- fromJSON(txt = eventBUrl)
            res <- fromJSON(txt = json.url)
          }
          else if (length(eventid) == 0 & length(grep("open.connection", errorRes)) == 0) # if error in eventbrite but no error in meetup
          {
            eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&token=TOKEN&expand=venue", sep = "")
            json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
            resEventB <- fromJSON(txt = eventBUrl)
            res <- fromJSON(txt = json.url)
          }
          else if (length(eventid) != 0 & length(grep("open.connection", errorRes)) == 1) # if no error in eventbrite but error in meetup
          {
            eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&categories=", eventid, "&token=TOKEN&expand=venue", sep = "")
            json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
            resEventB <- fromJSON(txt = eventBUrl)
            res <- fromJSON(txt = json.url)
          }
          else # no error in eventbrite and no error in meetup
          {
            eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&categories=", eventid, "&token=TOKEN&expand=venue", sep = "")
            json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
            resEventB <- fromJSON(txt = eventBUrl)
            res <- fromJSON(txt = json.url)
          }
        }
        
        if (length(res$results) == 0) # if no meetup observations
        {
          if (nrow(dat) == 0 & length(resEventB$events) == 0 & length(ct$businesses) == 1) {
            content <- paste("No places/events/food at this location", "\U0001f622")
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addPopups(
                clng, clat, "<br>", content, "</br>",
                options = popupOptions(closeButton = FALSE, closeOnClick = TRUE)
              )
          }
          else if (nrow(dat) == 0 & length(resEventB$events) == 0 & length(ct$businesses) > 1) {
            latitudeYelp <- vector("character", length(ct$businesses))
            longitudeYelp <- vector("character", length(ct$businesses))
            businessName <- vector("character", length(ct$businesses))
            imageURL <- vector("character", length(ct$businesses))
            isClosed <- vector("character", length(ct$businesses))
            businessRating <- vector("character", length(ct$businesses))
            businessPrice <- vector("character", length(ct$businesses))
            displayAddress <- vector("character", length(ct$businesses))
            
            for (i in 1:length(ct$businesses))
            {
              if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
                latitudeYelp[[i]] <- "NA"
              }
              else {
                latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
              }
              if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
                longitudeYelp[[i]] <- "NA"
              }
              else {
                longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
              }
              if (is.null(ct$businesses[[i]]$name)) {
                businessName[[i]] <- "NA"
              }
              else {
                businessName[[i]] <- ct$businesses[[i]]$name
              }
              if (is.null(ct$businesses[[i]]$image_url)) {
                imageURL[[i]] <- "NA"
              }
              else {
                imageURL[[i]] <- ct$businesses[[i]]$image_url
              }
              if (is.null(ct$businesses[[i]]$is_closed)) {
                isClosed[[i]] <- "NA"
              }
              else {
                isClosed[[i]] <- ct$businesses[[i]]$is_closed
              }
              if (is.null(ct$businesses[[i]]$rating)) {
                businessRating[[i]] <- "NA"
              }
              else {
                businessRating[[i]] <- ct$businesses[[i]]$rating
              }
              if (is.null(ct$businesses[[i]]$price)) {
                businessPrice[[i]] <- "NA"
              }
              else {
                businessPrice[[i]] <- ct$businesses[[i]]$price
              }
              if (is.null(ct$businesses[[i]]$location$display_address)) {
                displayAddress[[i]] <- "NA"
                # print(i)
              }
              else {
                if (length(ct$businesses[[i]]$location$display_address) == 1) {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
                else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                  add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                  displayAddress[[i]] <- add
                }
                else {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
              }
            }
            
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 11) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon,
                popupOptions = popupOptions(
                  minWidth = 400, maxWidth = 400,
                  maxHeight = 400,
                  closeOnClick = TRUE
                )
              )
          }
          else if (nrow(dat) != 0 & length(resEventB$events) == 0 & length(ct$businesses) == 1) # google data has some observations > 0
            # else if (length(res$results$headcount) != 0 & nrow(resEventB$events) == 0)
          { # google data has some observations > 0
            if (is.null(document$results$rating) == TRUE) {
              rating <- NA
              dat <- cbind(dat, rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            else {
              dat <- cbind(dat, document$results$rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            if (is.null(document$results$price_level) == TRUE) {
              price_level <- NA
              dat <- cbind(dat, price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            else {
              dat <- cbind(dat, document$results$price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price")
              dat <- dat %>% dplyr::inner_join(googlePrices)
              dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            if (is.null(document$results$vicinity) == TRUE) {
              vicinity <- NA
              dat <- cbind(dat, vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            else {
              dat <- cbind(dat, document$results$vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            photo <- vector("character", length(document$results$name))
            if (length(document$results$photos) != 0) {
              for (i in 1:length(document$results$photos))
              {
                if (is.null(document$results$photos[i][[1]]) == FALSE) {
                  photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
                }
                else {
                  photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
                }
              }
              dat <- data.frame(dat, photo)
              leaflet("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                    paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
            else {
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = "")
                  ), "</h5>", "</br>",
                  icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
          }
          else if (nrow(dat) != 0 & length(resEventB$events) == 0 & length(ct$businesses) > 1) {
            latitudeYelp <- vector("character", length(ct$businesses))
            longitudeYelp <- vector("character", length(ct$businesses))
            businessName <- vector("character", length(ct$businesses))
            imageURL <- vector("character", length(ct$businesses))
            isClosed <- vector("character", length(ct$businesses))
            businessRating <- vector("character", length(ct$businesses))
            businessPrice <- vector("character", length(ct$businesses))
            displayAddress <- vector("character", length(ct$businesses))
            
            for (i in 1:length(ct$businesses))
            {
              if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
                latitudeYelp[[i]] <- "NA"
              }
              else {
                latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
              }
              if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
                longitudeYelp[[i]] <- "NA"
              }
              else {
                longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
              }
              if (is.null(ct$businesses[[i]]$name)) {
                businessName[[i]] <- "NA"
              }
              else {
                businessName[[i]] <- ct$businesses[[i]]$name
              }
              if (is.null(ct$businesses[[i]]$image_url)) {
                imageURL[[i]] <- "NA"
              }
              else {
                imageURL[[i]] <- ct$businesses[[i]]$image_url
              }
              if (is.null(ct$businesses[[i]]$is_closed)) {
                isClosed[[i]] <- "NA"
              }
              else {
                isClosed[[i]] <- ct$businesses[[i]]$is_closed
              }
              if (is.null(ct$businesses[[i]]$rating)) {
                businessRating[[i]] <- "NA"
              }
              else {
                businessRating[[i]] <- ct$businesses[[i]]$rating
              }
              if (is.null(ct$businesses[[i]]$price)) {
                businessPrice[[i]] <- "NA"
              }
              else {
                businessPrice[[i]] <- ct$businesses[[i]]$price
              }
              if (is.null(ct$businesses[[i]]$location$display_address)) {
                displayAddress[[i]] <- "NA"
                # print(i)
              }
              else {
                if (length(ct$businesses[[i]]$location$display_address) == 1) {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
                else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                  add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                  displayAddress[[i]] <- add
                }
                else {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
              }
            }
            
            if (is.null(document$results$rating) == TRUE) {
              rating <- NA
              dat <- cbind(dat, rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            else {
              dat <- cbind(dat, document$results$rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            if (is.null(document$results$price_level) == TRUE) {
              price_level <- NA
              dat <- cbind(dat, price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            else {
              dat <- cbind(dat, document$results$price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price")
              dat <- dat %>% dplyr::inner_join(googlePrices)
              dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            if (is.null(document$results$vicinity) == TRUE) {
              vicinity <- NA
              dat <- cbind(dat, vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            else {
              dat <- cbind(dat, document$results$vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            photo <- vector("character", length(document$results$name))
            if (length(document$results$photos) != 0) {
              for (i in 1:length(document$results$photos))
              {
                if (is.null(document$results$photos[i][[1]]) == FALSE) {
                  photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
                }
                else {
                  photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
                }
              }
              dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = "", "</h5>", "</br>"),
                    paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                    paste("Price: ", businessPrice, sep = ""), "<br>",
                    paste("Rating: ", businessRating, sep = ""), "<br>",
                    paste("Restaurant Closing Status: ", isClosed, sep = ""),
                    "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                    paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                    "<br>"
                  ),
                  icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
            else {
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                    paste("Price: ", businessPrice, sep = ""), "<br>",
                    paste("Rating: ", businessRating, sep = ""), "<br>",
                    paste("Restaurant Closing Status: ", isClosed, sep = ""),
                    "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                    paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                    "<br>"
                  ),
                  icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
          }
          else if (nrow(dat) == 0 & length(resEventB$events) != 0 & length(ct$businesses) == 1) # if no google content but eventbrite is present
          {
            startLocal <- strsplit(resEventB$events$start$local, "T")
            startLocalDate <- vector("character", length(startLocal))
            startLocalTime <- vector("character", length(startLocal))
            
            endLocal <- strsplit(resEventB$events$end$local, "T")
            endLocalDate <- vector("character", length(endLocal))
            endLocalTime <- vector("character", length(endLocal))
            for (i in 1:length(startLocal))
            {
              startLocalDate[[i]] <- startLocal[[i]][1]
              startLocalTime[[i]] <- startLocal[[i]][2]
            }
            for (i in 1:length(endLocal))
            {
              endLocalDate[[i]] <- endLocal[[i]][1]
              endLocalTime[[i]] <- endLocal[[i]][2]
            }
            startLocalDateFinal <- try(format(as.Date(startLocalDate), format = "%B %d %Y"))
            startLocalWeekday <- try(weekdays(as.Date(startLocalDate)))
            
            endLocalDateFinal <- try(format(as.Date(endLocalDate), format = "%B %d %Y"))
            endLocalWeekday <- try(weekdays(as.Date(endLocalDate)))
            
            startLocalTimeFinal <- try(format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p"))
            endLocalTimeFinal <- try(format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p"))
            
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"),
                  "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else if (nrow(dat) == 0 & length(resEventB$events) != 0 & length(ct$businesses) > 1)
            # else if (nrow(dat) != 0 & length(resEventB$events) != 0) #condition if google api and eventsbrite have content
          {
            latitudeYelp <- vector("character", length(ct$businesses))
            longitudeYelp <- vector("character", length(ct$businesses))
            businessName <- vector("character", length(ct$businesses))
            imageURL <- vector("character", length(ct$businesses))
            isClosed <- vector("character", length(ct$businesses))
            businessRating <- vector("character", length(ct$businesses))
            businessPrice <- vector("character", length(ct$businesses))
            displayAddress <- vector("character", length(ct$businesses))
            
            for (i in 1:length(ct$businesses))
            {
              if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
                latitudeYelp[[i]] <- "NA"
              }
              else {
                latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
              }
              if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
                longitudeYelp[[i]] <- "NA"
              }
              else {
                longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
              }
              if (is.null(ct$businesses[[i]]$name)) {
                businessName[[i]] <- "NA"
              }
              else {
                businessName[[i]] <- ct$businesses[[i]]$name
              }
              if (is.null(ct$businesses[[i]]$image_url)) {
                imageURL[[i]] <- "NA"
              }
              else {
                imageURL[[i]] <- ct$businesses[[i]]$image_url
              }
              if (is.null(ct$businesses[[i]]$is_closed)) {
                isClosed[[i]] <- "NA"
              }
              else {
                isClosed[[i]] <- ct$businesses[[i]]$is_closed
              }
              if (is.null(ct$businesses[[i]]$rating)) {
                businessRating[[i]] <- "NA"
              }
              else {
                businessRating[[i]] <- ct$businesses[[i]]$rating
              }
              if (is.null(ct$businesses[[i]]$price)) {
                businessPrice[[i]] <- "NA"
              }
              else {
                businessPrice[[i]] <- ct$businesses[[i]]$price
              }
              if (is.null(ct$businesses[[i]]$location$display_address)) {
                displayAddress[[i]] <- "NA"
                # print(i)
              }
              else {
                if (length(ct$businesses[[i]]$location$display_address) == 1) {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
                else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                  add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                  displayAddress[[i]] <- add
                }
                else {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
              }
            }
            
            startLocal <- strsplit(resEventB$events$start$local, "T")
            startLocalDate <- vector("character", length(startLocal))
            startLocalTime <- vector("character", length(startLocal))
            
            endLocal <- strsplit(resEventB$events$end$local, "T")
            endLocalDate <- vector("character", length(endLocal))
            endLocalTime <- vector("character", length(endLocal))
            for (i in 1:length(startLocal))
            {
              startLocalDate[[i]] <- startLocal[[i]][1]
              startLocalTime[[i]] <- startLocal[[i]][2]
            }
            for (i in 1:length(endLocal))
            {
              endLocalDate[[i]] <- endLocal[[i]][1]
              endLocalTime[[i]] <- endLocal[[i]][2]
            }
            startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
            startLocalWeekday <- weekdays(as.Date(startLocalDate))
            
            endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
            endLocalWeekday <- weekdays(as.Date(endLocalDate))
            
            startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"),
                  "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else if (nrow(dat) != 0 & length(resEventB$events) != 0 & length(ct$businesses) == 1) {
            startLocal <- strsplit(resEventB$events$start$local, "T")
            startLocalDate <- vector("character", length(startLocal))
            startLocalTime <- vector("character", length(startLocal))
            
            endLocal <- strsplit(resEventB$events$end$local, "T")
            endLocalDate <- vector("character", length(endLocal))
            endLocalTime <- vector("character", length(endLocal))
            for (i in 1:length(startLocal))
            {
              startLocalDate[[i]] <- startLocal[[i]][1]
              startLocalTime[[i]] <- startLocal[[i]][2]
            }
            for (i in 1:length(endLocal))
            {
              endLocalDate[[i]] <- endLocal[[i]][1]
              endLocalTime[[i]] <- endLocal[[i]][2]
            }
            startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
            startLocalWeekday <- weekdays(as.Date(startLocalDate))
            
            endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
            endLocalWeekday <- weekdays(as.Date(endLocalDate))
            
            startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            
            if (is.null(document$results$rating) == TRUE) {
              rating <- NA
              dat <- cbind(dat, rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            else {
              dat <- cbind(dat, document$results$rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            if (is.null(document$results$price_level) == TRUE) {
              price_level <- NA
              dat <- cbind(dat, price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            else {
              dat <- cbind(dat, document$results$price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price")
              dat <- dat %>% dplyr::inner_join(googlePrices)
              dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            if (is.null(document$results$vicinity) == TRUE) {
              vicinity <- NA
              dat <- cbind(dat, vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            else {
              dat <- cbind(dat, document$results$vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            
            photo <- vector("character", length(document$results$name))
            if (length(document$results$photos) != 0) {
              for (i in 1:length(document$results$photos))
              {
                if (is.null(document$results$photos[i][[1]]) == FALSE) {
                  photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
                }
                else {
                  photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
                }
              }
              
              dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                    paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                  lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                    paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                    paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                    "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", resEventB$events$logo$url, ">"), "<br>",
                    resEventB$events$description$text
                  ),
                  icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
            else {
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                  lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                    paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                    paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                    "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", resEventB$events$logo$url, ">"),
                    "<br>",
                    resEventB$events$description$text
                  ),
                  icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
          }
          else if (nrow(dat) != 0 & length(resEventB$events) != 0 & length(ct$businesses) > 1) {
            latitudeYelp <- vector("character", length(ct$businesses))
            longitudeYelp <- vector("character", length(ct$businesses))
            businessName <- vector("character", length(ct$businesses))
            imageURL <- vector("character", length(ct$businesses))
            isClosed <- vector("character", length(ct$businesses))
            businessRating <- vector("character", length(ct$businesses))
            businessPrice <- vector("character", length(ct$businesses))
            displayAddress <- vector("character", length(ct$businesses))
            
            for (i in 1:length(ct$businesses))
            {
              if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
                latitudeYelp[[i]] <- "NA"
              }
              else {
                latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
              }
              if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
                longitudeYelp[[i]] <- "NA"
              }
              else {
                longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
              }
              if (is.null(ct$businesses[[i]]$name)) {
                businessName[[i]] <- "NA"
              }
              else {
                businessName[[i]] <- ct$businesses[[i]]$name
              }
              if (is.null(ct$businesses[[i]]$image_url)) {
                imageURL[[i]] <- "NA"
              }
              else {
                imageURL[[i]] <- ct$businesses[[i]]$image_url
              }
              if (is.null(ct$businesses[[i]]$is_closed)) {
                isClosed[[i]] <- "NA"
              }
              else {
                isClosed[[i]] <- ct$businesses[[i]]$is_closed
              }
              if (is.null(ct$businesses[[i]]$rating)) {
                businessRating[[i]] <- "NA"
              }
              else {
                businessRating[[i]] <- ct$businesses[[i]]$rating
              }
              if (is.null(ct$businesses[[i]]$price)) {
                businessPrice[[i]] <- "NA"
              }
              else {
                businessPrice[[i]] <- ct$businesses[[i]]$price
              }
              if (is.null(ct$businesses[[i]]$location$display_address)) {
                displayAddress[[i]] <- "NA"
                # print(i)
              }
              else {
                if (length(ct$businesses[[i]]$location$display_address) == 1) {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
                else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                  add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                  displayAddress[[i]] <- add
                }
                else {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
              }
            }
            
            startLocal <- strsplit(resEventB$events$start$local, "T")
            startLocalDate <- vector("character", length(startLocal))
            startLocalTime <- vector("character", length(startLocal))
            
            endLocal <- strsplit(resEventB$events$end$local, "T")
            endLocalDate <- vector("character", length(endLocal))
            endLocalTime <- vector("character", length(endLocal))
            for (i in 1:length(startLocal))
            {
              startLocalDate[[i]] <- startLocal[[i]][1]
              startLocalTime[[i]] <- startLocal[[i]][2]
            }
            for (i in 1:length(endLocal))
            {
              endLocalDate[[i]] <- endLocal[[i]][1]
              endLocalTime[[i]] <- endLocal[[i]][2]
            }
            startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
            startLocalWeekday <- weekdays(as.Date(startLocalDate))
            
            endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
            endLocalWeekday <- weekdays(as.Date(endLocalDate))
            
            startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            
            if (is.null(document$results$rating) == TRUE) {
              rating <- NA
              dat <- cbind(dat, rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            else {
              dat <- cbind(dat, document$results$rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            if (is.null(document$results$price_level) == TRUE) {
              price_level <- NA
              dat <- cbind(dat, price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            else {
              dat <- cbind(dat, document$results$price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price")
              dat <- dat %>% dplyr::inner_join(googlePrices)
              dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            if (is.null(document$results$vicinity) == TRUE) {
              vicinity <- NA
              dat <- cbind(dat, vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            else {
              dat <- cbind(dat, document$results$vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            
            photo <- vector("character", length(document$results$name))
            if (length(document$results$photos) != 0) {
              for (i in 1:length(document$results$photos))
              {
                if (is.null(document$results$photos[i][[1]]) == FALSE) {
                  photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
                }
                else {
                  photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
                }
              }
              
              dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                    paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                  lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                    paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                    paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                    "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", resEventB$events$logo$url, ">"), "<br>",
                    resEventB$events$description$text
                  ),
                  icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                    paste("Price: ", businessPrice, sep = ""), "<br>",
                    paste("Rating: ", businessRating, sep = ""), "<br>",
                    paste("Restaurant Closing Status: ", isClosed, sep = ""),
                    "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                    paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                    "<br>"
                  ),
                  icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
            else {
              # dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                  lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                    paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                    paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                    "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", resEventB$events$logo$url, ">"),
                    "<br>",
                    resEventB$events$description$text
                  ),
                  icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                  ),
                  icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                    paste("Price: ", businessPrice, sep = ""), "<br>",
                    paste("Rating: ", businessRating, sep = ""), "<br>",
                    paste("Restaurant Closing Status: ", isClosed, sep = ""),
                    "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                    paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                    "<br>"
                  ),
                  icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
          }
        }
        else # if meetup api has events
        {
          photos <- vector("character", length(res$results$group$photos))
          for (i in 1:length(res$results$distance))
          {
            # photos
            photo <- data.frame(res$results$group$photos[[i]])
            if (is.null(photo[1, ]$photo_link)) {
              photos[[i]] <- as.character("NA")
            }
            else {
              photos[[i]] <- photo[1, ]$photo_link
            }
          }
          
          eventsDat <- data.frame(photos)
          
          # Calculating event start time
          eventsDat$timeUTC <- anytime(as.numeric(as.character(res$results$time * 0.001)))
          currTime <- Sys.time()
          currDate <- as.numeric(as.POSIXct(currTime))
          url2 <- paste("https://maps.googleapis.com/maps/api/timezone/json?location=", clat, ",", clng, "&timestamp=", currDate, "&key=KEY", sep = "")
          document2 <- fromJSON(txt = url2)
          eventsDat$timeZone <- format(eventsDat$timeUTC, tz = document2$timeZoneId, usetz = TRUE)
          if (is.null(eventsDat$timeZone) == FALSE) {
            timeOnlyStart <- format(ymd_hms(eventsDat$timeZone), "%H:%M:%S") # extract just the time
            eventsDat$timeStartFinal <- format(strptime(timeOnlyStart, format = "%H:%M:%S"), "%I:%M:%S %p")
            eventsDat$startDate <- as.Date(eventsDat$timeZone)
            eventsDat$startDateFinal <- format(eventsDat$startDate, format = "%B %d %Y")
            eventsDat$startWeekday <- weekdays(eventsDat$startDate)
          }
          else {
            timeOnlyStart <- "NA"
            eventsDat$timeStartFinal <- "NA"
            eventsDat$startDate <- "NA"
            eventsDat$startDateFinal <- "NA"
            eventsDat$startWeekday <- "NA"
          }
          # Calculating event end time
          timeEnd <- vector("raw", length(eventsDat$timeUTC))
          if (is.null(res$results$duration)) {
            eventsDat$timeEnd <- "NA"
            eventsDat$timeEndZone <- "NA"
            timeOnlyEnd <- "NA"
            eventsDat$timeEndFinal <- "NA"
            eventsDat$endDate <- "NA"
            eventsDat$endDateFinal <- "NA"
            eventsDat$endWeekday <- "NA"
          }
          else {
            eventsDat$timeEnd <- as.POSIXlt(eventsDat$timeUTC) + as.numeric(as.character(res$results$duration)) * 0.001
            eventsDat$timeEndZone <- format(eventsDat$timeEnd, tz = document2$timeZoneId, usetz = TRUE)
            timeOnlyEnd <- format(ymd_hms(eventsDat$timeEndZone), "%H:%M:%S") # extract just the time
            eventsDat$timeEndFinal <- format(strptime(timeOnlyEnd, format = "%H:%M:%S"), "%I:%M:%S %p")
            eventsDat$endDate <- as.Date(eventsDat$timeEndZone)
            eventsDat$endDateFinal <- format(eventsDat$endDate, format = "%B %d %Y")
            eventsDat$endWeekday <- weekdays(eventsDat$endDate)
          }
          if (nrow(dat) == 0 & nrow(resEventB$events) == 0 & length(ct$businesses) == 1) # if no values in google api and eventbrite api
          {
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else if (nrow(dat) == 0 & nrow(resEventB$events) == 0 & length(ct$businesses) > 1) # if no values in google api and eventbrite api
          {
            latitudeYelp <- vector("character", length(ct$businesses))
            longitudeYelp <- vector("character", length(ct$businesses))
            businessName <- vector("character", length(ct$businesses))
            imageURL <- vector("character", length(ct$businesses))
            isClosed <- vector("character", length(ct$businesses))
            businessRating <- vector("character", length(ct$businesses))
            businessPrice <- vector("character", length(ct$businesses))
            displayAddress <- vector("character", length(ct$businesses))
            
            for (i in 1:length(ct$businesses))
            {
              if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
                latitudeYelp[[i]] <- "NA"
              }
              else {
                latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
              }
              if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
                longitudeYelp[[i]] <- "NA"
              }
              else {
                longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
              }
              if (is.null(ct$businesses[[i]]$name)) {
                businessName[[i]] <- "NA"
              }
              else {
                businessName[[i]] <- ct$businesses[[i]]$name
              }
              if (is.null(ct$businesses[[i]]$image_url)) {
                imageURL[[i]] <- "NA"
              }
              else {
                imageURL[[i]] <- ct$businesses[[i]]$image_url
              }
              if (is.null(ct$businesses[[i]]$is_closed)) {
                isClosed[[i]] <- "NA"
              }
              else {
                isClosed[[i]] <- ct$businesses[[i]]$is_closed
              }
              if (is.null(ct$businesses[[i]]$rating)) {
                businessRating[[i]] <- "NA"
              }
              else {
                businessRating[[i]] <- ct$businesses[[i]]$rating
              }
              if (is.null(ct$businesses[[i]]$price)) {
                businessPrice[[i]] <- "NA"
              }
              else {
                businessPrice[[i]] <- ct$businesses[[i]]$price
              }
              if (is.null(ct$businesses[[i]]$location$display_address)) {
                displayAddress[[i]] <- "NA"
                # print(i)
              }
              else {
                if (length(ct$businesses[[i]]$location$display_address) == 1) {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
                else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                  add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                  displayAddress[[i]] <- add
                }
                else {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
              }
            }
            
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else if (nrow(dat) != 0 & nrow(resEventB$events) == 0 & length(ct$businesses) == 1) # if google api has values and eventbrite does not
          { # google data has some observations > 0
            if (is.null(document$results$rating) == TRUE) {
              rating <- NA
              dat <- cbind(dat, rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            else {
              dat <- cbind(dat, document$results$rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            if (is.null(document$results$price_level) == TRUE) {
              price_level <- NA
              dat <- cbind(dat, price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            else {
              dat <- cbind(dat, document$results$price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price")
              dat <- dat %>% dplyr::inner_join(googlePrices)
              dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            if (is.null(document$results$vicinity) == TRUE) {
              vicinity <- NA
              dat <- cbind(dat, vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            else {
              dat <- cbind(dat, document$results$vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            
            photo <- vector("character", length(document$results$name))
            if (length(document$results$photos) != 0) {
              for (i in 1:length(document$results$photos))
              {
                if (is.null(document$results$photos[i][[1]]) == FALSE) {
                  photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
                }
                else {
                  photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
                }
              }
              
              dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(res$results$group$group_lon)),
                  lat = as.numeric(as.character(res$results$group$group_lat)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                    paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                    paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                    "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                    "<br>",
                    "<h6>", res$results$group$name, "</h6>", "<br>",
                    res$results$description
                  ),
                  icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                  ),
                  paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">"), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
            else {
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(res$results$group$group_lon)),
                  lat = as.numeric(as.character(res$results$group$group_lat)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                    paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                    paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                    "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                    "<br>",
                    "<h6>", res$results$group$name, "</h6>", "<br>",
                    res$results$description
                  ),
                  icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 300, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
          }
          else if (nrow(dat) != 0 & nrow(resEventB$events) == 0 & length(ct$businesses) > 1) # if google api has values and eventbrite does not
          { # google data has some observations > 0
            latitudeYelp <- vector("character", length(ct$businesses))
            longitudeYelp <- vector("character", length(ct$businesses))
            businessName <- vector("character", length(ct$businesses))
            imageURL <- vector("character", length(ct$businesses))
            isClosed <- vector("character", length(ct$businesses))
            businessRating <- vector("character", length(ct$businesses))
            businessPrice <- vector("character", length(ct$businesses))
            displayAddress <- vector("character", length(ct$businesses))
            
            for (i in 1:length(ct$businesses))
            {
              if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
                latitudeYelp[[i]] <- "NA"
              }
              else {
                latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
              }
              if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
                longitudeYelp[[i]] <- "NA"
              }
              else {
                longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
              }
              if (is.null(ct$businesses[[i]]$name)) {
                businessName[[i]] <- "NA"
              }
              else {
                businessName[[i]] <- ct$businesses[[i]]$name
              }
              if (is.null(ct$businesses[[i]]$image_url)) {
                imageURL[[i]] <- "NA"
              }
              else {
                imageURL[[i]] <- ct$businesses[[i]]$image_url
              }
              if (is.null(ct$businesses[[i]]$is_closed)) {
                isClosed[[i]] <- "NA"
              }
              else {
                isClosed[[i]] <- ct$businesses[[i]]$is_closed
              }
              if (is.null(ct$businesses[[i]]$rating)) {
                businessRating[[i]] <- "NA"
              }
              else {
                businessRating[[i]] <- ct$businesses[[i]]$rating
              }
              if (is.null(ct$businesses[[i]]$price)) {
                businessPrice[[i]] <- "NA"
              }
              else {
                businessPrice[[i]] <- ct$businesses[[i]]$price
              }
              if (is.null(ct$businesses[[i]]$location$display_address)) {
                displayAddress[[i]] <- "NA"
                # print(i)
              }
              else {
                if (length(ct$businesses[[i]]$location$display_address) == 1) {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
                else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                  add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                  displayAddress[[i]] <- add
                }
                else {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
              }
            }
            
            if (is.null(document$results$rating) == TRUE) {
              rating <- NA
              dat <- cbind(dat, rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            else {
              dat <- cbind(dat, document$results$rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            if (is.null(document$results$price_level) == TRUE) {
              price_level <- NA
              dat <- cbind(dat, price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            else {
              dat <- cbind(dat, document$results$price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price")
              dat <- dat %>% dplyr::inner_join(googlePrices)
              dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            if (is.null(document$results$vicinity) == TRUE) {
              vicinity <- NA
              dat <- cbind(dat, vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            else {
              dat <- cbind(dat, document$results$vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            
            photo <- vector("character", length(document$results$name))
            if (length(document$results$photos) != 0) {
              for (i in 1:length(document$results$photos))
              {
                if (is.null(document$results$photos[i][[1]]) == FALSE) {
                  photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
                }
                else {
                  photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
                }
              }
              
              dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(res$results$group$group_lon)),
                  lat = as.numeric(as.character(res$results$group$group_lat)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                    paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                    paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                    "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                    "<br>",
                    "<h6>", res$results$group$name, "</h6>", "<br>",
                    res$results$description
                  ),
                  icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 300, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                    paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                    paste("Price: ", businessPrice, sep = ""), "<br>",
                    paste("Rating: ", businessRating, sep = ""), "<br>",
                    paste("Restaurant Closing Status: ", isClosed, sep = ""),
                    "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                    paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                    "<br>"
                  ),
                  icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
            else {
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(res$results$group$group_lon)),
                  lat = as.numeric(as.character(res$results$group$group_lat)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                    paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                    paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                    "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                    "<br>",
                    "<h6>", res$results$group$name, "</h6>", "<br>",
                    res$results$description
                  ),
                  icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                    paste("Price: ", businessPrice, sep = ""), "<br>",
                    paste("Rating: ", businessRating, sep = ""), "<br>",
                    paste("Restaurant Closing Status: ", isClosed, sep = ""),
                    "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                    paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                    "<br>"
                  ),
                  icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
          }
          else if (nrow(dat) == 0 & nrow(resEventB$events) != 0 & length(ct$businesses) == 1) {
            startLocal <- strsplit(resEventB$events$start$local, "T")
            startLocalDate <- vector("character", length(startLocal))
            startLocalTime <- vector("character", length(startLocal))
            
            endLocal <- strsplit(resEventB$events$end$local, "T")
            endLocalDate <- vector("character", length(endLocal))
            endLocalTime <- vector("character", length(endLocal))
            for (i in 1:length(startLocal))
            {
              startLocalDate[[i]] <- startLocal[[i]][1]
              startLocalTime[[i]] <- startLocal[[i]][2]
            }
            for (i in 1:length(endLocal))
            {
              endLocalDate[[i]] <- endLocal[[i]][1]
              endLocalTime[[i]] <- endLocal[[i]][2]
            }
            startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
            startLocalWeekday <- weekdays(as.Date(startLocalDate))
            
            endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
            endLocalWeekday <- weekdays(as.Date(endLocalDate))
            
            startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            
            
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"),
                  "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else if (nrow(dat) == 0 & nrow(resEventB$events) != 0 & length(ct$businesses) > 1) {
            latitudeYelp <- vector("character", length(ct$businesses))
            longitudeYelp <- vector("character", length(ct$businesses))
            businessName <- vector("character", length(ct$businesses))
            imageURL <- vector("character", length(ct$businesses))
            isClosed <- vector("character", length(ct$businesses))
            businessRating <- vector("character", length(ct$businesses))
            businessPrice <- vector("character", length(ct$businesses))
            displayAddress <- vector("character", length(ct$businesses))
            
            for (i in 1:length(ct$businesses))
            {
              if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
                latitudeYelp[[i]] <- "NA"
              }
              else {
                latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
              }
              if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
                longitudeYelp[[i]] <- "NA"
              }
              else {
                longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
              }
              if (is.null(ct$businesses[[i]]$name)) {
                businessName[[i]] <- "NA"
              }
              else {
                businessName[[i]] <- ct$businesses[[i]]$name
              }
              if (is.null(ct$businesses[[i]]$image_url)) {
                imageURL[[i]] <- "NA"
              }
              else {
                imageURL[[i]] <- ct$businesses[[i]]$image_url
              }
              if (is.null(ct$businesses[[i]]$is_closed)) {
                isClosed[[i]] <- "NA"
              }
              else {
                isClosed[[i]] <- ct$businesses[[i]]$is_closed
              }
              if (is.null(ct$businesses[[i]]$rating)) {
                businessRating[[i]] <- "NA"
              }
              else {
                businessRating[[i]] <- ct$businesses[[i]]$rating
              }
              if (is.null(ct$businesses[[i]]$price)) {
                businessPrice[[i]] <- "NA"
              }
              else {
                businessPrice[[i]] <- ct$businesses[[i]]$price
              }
              if (is.null(ct$businesses[[i]]$location$display_address)) {
                displayAddress[[i]] <- "NA"
                # print(i)
              }
              else {
                if (length(ct$businesses[[i]]$location$display_address) == 1) {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
                else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                  add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                  displayAddress[[i]] <- add
                }
                else {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
              }
            }
            
            startLocal <- strsplit(resEventB$events$start$local, "T")
            startLocalDate <- vector("character", length(startLocal))
            startLocalTime <- vector("character", length(startLocal))
            
            endLocal <- strsplit(resEventB$events$end$local, "T")
            endLocalDate <- vector("character", length(endLocal))
            endLocalTime <- vector("character", length(endLocal))
            for (i in 1:length(startLocal))
            {
              startLocalDate[[i]] <- startLocal[[i]][1]
              startLocalTime[[i]] <- startLocal[[i]][2]
            }
            for (i in 1:length(endLocal))
            {
              endLocalDate[[i]] <- endLocal[[i]][1]
              endLocalTime[[i]] <- endLocal[[i]][2]
            }
            startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
            startLocalWeekday <- weekdays(as.Date(startLocalDate))
            
            endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
            endLocalWeekday <- weekdays(as.Date(endLocalDate))
            
            startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            
            
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"),
                  "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else if (nrow(dat) != 0 & nrow(resEventB$events) != 0 & length(ct$businesses) == 1) {
            # else #if all google, eventbrite and meetup are present
            # {
            startLocal <- strsplit(resEventB$events$start$local, "T")
            startLocalDate <- vector("character", length(startLocal))
            startLocalTime <- vector("character", length(startLocal))
            
            endLocal <- strsplit(resEventB$events$end$local, "T")
            endLocalDate <- vector("character", length(endLocal))
            endLocalTime <- vector("character", length(endLocal))
            for (i in 1:length(startLocal))
            {
              startLocalDate[[i]] <- startLocal[[i]][1]
              startLocalTime[[i]] <- startLocal[[i]][2]
            }
            for (i in 1:length(endLocal))
            {
              endLocalDate[[i]] <- endLocal[[i]][1]
              endLocalTime[[i]] <- endLocal[[i]][2]
            }
            startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
            startLocalWeekday <- weekdays(as.Date(startLocalDate))
            
            endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
            endLocalWeekday <- weekdays(as.Date(endLocalDate))
            
            startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            
            if (is.null(document$results$rating) == TRUE) {
              rating <- NA
              dat <- cbind(dat, rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            else {
              dat <- cbind(dat, document$results$rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            if (is.null(document$results$price_level) == TRUE) {
              price_level <- NA
              dat <- cbind(dat, price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            else {
              dat <- cbind(dat, document$results$price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price")
              dat <- dat %>% dplyr::inner_join(googlePrices)
              dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            if (is.null(document$results$vicinity) == TRUE) {
              vicinity <- NA
              dat <- cbind(dat, vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            else {
              dat <- cbind(dat, document$results$vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            
            photo <- vector("character", length(document$results$name))
            if (length(document$results$photos) != 0) {
              for (i in 1:length(document$results$photos))
              {
                if (is.null(document$results$photos[i][[1]]) == FALSE) {
                  photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
                }
                else {
                  photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
                }
              }
              
              dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(res$results$group$group_lon)),
                  lat = as.numeric(as.character(res$results$group$group_lat)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                    paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                    paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                    "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                    "<br>",
                    "<h6>", res$results$group$name, "</h6>", "<br>",
                    res$results$description
                  ),
                  icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                    paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                  lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                    paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                    paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                    "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", resEventB$events$logo$url, ">"), "<br>",
                    resEventB$events$description$text
                  ),
                  icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
            else {
              
              # dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(res$results$group$group_lon)),
                  lat = as.numeric(as.character(res$results$group$group_lat)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                    paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                    paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                    "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                    "<br>",
                    "<h6>", res$results$group$name, "</h6>", "<br>",
                    res$results$description
                  ),
                  icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                  lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                    paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                    paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                    "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", resEventB$events$logo$url, ">"),
                    "<br>",
                    resEventB$events$description$text
                  ),
                  icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
          }
          else if (nrow(dat) != 0 & nrow(resEventB$events) != 0 & length(ct$businesses) > 1) {
            latitudeYelp <- vector("character", length(ct$businesses))
            longitudeYelp <- vector("character", length(ct$businesses))
            businessName <- vector("character", length(ct$businesses))
            imageURL <- vector("character", length(ct$businesses))
            isClosed <- vector("character", length(ct$businesses))
            businessRating <- vector("character", length(ct$businesses))
            businessPrice <- vector("character", length(ct$businesses))
            displayAddress <- vector("character", length(ct$businesses))
            
            for (i in 1:length(ct$businesses))
            {
              if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
                latitudeYelp[[i]] <- "NA"
              }
              else {
                latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
              }
              if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
                longitudeYelp[[i]] <- "NA"
              }
              else {
                longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
              }
              if (is.null(ct$businesses[[i]]$name)) {
                businessName[[i]] <- "NA"
              }
              else {
                businessName[[i]] <- ct$businesses[[i]]$name
              }
              if (is.null(ct$businesses[[i]]$image_url)) {
                imageURL[[i]] <- "NA"
              }
              else {
                imageURL[[i]] <- ct$businesses[[i]]$image_url
              }
              if (is.null(ct$businesses[[i]]$is_closed)) {
                isClosed[[i]] <- "NA"
              }
              else {
                isClosed[[i]] <- ct$businesses[[i]]$is_closed
              }
              if (is.null(ct$businesses[[i]]$rating)) {
                businessRating[[i]] <- "NA"
              }
              else {
                businessRating[[i]] <- ct$businesses[[i]]$rating
              }
              if (is.null(ct$businesses[[i]]$price)) {
                businessPrice[[i]] <- "NA"
              }
              else {
                businessPrice[[i]] <- ct$businesses[[i]]$price
              }
              if (is.null(ct$businesses[[i]]$location$display_address)) {
                displayAddress[[i]] <- "NA"
                # print(i)
              }
              else {
                if (length(ct$businesses[[i]]$location$display_address) == 1) {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
                else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                  add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                  displayAddress[[i]] <- add
                }
                else {
                  displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
                }
              }
            }
            
            startLocal <- strsplit(resEventB$events$start$local, "T")
            startLocalDate <- vector("character", length(startLocal))
            startLocalTime <- vector("character", length(startLocal))
            
            endLocal <- strsplit(resEventB$events$end$local, "T")
            endLocalDate <- vector("character", length(endLocal))
            endLocalTime <- vector("character", length(endLocal))
            for (i in 1:length(startLocal))
            {
              startLocalDate[[i]] <- startLocal[[i]][1]
              startLocalTime[[i]] <- startLocal[[i]][2]
            }
            for (i in 1:length(endLocal))
            {
              endLocalDate[[i]] <- endLocal[[i]][1]
              endLocalTime[[i]] <- endLocal[[i]][2]
            }
            startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
            startLocalWeekday <- weekdays(as.Date(startLocalDate))
            
            endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
            endLocalWeekday <- weekdays(as.Date(endLocalDate))
            
            startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
            
            if (is.null(document$results$rating) == TRUE) {
              rating <- NA
              dat <- cbind(dat, rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            else {
              dat <- cbind(dat, document$results$rating)
              colnames(dat) <- c("lng", "lat", "name", "rating")
            }
            if (is.null(document$results$price_level) == TRUE) {
              price_level <- NA
              dat <- cbind(dat, price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            else {
              dat <- cbind(dat, document$results$price_level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price")
              dat <- dat %>% dplyr::inner_join(googlePrices)
              dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
            }
            if (is.null(document$results$vicinity) == TRUE) {
              vicinity <- NA
              dat <- cbind(dat, vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            else {
              dat <- cbind(dat, document$results$vicinity)
              colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
            }
            
            photo <- vector("character", length(document$results$name))
            if (length(document$results$photos) != 0) {
              for (i in 1:length(document$results$photos))
              {
                if (is.null(document$results$photos[i][[1]]) == FALSE) {
                  photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
                }
                else {
                  photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
                }
              }
              
              dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(res$results$group$group_lon)),
                  lat = as.numeric(as.character(res$results$group$group_lat)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                    paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                    paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                    "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                    "<br>",
                    "<h6>", res$results$group$name, "</h6>", "<br>",
                    res$results$description
                  ),
                  icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                    paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                  lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                    paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                    paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                    "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", resEventB$events$logo$url, ">"), "<br>",
                    resEventB$events$description$text
                  ),
                  icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                    paste("Price: ", businessPrice, sep = ""), "<br>",
                    paste("Rating: ", businessRating, sep = ""), "<br>",
                    paste("Restaurant Closing Status: ", isClosed, sep = ""),
                    "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                    paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                    "<br>"
                  ),
                  icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400)
                )
            }
            else {
              
              # dat <- data.frame(dat, photo)
              leafletProxy("map") %>%
                clearMarkers() %>%
                clearPopups() %>%
                setView(lng = clng, lat = clat, zoom = 10) %>%
                addMarkers(
                  lng = as.numeric(as.character(res$results$group$group_lon)),
                  lat = as.numeric(as.character(res$results$group$group_lat)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                    paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                    paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                    "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                    "<br>",
                    "<h6>", res$results$group$name, "</h6>", "<br>",
                    res$results$description
                  ),
                  icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                  lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                    paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                    paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                    "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                    "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                    paste0("<img src = ", resEventB$events$logo$url, ">"),
                    "<br>",
                    resEventB$events$description$text
                  ),
                  icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = dat$lng, lat = dat$lat, group = "circles",
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                    paste("Address: ", dat$vicinity, sep = ""), "<br>",
                    paste("Price level: ", dat$price_level, sep = ""), "<br>",
                    paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                  ), icon = googleIcon,
                  popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                ) %>%
                addMarkers(
                  lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                  popup = paste(
                    "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                    paste("Price: ", businessPrice, sep = ""), "<br>",
                    paste("Rating: ", businessRating, sep = ""), "<br>",
                    paste("Restaurant Closing Status: ", isClosed, sep = ""),
                    "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                    paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                    "<br>"
                  ),
                  icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
                )
            }
          }
          else {
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          incProgress(0.3, detail = "Finishing...")
        }
      })
    }
  })
  
  
  observe({
    withProgress(message = "Application loading", value = 0, {
      meetupIcon <- makeIcon(
        iconUrl = "https://cdn-images-1.medium.com/max/1600/1*EK8I8k19h9V04NnyO59C_A.png",
        iconWidth = 45, iconHeight = 40,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      eventBrightIcon <- makeIcon(
        iconUrl = "https://cdn.evbstatic.com/s3-build/perm_001/e04503/django/images/favicons/favicon-194x194.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      googleIcon <- makeIcon(
        # iconUrl = "http://www.ditoweb.com/wp-content/uploads/2016/05/geolocation-api.png",
        iconUrl = "http://www.ceda.cz/files/logo/google/google_2016/icon_placesapi.png",
        iconWidth = 40, iconHeight = 40,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      price <- c(NA, 0, 1, 2, 3, 4)
      level <- c("NA", "Free", "Inexpensive", "Moderate", "Expensive", "Very Expensive")
      googlePrices <- data.frame(price, level)
      
      yelpIcon <- makeIcon(
        iconUrl = "https://networkprogramming.files.wordpress.com/2016/09/yelp.png",
        iconWidth = 40, iconHeight = 40,
        iconAnchorX = 0, iconAnchorY = 0
      )
      
      click <- mapClick2()
      
      clat <- click$lat
      clng <- click$lon
      
      
      distance <- input$miles * (1609.344)
      
      incProgress(0.3, detail = "Getting api data")
      
      url <- paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", clat, ",", clng, "&radius=", distance, "&types=", input$variable, "&key=KEY", sep = "")
      document <- fromJSON(txt = url)
      dat <- data.frame(
        document$results$geometry$location$lng, document$results$geometry$location$lat,
        document$results$name
      )
      
      eventBCategoryUrl <- "https://www.eventbriteapi.com/v3/categories/?&token=MHSCKC4OL6DZIQDXUP3W"
      eventCategories <- fromJSON(txt = eventBCategoryUrl)
      eventCategories2 <- data.frame(eventCategories$categories$id, eventCategories$categories$name)
      colnames(eventCategories2) <- c("id", "name")
      eventCategories2$name <- tolower(eventCategories2$name)
      
      radiusYelp <- input$miles / 1609.344
      url2 <- modify_url(
        "https://api.yelp.com", path = c("v3", "businesses", "search"),
        query = list(
          term = input$yelpSearch, latitude = clat, longitude = clng, radius_filter = radiusYelp,
          limit = 10
        )
      )
      res <- GET(url2, add_headers("Authorization" = paste("bearer", "API_KEY")))
      ct <- content(res)
      
      incProgress(0.4, detail = "Gathering data")
      
      if (input$meetupTopic == "") {
        eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&token=TOKEN&expand=venue", sep = "")
        json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
        resEventB <- fromJSON(txt = eventBUrl)
        res <- fromJSON(txt = json.url)
      }
      else {
        eventID <- eventCategories2[str_detect(eventCategories2$name, input$meetupTopic), ]
        eventid <- eventID$id
        json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
        errorRes <- try(fromJSON(txt = json.url), TRUE)
        if (length(eventid) == 0 & length(grep("open.connection", errorRes)) == 1) # if error in both meetup and eventbrite apis
        {
          eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&token=TOKEN&expand=venue", sep = "")
          json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&fields=group_photos;key=KEY", sep = "")
          resEventB <- fromJSON(txt = eventBUrl)
          res <- fromJSON(txt = json.url)
        }
        else if (length(eventid) == 0 & length(grep("open.connection", errorRes)) == 0) # if error in eventbrite but no error in meetup
        {
          eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&token=TOKEN&expand=venue", sep = "")
          json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
          resEventB <- fromJSON(txt = eventBUrl)
          res <- fromJSON(txt = json.url)
        }
        else if (length(eventid) != 0 & length(grep("open.connection", errorRes)) == 1) # if no error in eventbrite but error in meetup
        {
          eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&categories=", eventid, "&token=TOKEN&expand=venue", sep = "")
          json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
          resEventB <- fromJSON(txt = eventBUrl)
          res <- fromJSON(txt = json.url)
        }
        else # no error in eventbrite and no error in meetup
        {
          eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi&", "start_date.keyword=", input$eventBriteTime, "&categories=", eventid, "&token=TOKEN&expand=venue", sep = "")
          json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&time=", input$meetupTime[1], "w", ",", input$meetupTime[2], "w", "&fields=group_photos;key=KEY", sep = "")
          resEventB <- fromJSON(txt = eventBUrl)
          res <- fromJSON(txt = json.url)
        }
      }
      
      if (length(res$results) == 0) # if no meetup observations
      {
        if (nrow(dat) == 0 & length(resEventB$events) == 0 & length(ct$businesses) == 1) {
          content <- paste("No places/events/food at this location", "\U0001f622")
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearPopups() %>%
            setView(lng = clng, lat = clat, zoom = 10) %>%
            addPopups(
              clng, clat, "<br>", content, "</br>", options =
                popupOptions(closeButton = FALSE, closeOnClick = TRUE)
            )
        }
        else if (nrow(dat) == 0 & length(resEventB$events) == 0 & length(ct$businesses) > 1) {
          latitudeYelp <- vector("character", length(ct$businesses))
          longitudeYelp <- vector("character", length(ct$businesses))
          businessName <- vector("character", length(ct$businesses))
          imageURL <- vector("character", length(ct$businesses))
          isClosed <- vector("character", length(ct$businesses))
          businessRating <- vector("character", length(ct$businesses))
          businessPrice <- vector("character", length(ct$businesses))
          displayAddress <- vector("character", length(ct$businesses))
          
          for (i in 1:length(ct$businesses))
          {
            if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
              latitudeYelp[[i]] <- "NA"
            }
            else {
              latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
            }
            if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
              longitudeYelp[[i]] <- "NA"
            }
            else {
              longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
            }
            if (is.null(ct$businesses[[i]]$name)) {
              businessName[[i]] <- "NA"
            }
            else {
              businessName[[i]] <- ct$businesses[[i]]$name
            }
            if (is.null(ct$businesses[[i]]$image_url)) {
              imageURL[[i]] <- "NA"
            }
            else {
              imageURL[[i]] <- ct$businesses[[i]]$image_url
            }
            if (is.null(ct$businesses[[i]]$is_closed)) {
              isClosed[[i]] <- "NA"
            }
            else {
              isClosed[[i]] <- ct$businesses[[i]]$is_closed
            }
            if (is.null(ct$businesses[[i]]$rating)) {
              businessRating[[i]] <- "NA"
            }
            else {
              businessRating[[i]] <- ct$businesses[[i]]$rating
            }
            if (is.null(ct$businesses[[i]]$price)) {
              businessPrice[[i]] <- "NA"
            }
            else {
              businessPrice[[i]] <- ct$businesses[[i]]$price
            }
            if (is.null(ct$businesses[[i]]$location$display_address)) {
              displayAddress[[i]] <- "NA"
              # print(i)
            }
            else {
              if (length(ct$businesses[[i]]$location$display_address) == 1) {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
              else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                displayAddress[[i]] <- add
              }
              else {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
            }
          }
          
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearPopups() %>%
            setView(lng = clng, lat = clat, zoom = 11) %>%
            addMarkers(
              lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                paste("Price: ", businessPrice, sep = ""), "<br>",
                paste("Rating: ", businessRating, sep = ""), "<br>",
                paste("Restaurant Closing Status: ", isClosed, sep = ""),
                "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                "<br>"
              ),
              icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            )
        }
        else if (nrow(dat) != 0 & length(resEventB$events) == 0 & length(ct$businesses) == 1) # google data has some observations > 0
          # else if (length(res$results$headcount) != 0 & nrow(resEventB$events) == 0)
        { # google data has some observations > 0
          if (is.null(document$results$rating) == TRUE) {
            rating <- NA
            dat <- cbind(dat, rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          else {
            dat <- cbind(dat, document$results$rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          if (is.null(document$results$price_level) == TRUE) {
            price_level <- NA
            dat <- cbind(dat, price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          else {
            dat <- cbind(dat, document$results$price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price")
            dat <- dat %>% dplyr::inner_join(googlePrices)
            dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          if (is.null(document$results$vicinity) == TRUE) {
            vicinity <- NA
            dat <- cbind(dat, vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          else {
            dat <- cbind(dat, document$results$vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          photo <- vector("character", length(document$results$name))
          if (length(document$results$photos) != 0) {
            for (i in 1:length(document$results$photos))
            {
              if (is.null(document$results$photos[i][[1]]) == FALSE) {
                photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
              }
              else {
                photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
              }
            }
            dat <- data.frame(dat, photo)
            leaflet("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                  paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else {
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = "")
                ), "</h5>", "</br>",
                icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
        }
        else if (nrow(dat) != 0 & length(resEventB$events) == 0 & length(ct$businesses) > 1) {
          latitudeYelp <- vector("character", length(ct$businesses))
          longitudeYelp <- vector("character", length(ct$businesses))
          businessName <- vector("character", length(ct$businesses))
          imageURL <- vector("character", length(ct$businesses))
          isClosed <- vector("character", length(ct$businesses))
          businessRating <- vector("character", length(ct$businesses))
          businessPrice <- vector("character", length(ct$businesses))
          displayAddress <- vector("character", length(ct$businesses))
          
          for (i in 1:length(ct$businesses))
          {
            if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
              latitudeYelp[[i]] <- "NA"
            }
            else {
              latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
            }
            if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
              longitudeYelp[[i]] <- "NA"
            }
            else {
              longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
            }
            if (is.null(ct$businesses[[i]]$name)) {
              businessName[[i]] <- "NA"
            }
            else {
              businessName[[i]] <- ct$businesses[[i]]$name
            }
            if (is.null(ct$businesses[[i]]$image_url)) {
              imageURL[[i]] <- "NA"
            }
            else {
              imageURL[[i]] <- ct$businesses[[i]]$image_url
            }
            if (is.null(ct$businesses[[i]]$is_closed)) {
              isClosed[[i]] <- "NA"
            }
            else {
              isClosed[[i]] <- ct$businesses[[i]]$is_closed
            }
            if (is.null(ct$businesses[[i]]$rating)) {
              businessRating[[i]] <- "NA"
            }
            else {
              businessRating[[i]] <- ct$businesses[[i]]$rating
            }
            if (is.null(ct$businesses[[i]]$price)) {
              businessPrice[[i]] <- "NA"
            }
            else {
              businessPrice[[i]] <- ct$businesses[[i]]$price
            }
            if (is.null(ct$businesses[[i]]$location$display_address)) {
              displayAddress[[i]] <- "NA"
              # print(i)
            }
            else {
              if (length(ct$businesses[[i]]$location$display_address) == 1) {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
              else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                displayAddress[[i]] <- add
              }
              else {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
            }
          }
          
          if (is.null(document$results$rating) == TRUE) {
            rating <- NA
            dat <- cbind(dat, rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          else {
            dat <- cbind(dat, document$results$rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          if (is.null(document$results$price_level) == TRUE) {
            price_level <- NA
            dat <- cbind(dat, price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          else {
            dat <- cbind(dat, document$results$price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price")
            dat <- dat %>% dplyr::inner_join(googlePrices)
            dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          if (is.null(document$results$vicinity) == TRUE) {
            vicinity <- NA
            dat <- cbind(dat, vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          else {
            dat <- cbind(dat, document$results$vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          photo <- vector("character", length(document$results$name))
          if (length(document$results$photos) != 0) {
            for (i in 1:length(document$results$photos))
            {
              if (is.null(document$results$photos[i][[1]]) == FALSE) {
                photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
              }
              else {
                photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
              }
            }
            dat <- data.frame(dat, photo)
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = "", "</h5>", "</br>"),
                  paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else {
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
        }
        else if (nrow(dat) == 0 & length(resEventB$events) != 0 & length(ct$businesses) == 1) # if no google content but eventbrite is present
        {
          startLocal <- strsplit(resEventB$events$start$local, "T")
          startLocalDate <- vector("character", length(startLocal))
          startLocalTime <- vector("character", length(startLocal))
          
          endLocal <- strsplit(resEventB$events$end$local, "T")
          endLocalDate <- vector("character", length(endLocal))
          endLocalTime <- vector("character", length(endLocal))
          for (i in 1:length(startLocal))
          {
            startLocalDate[[i]] <- startLocal[[i]][1]
            startLocalTime[[i]] <- startLocal[[i]][2]
          }
          for (i in 1:length(endLocal))
          {
            endLocalDate[[i]] <- endLocal[[i]][1]
            endLocalTime[[i]] <- endLocal[[i]][2]
          }
          startLocalDateFinal <- try(format(as.Date(startLocalDate), format = "%B %d %Y"))
          startLocalWeekday <- try(weekdays(as.Date(startLocalDate)))
          
          endLocalDateFinal <- try(format(as.Date(endLocalDate), format = "%B %d %Y"))
          endLocalWeekday <- try(weekdays(as.Date(endLocalDate)))
          
          startLocalTimeFinal <- try(format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p"))
          endLocalTimeFinal <- try(format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p"))
          
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearPopups() %>%
            setView(lng = clng, lat = clat, zoom = 10) %>%
            addMarkers(
              lng = as.numeric(as.character(resEventB$events$venue$longitude)),
              lat = as.numeric(as.character(resEventB$events$venue$latitude)),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                paste0("<img src = ", resEventB$events$logo$url, ">"),
                "<br>",
                resEventB$events$description$text
              ),
              icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            )
        }
        else if (nrow(dat) == 0 & length(resEventB$events) != 0 & length(ct$businesses) > 1)
          # else if (nrow(dat) != 0 & length(resEventB$events) != 0) #condition if google api and eventsbrite have content
        {
          latitudeYelp <- vector("character", length(ct$businesses))
          longitudeYelp <- vector("character", length(ct$businesses))
          businessName <- vector("character", length(ct$businesses))
          imageURL <- vector("character", length(ct$businesses))
          isClosed <- vector("character", length(ct$businesses))
          businessRating <- vector("character", length(ct$businesses))
          businessPrice <- vector("character", length(ct$businesses))
          displayAddress <- vector("character", length(ct$businesses))
          
          for (i in 1:length(ct$businesses))
          {
            if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
              latitudeYelp[[i]] <- "NA"
            }
            else {
              latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
            }
            if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
              longitudeYelp[[i]] <- "NA"
            }
            else {
              longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
            }
            if (is.null(ct$businesses[[i]]$name)) {
              businessName[[i]] <- "NA"
            }
            else {
              businessName[[i]] <- ct$businesses[[i]]$name
            }
            if (is.null(ct$businesses[[i]]$image_url)) {
              imageURL[[i]] <- "NA"
            }
            else {
              imageURL[[i]] <- ct$businesses[[i]]$image_url
            }
            if (is.null(ct$businesses[[i]]$is_closed)) {
              isClosed[[i]] <- "NA"
            }
            else {
              isClosed[[i]] <- ct$businesses[[i]]$is_closed
            }
            if (is.null(ct$businesses[[i]]$rating)) {
              businessRating[[i]] <- "NA"
            }
            else {
              businessRating[[i]] <- ct$businesses[[i]]$rating
            }
            if (is.null(ct$businesses[[i]]$price)) {
              businessPrice[[i]] <- "NA"
            }
            else {
              businessPrice[[i]] <- ct$businesses[[i]]$price
            }
            if (is.null(ct$businesses[[i]]$location$display_address)) {
              displayAddress[[i]] <- "NA"
              # print(i)
            }
            else {
              if (length(ct$businesses[[i]]$location$display_address) == 1) {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
              else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                displayAddress[[i]] <- add
              }
              else {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
            }
          }
          
          startLocal <- strsplit(resEventB$events$start$local, "T")
          startLocalDate <- vector("character", length(startLocal))
          startLocalTime <- vector("character", length(startLocal))
          
          endLocal <- strsplit(resEventB$events$end$local, "T")
          endLocalDate <- vector("character", length(endLocal))
          endLocalTime <- vector("character", length(endLocal))
          for (i in 1:length(startLocal))
          {
            startLocalDate[[i]] <- startLocal[[i]][1]
            startLocalTime[[i]] <- startLocal[[i]][2]
          }
          for (i in 1:length(endLocal))
          {
            endLocalDate[[i]] <- endLocal[[i]][1]
            endLocalTime[[i]] <- endLocal[[i]][2]
          }
          startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
          startLocalWeekday <- weekdays(as.Date(startLocalDate))
          
          endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
          endLocalWeekday <- weekdays(as.Date(endLocalDate))
          
          startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearPopups() %>%
            setView(lng = clng, lat = clat, zoom = 10) %>%
            addMarkers(
              lng = as.numeric(as.character(resEventB$events$venue$longitude)),
              lat = as.numeric(as.character(resEventB$events$venue$latitude)),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                paste0("<img src = ", resEventB$events$logo$url, ">"),
                "<br>",
                resEventB$events$description$text
              ),
              icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            ) %>%
            addMarkers(
              lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                paste("Price: ", businessPrice, sep = ""), "<br>",
                paste("Rating: ", businessRating, sep = ""), "<br>",
                paste("Restaurant Closing Status: ", isClosed, sep = ""),
                "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                "<br>"
              ),
              icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            )
        }
        else if (nrow(dat) != 0 & length(resEventB$events) != 0 & length(ct$businesses) == 1) {
          startLocal <- strsplit(resEventB$events$start$local, "T")
          startLocalDate <- vector("character", length(startLocal))
          startLocalTime <- vector("character", length(startLocal))
          
          endLocal <- strsplit(resEventB$events$end$local, "T")
          endLocalDate <- vector("character", length(endLocal))
          endLocalTime <- vector("character", length(endLocal))
          for (i in 1:length(startLocal))
          {
            startLocalDate[[i]] <- startLocal[[i]][1]
            startLocalTime[[i]] <- startLocal[[i]][2]
          }
          for (i in 1:length(endLocal))
          {
            endLocalDate[[i]] <- endLocal[[i]][1]
            endLocalTime[[i]] <- endLocal[[i]][2]
          }
          startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
          startLocalWeekday <- weekdays(as.Date(startLocalDate))
          
          endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
          endLocalWeekday <- weekdays(as.Date(endLocalDate))
          
          startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          
          if (is.null(document$results$rating) == TRUE) {
            rating <- NA
            dat <- cbind(dat, rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          else {
            dat <- cbind(dat, document$results$rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          if (is.null(document$results$price_level) == TRUE) {
            price_level <- NA
            dat <- cbind(dat, price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          else {
            dat <- cbind(dat, document$results$price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price")
            dat <- dat %>% dplyr::inner_join(googlePrices)
            dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          if (is.null(document$results$vicinity) == TRUE) {
            vicinity <- NA
            dat <- cbind(dat, vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          else {
            dat <- cbind(dat, document$results$vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          
          photo <- vector("character", length(document$results$name))
          if (length(document$results$photos) != 0) {
            for (i in 1:length(document$results$photos))
            {
              if (is.null(document$results$photos[i][[1]]) == FALSE) {
                photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
              }
              else {
                photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
              }
            }
            
            dat <- data.frame(dat, photo)
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                  paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"), "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else {
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"),
                  "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
        }
        else if (nrow(dat) != 0 & length(resEventB$events) != 0 & length(ct$businesses) > 1) {
          latitudeYelp <- vector("character", length(ct$businesses))
          longitudeYelp <- vector("character", length(ct$businesses))
          businessName <- vector("character", length(ct$businesses))
          imageURL <- vector("character", length(ct$businesses))
          isClosed <- vector("character", length(ct$businesses))
          businessRating <- vector("character", length(ct$businesses))
          businessPrice <- vector("character", length(ct$businesses))
          displayAddress <- vector("character", length(ct$businesses))
          
          for (i in 1:length(ct$businesses))
          {
            if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
              latitudeYelp[[i]] <- "NA"
            }
            else {
              latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
            }
            if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
              longitudeYelp[[i]] <- "NA"
            }
            else {
              longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
            }
            if (is.null(ct$businesses[[i]]$name)) {
              businessName[[i]] <- "NA"
            }
            else {
              businessName[[i]] <- ct$businesses[[i]]$name
            }
            if (is.null(ct$businesses[[i]]$image_url)) {
              imageURL[[i]] <- "NA"
            }
            else {
              imageURL[[i]] <- ct$businesses[[i]]$image_url
            }
            if (is.null(ct$businesses[[i]]$is_closed)) {
              isClosed[[i]] <- "NA"
            }
            else {
              isClosed[[i]] <- ct$businesses[[i]]$is_closed
            }
            if (is.null(ct$businesses[[i]]$rating)) {
              businessRating[[i]] <- "NA"
            }
            else {
              businessRating[[i]] <- ct$businesses[[i]]$rating
            }
            if (is.null(ct$businesses[[i]]$price)) {
              businessPrice[[i]] <- "NA"
            }
            else {
              businessPrice[[i]] <- ct$businesses[[i]]$price
            }
            if (is.null(ct$businesses[[i]]$location$display_address)) {
              displayAddress[[i]] <- "NA"
              # print(i)
            }
            else {
              if (length(ct$businesses[[i]]$location$display_address) == 1) {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
              else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                displayAddress[[i]] <- add
              }
              else {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
            }
          }
          
          startLocal <- strsplit(resEventB$events$start$local, "T")
          startLocalDate <- vector("character", length(startLocal))
          startLocalTime <- vector("character", length(startLocal))
          
          endLocal <- strsplit(resEventB$events$end$local, "T")
          endLocalDate <- vector("character", length(endLocal))
          endLocalTime <- vector("character", length(endLocal))
          for (i in 1:length(startLocal))
          {
            startLocalDate[[i]] <- startLocal[[i]][1]
            startLocalTime[[i]] <- startLocal[[i]][2]
          }
          for (i in 1:length(endLocal))
          {
            endLocalDate[[i]] <- endLocal[[i]][1]
            endLocalTime[[i]] <- endLocal[[i]][2]
          }
          startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
          startLocalWeekday <- weekdays(as.Date(startLocalDate))
          
          endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
          endLocalWeekday <- weekdays(as.Date(endLocalDate))
          
          startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          
          if (is.null(document$results$rating) == TRUE) {
            rating <- NA
            dat <- cbind(dat, rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          else {
            dat <- cbind(dat, document$results$rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          if (is.null(document$results$price_level) == TRUE) {
            price_level <- NA
            dat <- cbind(dat, price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          else {
            dat <- cbind(dat, document$results$price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price")
            dat <- dat %>% dplyr::inner_join(googlePrices)
            dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          if (is.null(document$results$vicinity) == TRUE) {
            vicinity <- NA
            dat <- cbind(dat, vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          else {
            dat <- cbind(dat, document$results$vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          
          photo <- vector("character", length(document$results$name))
          if (length(document$results$photos) != 0) {
            for (i in 1:length(document$results$photos))
            {
              if (is.null(document$results$photos[i][[1]]) == FALSE) {
                photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
              }
              else {
                photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
              }
            }
            
            dat <- data.frame(dat, photo)
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                  paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"), "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else {
            # dat <- data.frame(dat, photo)
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"),
                  "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                ),
                icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
        }
      }
      else # if meetup api has events
      {
        photos <- vector("character", length(res$results$group$photos))
        for (i in 1:length(res$results$distance))
        {
          # photos
          photo <- data.frame(res$results$group$photos[[i]])
          if (is.null(photo[1, ]$photo_link)) {
            photos[[i]] <- as.character("NA")
          }
          else {
            photos[[i]] <- photo[1, ]$photo_link
          }
        }
        
        eventsDat <- data.frame(photos)
        
        # Calculating event start time
        eventsDat$timeUTC <- anytime(as.numeric(as.character(res$results$time * 0.001)))
        currTime <- Sys.time()
        currDate <- as.numeric(as.POSIXct(currTime))
        url2 <- paste("https://maps.googleapis.com/maps/api/timezone/json?location=", clat, ",", clng, "&timestamp=", currDate, "&key=KEY", sep = "")
        document2 <- fromJSON(txt = url2)
        eventsDat$timeZone <- format(eventsDat$timeUTC, tz = document2$timeZoneId, usetz = TRUE)
        if (is.null(eventsDat$timeZone) == FALSE) {
          timeOnlyStart <- format(ymd_hms(eventsDat$timeZone), "%H:%M:%S") # extract just the time
          eventsDat$timeStartFinal <- format(strptime(timeOnlyStart, format = "%H:%M:%S"), "%I:%M:%S %p")
          eventsDat$startDate <- as.Date(eventsDat$timeZone)
          eventsDat$startDateFinal <- format(eventsDat$startDate, format = "%B %d %Y")
          eventsDat$startWeekday <- weekdays(eventsDat$startDate)
        }
        else {
          timeOnlyStart <- "NA"
          eventsDat$timeStartFinal <- "NA"
          eventsDat$startDate <- "NA"
          eventsDat$startDateFinal <- "NA"
          eventsDat$startWeekday <- "NA"
        }
        # Calculating event end time
        timeEnd <- vector("raw", length(eventsDat$timeUTC))
        if (is.null(res$results$duration)) {
          eventsDat$timeEnd <- "NA"
          eventsDat$timeEndZone <- "NA"
          timeOnlyEnd <- "NA"
          eventsDat$timeEndFinal <- "NA"
          eventsDat$endDate <- "NA"
          eventsDat$endDateFinal <- "NA"
          eventsDat$endWeekday <- "NA"
        }
        else {
          eventsDat$timeEnd <- as.POSIXlt(eventsDat$timeUTC) + as.numeric(as.character(res$results$duration)) * 0.001
          eventsDat$timeEndZone <- format(eventsDat$timeEnd, tz = document2$timeZoneId, usetz = TRUE)
          timeOnlyEnd <- format(ymd_hms(eventsDat$timeEndZone), "%H:%M:%S") # extract just the time
          eventsDat$timeEndFinal <- format(strptime(timeOnlyEnd, format = "%H:%M:%S"), "%I:%M:%S %p")
          eventsDat$endDate <- as.Date(eventsDat$timeEndZone)
          eventsDat$endDateFinal <- format(eventsDat$endDate, format = "%B %d %Y")
          eventsDat$endWeekday <- weekdays(eventsDat$endDate)
        }
        if (nrow(dat) == 0 & nrow(resEventB$events) == 0 & length(ct$businesses) == 1) # if no values in google api and eventbrite api
        {
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearPopups() %>%
            setView(lng = clng, lat = clat, zoom = 10) %>%
            addMarkers(
              lng = as.numeric(as.character(res$results$group$group_lon)),
              lat = as.numeric(as.character(res$results$group$group_lat)),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                "<br>",
                "<h6>", res$results$group$name, "</h6>", "<br>",
                res$results$description
              ),
              
              icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            )
        }
        else if (nrow(dat) == 0 & nrow(resEventB$events) == 0 & length(ct$businesses) > 1) # if no values in google api and eventbrite api
        {
          latitudeYelp <- vector("character", length(ct$businesses))
          longitudeYelp <- vector("character", length(ct$businesses))
          businessName <- vector("character", length(ct$businesses))
          imageURL <- vector("character", length(ct$businesses))
          isClosed <- vector("character", length(ct$businesses))
          businessRating <- vector("character", length(ct$businesses))
          businessPrice <- vector("character", length(ct$businesses))
          displayAddress <- vector("character", length(ct$businesses))
          
          for (i in 1:length(ct$businesses))
          {
            if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
              latitudeYelp[[i]] <- "NA"
            }
            else {
              latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
            }
            if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
              longitudeYelp[[i]] <- "NA"
            }
            else {
              longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
            }
            if (is.null(ct$businesses[[i]]$name)) {
              businessName[[i]] <- "NA"
            }
            else {
              businessName[[i]] <- ct$businesses[[i]]$name
            }
            if (is.null(ct$businesses[[i]]$image_url)) {
              imageURL[[i]] <- "NA"
            }
            else {
              imageURL[[i]] <- ct$businesses[[i]]$image_url
            }
            if (is.null(ct$businesses[[i]]$is_closed)) {
              isClosed[[i]] <- "NA"
            }
            else {
              isClosed[[i]] <- ct$businesses[[i]]$is_closed
            }
            if (is.null(ct$businesses[[i]]$rating)) {
              businessRating[[i]] <- "NA"
            }
            else {
              businessRating[[i]] <- ct$businesses[[i]]$rating
            }
            if (is.null(ct$businesses[[i]]$price)) {
              businessPrice[[i]] <- "NA"
            }
            else {
              businessPrice[[i]] <- ct$businesses[[i]]$price
            }
            if (is.null(ct$businesses[[i]]$location$display_address)) {
              displayAddress[[i]] <- "NA"
              # print(i)
            }
            else {
              if (length(ct$businesses[[i]]$location$display_address) == 1) {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
              else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                displayAddress[[i]] <- add
              }
              else {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
            }
          }
          
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearPopups() %>%
            setView(lng = clng, lat = clat, zoom = 10) %>%
            addMarkers(
              lng = as.numeric(as.character(res$results$group$group_lon)),
              lat = as.numeric(as.character(res$results$group$group_lat)),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                "<br>",
                "<h6>", res$results$group$name, "</h6>", "<br>",
                res$results$description
              ),
              icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            ) %>%
            addMarkers(
              lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                paste("Price: ", businessPrice, sep = ""), "<br>",
                paste("Rating: ", businessRating, sep = ""), "<br>",
                paste("Restaurant Closing Status: ", isClosed, sep = ""),
                "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                "<br>"
              ),
              icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            )
        }
        else if (nrow(dat) != 0 & nrow(resEventB$events) == 0 & length(ct$businesses) == 1) # if google api has values and eventbrite does not
        { # google data has some observations > 0
          if (is.null(document$results$rating) == TRUE) {
            rating <- NA
            dat <- cbind(dat, rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          else {
            dat <- cbind(dat, document$results$rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          if (is.null(document$results$price_level) == TRUE) {
            price_level <- NA
            dat <- cbind(dat, price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          else {
            dat <- cbind(dat, document$results$price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price")
            dat <- dat %>% dplyr::inner_join(googlePrices)
            dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          if (is.null(document$results$vicinity) == TRUE) {
            vicinity <- NA
            dat <- cbind(dat, vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          else {
            dat <- cbind(dat, document$results$vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          
          photo <- vector("character", length(document$results$name))
          if (length(document$results$photos) != 0) {
            for (i in 1:length(document$results$photos))
            {
              if (is.null(document$results$photos[i][[1]]) == FALSE) {
                photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
              }
              else {
                photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
              }
            }
            
            dat <- data.frame(dat, photo)
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                ),
                paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">"), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400), closeOnClick = TRUE
              )
          }
          else {
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 300, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
        }
        else if (nrow(dat) != 0 & nrow(resEventB$events) == 0 & length(ct$businesses) > 1) # if google api has values and eventbrite does not
        { # google data has some observations > 0
          latitudeYelp <- vector("character", length(ct$businesses))
          longitudeYelp <- vector("character", length(ct$businesses))
          businessName <- vector("character", length(ct$businesses))
          imageURL <- vector("character", length(ct$businesses))
          isClosed <- vector("character", length(ct$businesses))
          businessRating <- vector("character", length(ct$businesses))
          businessPrice <- vector("character", length(ct$businesses))
          displayAddress <- vector("character", length(ct$businesses))
          
          for (i in 1:length(ct$businesses))
          {
            if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
              latitudeYelp[[i]] <- "NA"
            }
            else {
              latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
            }
            if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
              longitudeYelp[[i]] <- "NA"
            }
            else {
              longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
            }
            if (is.null(ct$businesses[[i]]$name)) {
              businessName[[i]] <- "NA"
            }
            else {
              businessName[[i]] <- ct$businesses[[i]]$name
            }
            if (is.null(ct$businesses[[i]]$image_url)) {
              imageURL[[i]] <- "NA"
            }
            else {
              imageURL[[i]] <- ct$businesses[[i]]$image_url
            }
            if (is.null(ct$businesses[[i]]$is_closed)) {
              isClosed[[i]] <- "NA"
            }
            else {
              isClosed[[i]] <- ct$businesses[[i]]$is_closed
            }
            if (is.null(ct$businesses[[i]]$rating)) {
              businessRating[[i]] <- "NA"
            }
            else {
              businessRating[[i]] <- ct$businesses[[i]]$rating
            }
            if (is.null(ct$businesses[[i]]$price)) {
              businessPrice[[i]] <- "NA"
            }
            else {
              businessPrice[[i]] <- ct$businesses[[i]]$price
            }
            if (is.null(ct$businesses[[i]]$location$display_address)) {
              displayAddress[[i]] <- "NA"
              # print(i)
            }
            else {
              if (length(ct$businesses[[i]]$location$display_address) == 1) {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
              else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                displayAddress[[i]] <- add
              }
              else {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
            }
          }
          
          if (is.null(document$results$rating) == TRUE) {
            rating <- NA
            dat <- cbind(dat, rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          else {
            dat <- cbind(dat, document$results$rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          if (is.null(document$results$price_level) == TRUE) {
            price_level <- NA
            dat <- cbind(dat, price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          else {
            dat <- cbind(dat, document$results$price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price")
            dat <- dat %>% dplyr::inner_join(googlePrices)
            dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          if (is.null(document$results$vicinity) == TRUE) {
            vicinity <- NA
            dat <- cbind(dat, vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          else {
            dat <- cbind(dat, document$results$vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          
          photo <- vector("character", length(document$results$name))
          if (length(document$results$photos) != 0) {
            for (i in 1:length(document$results$photos))
            {
              if (is.null(document$results$photos[i][[1]]) == FALSE) {
                photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
              }
              else {
                photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
              }
            }
            
            dat <- data.frame(dat, photo)
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 300, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                  paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else {
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
        }
        else if (nrow(dat) == 0 & nrow(resEventB$events) != 0 & length(ct$businesses) == 1) {
          startLocal <- strsplit(resEventB$events$start$local, "T")
          startLocalDate <- vector("character", length(startLocal))
          startLocalTime <- vector("character", length(startLocal))
          
          endLocal <- strsplit(resEventB$events$end$local, "T")
          endLocalDate <- vector("character", length(endLocal))
          endLocalTime <- vector("character", length(endLocal))
          for (i in 1:length(startLocal))
          {
            startLocalDate[[i]] <- startLocal[[i]][1]
            startLocalTime[[i]] <- startLocal[[i]][2]
          }
          for (i in 1:length(endLocal))
          {
            endLocalDate[[i]] <- endLocal[[i]][1]
            endLocalTime[[i]] <- endLocal[[i]][2]
          }
          startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
          startLocalWeekday <- weekdays(as.Date(startLocalDate))
          
          endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
          endLocalWeekday <- weekdays(as.Date(endLocalDate))
          
          startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          
          
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearPopups() %>%
            setView(lng = clng, lat = clat, zoom = 10) %>%
            addMarkers(
              lng = as.numeric(as.character(res$results$group$group_lon)),
              lat = as.numeric(as.character(res$results$group$group_lat)),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                "<br>",
                "<h6>", res$results$group$name, "</h6>", "<br>",
                res$results$description
              ),
              icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            ) %>%
            addMarkers(
              lng = as.numeric(as.character(resEventB$events$venue$longitude)),
              lat = as.numeric(as.character(resEventB$events$venue$latitude)),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                paste0("<img src = ", resEventB$events$logo$url, ">"),
                "<br>",
                resEventB$events$description$text
              ),
              icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            )
        }
        else if (nrow(dat) == 0 & nrow(resEventB$events) != 0 & length(ct$businesses) > 1) {
          latitudeYelp <- vector("character", length(ct$businesses))
          longitudeYelp <- vector("character", length(ct$businesses))
          businessName <- vector("character", length(ct$businesses))
          imageURL <- vector("character", length(ct$businesses))
          isClosed <- vector("character", length(ct$businesses))
          businessRating <- vector("character", length(ct$businesses))
          businessPrice <- vector("character", length(ct$businesses))
          displayAddress <- vector("character", length(ct$businesses))
          
          for (i in 1:length(ct$businesses))
          {
            if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
              latitudeYelp[[i]] <- "NA"
            }
            else {
              latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
            }
            if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
              longitudeYelp[[i]] <- "NA"
            }
            else {
              longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
            }
            if (is.null(ct$businesses[[i]]$name)) {
              businessName[[i]] <- "NA"
            }
            else {
              businessName[[i]] <- ct$businesses[[i]]$name
            }
            if (is.null(ct$businesses[[i]]$image_url)) {
              imageURL[[i]] <- "NA"
            }
            else {
              imageURL[[i]] <- ct$businesses[[i]]$image_url
            }
            if (is.null(ct$businesses[[i]]$is_closed)) {
              isClosed[[i]] <- "NA"
            }
            else {
              isClosed[[i]] <- ct$businesses[[i]]$is_closed
            }
            if (is.null(ct$businesses[[i]]$rating)) {
              businessRating[[i]] <- "NA"
            }
            else {
              businessRating[[i]] <- ct$businesses[[i]]$rating
            }
            if (is.null(ct$businesses[[i]]$price)) {
              businessPrice[[i]] <- "NA"
            }
            else {
              businessPrice[[i]] <- ct$businesses[[i]]$price
            }
            if (is.null(ct$businesses[[i]]$location$display_address)) {
              displayAddress[[i]] <- "NA"
              # print(i)
            }
            else {
              if (length(ct$businesses[[i]]$location$display_address) == 1) {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
              else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                displayAddress[[i]] <- add
              }
              else {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
            }
          }
          
          startLocal <- strsplit(resEventB$events$start$local, "T")
          startLocalDate <- vector("character", length(startLocal))
          startLocalTime <- vector("character", length(startLocal))
          
          endLocal <- strsplit(resEventB$events$end$local, "T")
          endLocalDate <- vector("character", length(endLocal))
          endLocalTime <- vector("character", length(endLocal))
          for (i in 1:length(startLocal))
          {
            startLocalDate[[i]] <- startLocal[[i]][1]
            startLocalTime[[i]] <- startLocal[[i]][2]
          }
          for (i in 1:length(endLocal))
          {
            endLocalDate[[i]] <- endLocal[[i]][1]
            endLocalTime[[i]] <- endLocal[[i]][2]
          }
          startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
          startLocalWeekday <- weekdays(as.Date(startLocalDate))
          
          endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
          endLocalWeekday <- weekdays(as.Date(endLocalDate))
          
          startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          
          
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearPopups() %>%
            setView(lng = clng, lat = clat, zoom = 10) %>%
            addMarkers(
              lng = as.numeric(as.character(res$results$group$group_lon)),
              lat = as.numeric(as.character(res$results$group$group_lat)),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                "<br>",
                "<h6>", res$results$group$name, "</h6>", "<br>",
                res$results$description
              ),
              icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            ) %>%
            addMarkers(
              lng = as.numeric(as.character(resEventB$events$venue$longitude)),
              lat = as.numeric(as.character(resEventB$events$venue$latitude)),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                paste0("<img src = ", resEventB$events$logo$url, ">"),
                "<br>",
                resEventB$events$description$text
              ),
              icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            ) %>%
            addMarkers(
              lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                paste("Price: ", businessPrice, sep = ""), "<br>",
                paste("Rating: ", businessRating, sep = ""), "<br>",
                paste("Restaurant Closing Status: ", isClosed, sep = ""),
                "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                "<br>"
              ),
              icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            )
        }
        else if (nrow(dat) != 0 & nrow(resEventB$events) != 0 & length(ct$businesses) == 1) {
          # else #if all google, eventbrite and meetup are present
          # {
          startLocal <- strsplit(resEventB$events$start$local, "T")
          startLocalDate <- vector("character", length(startLocal))
          startLocalTime <- vector("character", length(startLocal))
          
          endLocal <- strsplit(resEventB$events$end$local, "T")
          endLocalDate <- vector("character", length(endLocal))
          endLocalTime <- vector("character", length(endLocal))
          for (i in 1:length(startLocal))
          {
            startLocalDate[[i]] <- startLocal[[i]][1]
            startLocalTime[[i]] <- startLocal[[i]][2]
          }
          for (i in 1:length(endLocal))
          {
            endLocalDate[[i]] <- endLocal[[i]][1]
            endLocalTime[[i]] <- endLocal[[i]][2]
          }
          startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
          startLocalWeekday <- weekdays(as.Date(startLocalDate))
          
          endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
          endLocalWeekday <- weekdays(as.Date(endLocalDate))
          
          startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          
          if (is.null(document$results$rating) == TRUE) {
            rating <- NA
            dat <- cbind(dat, rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          else {
            dat <- cbind(dat, document$results$rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          if (is.null(document$results$price_level) == TRUE) {
            price_level <- NA
            dat <- cbind(dat, price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          else {
            dat <- cbind(dat, document$results$price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price")
            dat <- dat %>% dplyr::inner_join(googlePrices)
            dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          if (is.null(document$results$vicinity) == TRUE) {
            vicinity <- NA
            dat <- cbind(dat, vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          else {
            dat <- cbind(dat, document$results$vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          
          photo <- vector("character", length(document$results$name))
          if (length(document$results$photos) != 0) {
            for (i in 1:length(document$results$photos))
            {
              if (is.null(document$results$photos[i][[1]]) == FALSE) {
                photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
              }
              else {
                photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
              }
            }
            
            dat <- data.frame(dat, photo)
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                  paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"), "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
          else {
            
            # dat <- data.frame(dat, photo)
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"),
                  "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
        }
        else if (nrow(dat) != 0 & nrow(resEventB$events) != 0 & length(ct$businesses) > 1) {
          latitudeYelp <- vector("character", length(ct$businesses))
          longitudeYelp <- vector("character", length(ct$businesses))
          businessName <- vector("character", length(ct$businesses))
          imageURL <- vector("character", length(ct$businesses))
          isClosed <- vector("character", length(ct$businesses))
          businessRating <- vector("character", length(ct$businesses))
          businessPrice <- vector("character", length(ct$businesses))
          displayAddress <- vector("character", length(ct$businesses))
          
          for (i in 1:length(ct$businesses))
          {
            if (is.null(ct$businesses[[i]]$coordinates$latitude)) {
              latitudeYelp[[i]] <- "NA"
            }
            else {
              latitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$latitude
            }
            if (is.null(ct$businesses[[i]]$coordinates$longitude)) {
              longitudeYelp[[i]] <- "NA"
            }
            else {
              longitudeYelp[[i]] <- ct$businesses[[i]]$coordinates$longitude
            }
            if (is.null(ct$businesses[[i]]$name)) {
              businessName[[i]] <- "NA"
            }
            else {
              businessName[[i]] <- ct$businesses[[i]]$name
            }
            if (is.null(ct$businesses[[i]]$image_url)) {
              imageURL[[i]] <- "NA"
            }
            else {
              imageURL[[i]] <- ct$businesses[[i]]$image_url
            }
            if (is.null(ct$businesses[[i]]$is_closed)) {
              isClosed[[i]] <- "NA"
            }
            else {
              isClosed[[i]] <- ct$businesses[[i]]$is_closed
            }
            if (is.null(ct$businesses[[i]]$rating)) {
              businessRating[[i]] <- "NA"
            }
            else {
              businessRating[[i]] <- ct$businesses[[i]]$rating
            }
            if (is.null(ct$businesses[[i]]$price)) {
              businessPrice[[i]] <- "NA"
            }
            else {
              businessPrice[[i]] <- ct$businesses[[i]]$price
            }
            if (is.null(ct$businesses[[i]]$location$display_address)) {
              displayAddress[[i]] <- "NA"
              # print(i)
            }
            else {
              if (length(ct$businesses[[i]]$location$display_address) == 1) {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
              else if (length(ct$businesses[[i]]$location$display_address) == 2) {
                add <- paste(ct$businesses[[i]]$location$display_address[[1]], ct$businesses[[i]]$location$display_address[[2]], sep = ", ")
                displayAddress[[i]] <- add
              }
              else {
                displayAddress[[i]] <- ct$businesses[[i]]$location$display_address[[1]]
              }
            }
          }
          
          startLocal <- strsplit(resEventB$events$start$local, "T")
          startLocalDate <- vector("character", length(startLocal))
          startLocalTime <- vector("character", length(startLocal))
          
          endLocal <- strsplit(resEventB$events$end$local, "T")
          endLocalDate <- vector("character", length(endLocal))
          endLocalTime <- vector("character", length(endLocal))
          for (i in 1:length(startLocal))
          {
            startLocalDate[[i]] <- startLocal[[i]][1]
            startLocalTime[[i]] <- startLocal[[i]][2]
          }
          for (i in 1:length(endLocal))
          {
            endLocalDate[[i]] <- endLocal[[i]][1]
            endLocalTime[[i]] <- endLocal[[i]][2]
          }
          startLocalDateFinal <- format(as.Date(startLocalDate), format = "%B %d %Y")
          startLocalWeekday <- weekdays(as.Date(startLocalDate))
          
          endLocalDateFinal <- format(as.Date(endLocalDate), format = "%B %d %Y")
          endLocalWeekday <- weekdays(as.Date(endLocalDate))
          
          startLocalTimeFinal <- format(strptime(startLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          endLocalTimeFinal <- format(strptime(endLocalTime, format = "%H:%M:%S"), "%I:%M:%S %p")
          
          if (is.null(document$results$rating) == TRUE) {
            rating <- NA
            dat <- cbind(dat, rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          else {
            dat <- cbind(dat, document$results$rating)
            colnames(dat) <- c("lng", "lat", "name", "rating")
          }
          if (is.null(document$results$price_level) == TRUE) {
            price_level <- NA
            dat <- cbind(dat, price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          else {
            dat <- cbind(dat, document$results$price_level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price")
            dat <- dat %>% dplyr::inner_join(googlePrices)
            dat <- dat %>% dplyr::select(lng, lat, name, rating, level)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level")
          }
          if (is.null(document$results$vicinity) == TRUE) {
            vicinity <- NA
            dat <- cbind(dat, vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          else {
            dat <- cbind(dat, document$results$vicinity)
            colnames(dat) <- c("lng", "lat", "name", "rating", "price_level", "vicinity")
          }
          
          photo <- vector("character", length(document$results$name))
          if (length(document$results$photos) != 0) {
            for (i in 1:length(document$results$photos))
            {
              if (is.null(document$results$photos[i][[1]]) == FALSE) {
                photo[[i]] <- (paste("https://maps.googleapis.com/maps/api/place/photo?maxwidth=300&photoreference=", document$results$photos[i][[1]]$photo_reference[1], "&sensor=false&key=KEY", sep = ""))
              }
              else {
                photo[[i]] <- "https://vignette2.wikia.nocookie.net/inkagames-english/images/0/0e/No_image.jpg/revision/latest?cb=20170113194025"
              }
            }
            
            dat <- data.frame(dat, photo)
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE, color = "red")
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>",
                  paste0("<img src = ", dat$photo, " width = 350", " height = 300", ">")
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE, color = "red")
              ) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"), "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE, color = "red")
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE, color = "red")
              )
          }
          else {
            leafletProxy("map") %>%
              clearMarkers() %>%
              clearPopups() %>%
              setView(lng = clng, lat = clat, zoom = 10) %>%
              addMarkers(
                lng = as.numeric(as.character(res$results$group$group_lon)),
                lat = as.numeric(as.character(res$results$group$group_lat)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                  paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                  paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                  "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                  "<br>",
                  "<h6>", res$results$group$name, "</h6>", "<br>",
                  res$results$description
                ),
                icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(as.character(resEventB$events$venue$longitude)),
                lat = as.numeric(as.character(resEventB$events$venue$latitude)),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", resEventB$events$name$text, "</br>", "</h4>", "<h5>",
                  paste(startLocalWeekday, startLocalDateFinal, sep = ", "), "<br>",
                  paste(startLocalTimeFinal, endLocalTimeFinal, sep = " - "),
                  "<br>", paste("Venue:", resEventB$events$venue$address$localized_address_display, sep = " "), "</h5>",
                  "<h6>", paste("Event link:", resEventB$events$url, sep = " "), "</h6>", "</br>",
                  paste0("<img src = ", resEventB$events$logo$url, ">"),
                  "<br>",
                  resEventB$events$description$text
                ),
                icon = eventBrightIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = dat$lng, lat = dat$lat, group = "circles",
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", dat$name, "</br>", "</h4>", "<h5>",
                  paste("Address: ", dat$vicinity, sep = ""), "<br>",
                  paste("Price level: ", dat$price_level, sep = ""), "<br>",
                  paste("Rating: ", dat$rating, sep = ""), "</h5>", "</br>"
                ), icon = googleIcon,
                popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              ) %>%
              addMarkers(
                lng = as.numeric(longitudeYelp), lat = as.numeric(latitudeYelp),
                popup = paste(
                  "<br>", "<h4 style='color:red;'>", businessName, "</br>", "</h4>", "<h5>",
                  paste("Price: ", businessPrice, sep = ""), "<br>",
                  paste("Rating: ", businessRating, sep = ""), "<br>",
                  paste("Restaurant Closing Status: ", isClosed, sep = ""),
                  "<br>", paste("Venue: ", displayAddress, sep = " "), "</h5>", "</br>",
                  paste0("<img src = ", imageURL, " width = 350", " height = 300", ">"),
                  "<br>"
                ),
                icon = yelpIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
              )
          }
        }
        else {
          leafletProxy("map") %>%
            clearMarkers() %>%
            clearPopups() %>%
            setView(lng = clng, lat = clat, zoom = 10) %>%
            addMarkers(
              lng = as.numeric(as.character(res$results$group$group_lon)),
              lat = as.numeric(as.character(res$results$group$group_lat)),
              popup = paste(
                "<br>", "<h4 style='color:red;'>", res$results$name, "</br>", "</h4>", "<h5>",
                paste(eventsDat$startWeekday, eventsDat$startDateFinal, sep = ", "), "<br>",
                paste(eventsDat$timeStartFinal, eventsDat$timeEndFinal, sep = " - "),
                "<br>", paste("Venue:", res$results$venue$address_1, sep = " "), "</h5>",
                "<h6>", paste("Event link:", res$results$event_url, sep = " "), "</h6>", "</br>",
                paste0("<img src = ", eventsDat$photos, " width = 350", " height = 300", ">"),
                "<br>",
                "<h6>", res$results$group$name, "</h6>", "<br>",
                res$results$description
              ),
              icon = meetupIcon, popupOptions = popupOptions(minWidth = 400, maxWidth = 400, maxHeight = 400, closeOnClick = TRUE)
            )
        }
        incProgress(0.3, detail = "Finishing...")
      }
    })
  })
  
  
  
  mapClick <- eventReactive(input$eventClick, {
    click <- input$map_click
    return(click)
  })
  
  observeEvent(input$zoomOut, {
    leafletProxy("map") %>%
      setView(lng = -18.455845, lat = -3.26, zoom = 10) %>%
      addProviderTiles(providers$OpenStreetMap.France, options = providerTileOptions(noWrap = TRUE))
  })
  
  observeEvent(input$zoomIn, {
    click <- mapClick()
    clat <- click$lat
    clng <- click$lng
    leafletProxy("map") %>%
      setView(lng = clng, lat = clat, zoom = 10) %>%
      addProviderTiles(providers$OpenStreetMap.France, options = providerTileOptions(noWrap = TRUE))
  })
  
  mapClick2 <- eventReactive(input$eventAddress, {
    address <- as.character(input$address)
    add <- geocode(address)
    return(add)
  })
  
  
  output$Tab <- renderDataTable({
    click <- mapClick()
    clat <- click$lat
    clng <- click$lng
    
    distance <- input$miles * (1609.344)
    
    url <- paste("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=", clat, ",", clng, "&radius=", miles * (1609.344), "&types=", variable, "&key=KEY", sep = "")
    document <- fromJSON(txt = url)
    dat <- data.frame(
      document$results$geometry$location$lng, document$results$geometry$location$lat,
      document$results$name
    )
    
    # colnames(dat) <- c("lng", "lat", "name")
    
    eventBCategoryUrl <- "https://www.eventbriteapi.com/v3/categories/?&token=TOKEN"
    eventCategories <- fromJSON(txt = eventBCategoryUrl)
    eventCategories2 <- data.frame(eventCategories$categories$id, eventCategories$categories$name)
    colnames(eventCategories2) <- c("id", "name")
    eventCategories2$name <- tolower(eventCategories2$name)
    
    if (input$meetupTopic == "") {
      eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi", "&token=TOKEN&expand=venue", sep = "")
      json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&fields=group_photos;key=KEY", sep = "")
      resEventB <- fromJSON(txt = eventBUrl)
      res <- fromJSON(txt = json.url)
    }
    else {
      eventID <- eventCategories2[str_detect(eventCategories2$name, input$meetupTopic), ]
      eventid <- eventID$id
      json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&fields=group_photos;key=KEY", sep = "")
      errorRes <- try(fromJSON(txt = json.url), TRUE)
      if (is.null(eventid) & length(grep("open.connection", errorRes)) == 1) # if error in both meetup and eventbrite apis
      {
        eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi", "&token=TOKEN&expand=venue", sep = "")
        json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&fields=group_photos;key=KEY", sep = "")
        resEventB <- fromJSON(txt = eventBUrl)
        res <- fromJSON(txt = json.url)
      }
      else if (is.null(eventid) & length(grep("open.connection", errorRes)) == 0) # if error in eventbrite but no error in meetup
      {
        eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", miles, "mi", "&token=TOKEN&expand=venue", sep = "")
        json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", miles, "&topic=", "tech", "&fields=group_photos;key=KEY", sep = "")
        resEventB <- fromJSON(txt = eventBUrl)
        res <- fromJSON(txt = json.url)
      }
      else if (is.null(eventid) == FALSE & length(grep("open.connection", errorRes)) == 1) # if no error in eventbrite but error in meetup
      {
        eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi", "&categories=", eventid, "&token=TOKEN&expand=venue", sep = "")
        json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&fields=group_photos;key=KEY", sep = "")
        resEventB <- fromJSON(txt = eventBUrl)
        res <- fromJSON(txt = json.url)
      }
      else # no error in eventbrite and no error in meetup
      {
        eventBUrl <- paste("https://www.eventbriteapi.com/v3/events/search/?location.latitude=", clat, "&location.longitude=", clng, "&location.within=", input$miles, "mi", "&categories=", eventid, "&token=TOKEN&expand=venue", sep = "")
        json.url <- paste("http://api.meetup.com/2/open_events.json?lat=", clat, "&lon=", clng, "&radius=", input$miles, "&topic=", input$meetupTopic, "&fields=group_photos;key=KEY", sep = "")
        resEventB <- fromJSON(txt = eventBUrl)
        res <- fromJSON(txt = json.url)
      }
    }
    return(data.frame(res$results))
  })
  output$Reference <- renderPrint({
    sessionInfo()
  })
}
