
library(jsonlite)
library(anytime)
library(httr)
library(shiny)
library(leaflet)
library(lubridate)
library(stringr)
library(ggmap)


ui <- navbarPage(
  "Location Explorer", inverse = TRUE,
  navbarMenu(
    "API Navigator",
    tabPanel(
      "Eventbrite-Google-Meetup-Yelp",
      # set app background color
      tags$head(
        tags$style(HTML("
                        body {
                        background-color: #f45c5c;
                        color: #000000; 
                        }
                        "))
        ),
      
      sidebarLayout(
        sidebarPanel(
          textInput("meetupTopic", label = "Specify Events Topic (i.e. Technology)", value = ""),
          
          textInput("yelpSearch", label = "Specify Food/Cuisine Type", value = "Cafe"),
          
          # numericInput("weeks", "Specify events time range in weeks", 2),
          
          sliderInput("meetupTime", "Search for Meetup Events in this time range (in weeks): ", min = 1, max = 20, value = c(1, 2)),
          
          selectInput("eventBriteTime", "Search for Eventbrite Events in this time range: ", c(
            "today" = "today", "tomorrow" = "tomorrow",
            "this_week" = "this_week", "this_weekend" = "this_weekend",
            "next_week" = "next_week", "this_month" = "this_month",
            "next_month" = "next_month"
          )),
          
          
          div(
            style = "display: inline-block;vertical-align:right; width: 165px;",
            actionButton("eventClick", "Show Events on Click", style = "color: #ffffff;background-color: #f45c5c;margin: 4px;")
          ),
          
          tags$head(
            tags$style(HTML("#eventClick{font-weight:bold;}"))
          ),
          
          div(
            style = "display: inline-block;vertical-align:right; width: 25px;",
            
            actionButton("eventAddress", "Show Events on Address", style = "color: #ffffff;background-color: #f45c5c;margin: 6px;")
          ),
          
          tags$head(
            tags$style(HTML("#eventAddress{font-weight:bold;}"))
          ),
          
          numericInput("miles", "Specify Search Radius (in miles)", 2, min = 1, max = 31),
          textInput("address", label = "Enter address", value = "Cambridge, MA"),
          wellPanel(
            radioButtons(
              "variable", "Choose a type:",
              c(
                "Accounting" = "accounting",
                "Airport" = "airport", "Amusement Park" = "amusement_park",
                "Aquarium" = "aquarium", "Art Gallery" = "art_gallery",
                "ATM" = "atm", "Bakery" = "bakery", "Bank" = "bank",
                "Bar" = "bar", "Beauty Salon" = "beauty_salon",
                "Bicycle Store" = "bicycle_store", "Book Store" = "book_store",
                "Bowling Alley" = "bowling_alley", "Bus Station" = "bus_station",
                "Cafe" = "cafe", "Campground" = "campground",
                "Car Dealer" = "car_dealer", "Car Rental" = "car_rental",
                "Car Repair" = "car_repair", "Car Wash" = "car_wash",
                "Casino" = "casino", "Cemetery" = "cemetery", "Church" = "church",
                "City Hall" = "city_hall", "Clothing Store" = "clothing_store",
                "Convenience Store" = "convenience_store", "Courthouse" = "courthouse",
                "Dentist" = "dentist", "Department Store" = "department_store",
                "Doctor" = "doctor", "Electrician" = "electrician",
                "Electronics Store" = "electronics_store", "Embassy" = "embassy",
                "Fire Station" = "fire_station", "Florist" = "florist",
                "Funeral Home" = "funeral_home",
                "Furniture Store" = "furniture_store", "Gas Station" = "gas_station",
                "Gym" = "gym", "Hair Care" = "hair_care", "Hardware Store" = "hardware_store",
                "Hindu Temple" = "hindu_temple",
                "Home Goods Store" = "home_goods_store", "Hospital" = "hospital",
                "Insurance Agency" = "insurance_agency", "Jewelry Store" = "jewelry_store",
                "Laundry" = "laundry", "Lawyer" = "lawyer", "Library" = "library",
                "Liquor Store" = "liquor_store", "Local Government Office" = "local_government_office",
                "Locksmith" = "locksmith", "Lodging" = "lodging", "Meal Delivery" = "meal_delivery",
                "Meal Takeaway" = "meal_takeaway", "Mosque" = "mosque", "Movie Rental" = "movie_rental",
                "Movie Theater" = "movie_theater", "Moving Company" = "moving_company", "Museum" = "museum",
                "Night Club" = "night_club", "Painter" = "painter", "Park" = "park",
                "Parking" = "parking", "Pet Store" = "pet_store", "Pharmacy" = "pharmacy",
                "Physiotherapist" = "physiotherapist",
                "Plumber" = "plumber", "Police" = "police", "Post Office" = "post_office",
                "Real Estate Agency" = "real_estate_agency", "Restaurant" = "restaurant",
                "Roofing Contractor" = "roofing_contractor", "RV Park" = "rv_park",
                "School" = "school", "Shoe Store" = "shoe_store", "Shopping Mall" = "shopping_mall",
                "Spa" = "spa", "Stadium" = "stadium", "Storage" = "storage", "Store" = "store",
                "Subway Station" = "subway_station", "Synagogue" = "synagogue",
                "Taxi Stand" = "taxi_stand", "Train Station" = "train_station",
                "Transit Station" = "transit_station", "Travel Agency" = "travel_agency",
                "University" = "university", "Veterinary Care" = "veterinary_care",
                "Zoo" = "zoo"
              ), selected = "cafe"
            ), style = "overflow-y:scroll; max-height: 600px"
          )
        ),
        mainPanel(
          h4("Location Explorer App displays location enabled local businesses, landmarks, and
                                            transit information via the Google Places API. In addition, this app
                                            displays local events 
                                            via the Meetup and Eventbrite APIs and food vendors
                                            via the Yelp API. Specify a location by clicking on the map or by manually searching the address by 
                                            vendor and event type and timeframe. Click on a popup to 
                                            retrieve more information about the business/event. By combining business, event and food services, 
                                            Location Explorer Application provides a centralized and systematized platform for
                                            information retrieval.", style = "color:white"),
          
          tags$head(
            tags$style(HTML("#zoomOut{font-weight:bold;}"))
          ),
          
          tags$head(
            tags$style(HTML("#zoomIn{font-weight:bold;}"))
          ),
          
          leafletOutput("map", height = 700)
        )
      )
        )
        ),
  tabPanel(
    "References",
    verbatimTextOutput("Reference")
  )
    )