#Daniel Iovino
#DSSA Practicum Whale Catalog App

#Create library
library(shiny)
library(tidyverse)
library(readxl)
library(dplyr)
library(shinydashboard)
library(glue)
library(scales)
library(plotly)
library(shinyWidgets)
library(shinythemes)
library(DT)
library(ggplot2) #Graphing
library(dplyr) #Data exploration functions
library(leaflet) #Map creation
library(leaflet.providers) #Assists map creation
library(sf) #Work with geopoint data
library(lubridate) #Seperate year from date column to graph new whales in cat. per year
library(styler)


# Set working directory (update if needed)
setwd("C:/Users/dan iovino/OneDrive - go.Stockton.edu/DSSA Practicum/DSSA_Practicum_Iovino_v2")
list.files()

# Load in whale data
Humpback <- read.csv("Humpback_Prac.csv", header = TRUE)

## Plot all recorded M.n. observations 2011-2024 ##

#Create data frame that has only start coords from CMWWRC_Clean
Whale_Coords <- data.frame(Humpback$Latitude, Humpback$Longitude, Humpback$Date, Humpback$Catologued.Animals)

#Rename columns of Whale_Coords
Whale_Coords <- Whale_Coords %>% rename(
  long  =  Humpback.Longitude,
  lat  =  Humpback.Latitude,
  WhaleID = Humpback.Catologued.Animals
)

#Check that lat and long values are integers not characters
str(Whale_Coords$lat)
str(Whale_Coords$long)

#Set proper names
lat <- Humpback$Latitude
long <- Humpback$Longitude
Date <- Humpback$Date

Whale_Coords$lat <- as.numeric(Whale_Coords$lat)
Whale_Coords$long <- as.numeric(Whale_Coords$long)

str(Whale_Coords)

Whale_Coords <- Whale_Coords %>%
  mutate(
    CleanDate = as.Date(Humpback.Date, format = "%m/%d/%Y"),
    Month = lubridate::month(CleanDate, label = TRUE, abbr = TRUE)
  )

month_choices <- sort(unique(Whale_Coords$Month))

#Define all_whales list for search bar
all_whales <- list(
  # Mn0001
  fluidRow(
    column(12,
           tags$h3("Mn0001, aka NJ1101"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0001.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0001.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0001.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  # Mn0002
  fluidRow(
    column(12,
           tags$h3("Mn0002, aka NJ1102"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0002.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0003
  fluidRow(
    column(12,
           tags$h3("Mn0003, aka ESCA"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0003.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0003.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0004
  fluidRow(
    column(12,
           tags$h3("Mn0004, aka ANGUS"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0004.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0004.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0005
  fluidRow(
    column(12,
           tags$h3("Mn0005, aka NJ1201"),
           tags$p("Dorsal Characteristics: Hooked, Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0005.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0005.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0005.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0006
  fluidRow(
    column(12,
           tags$h3("Mn0006, aka FINGERPAINT"),
           tags$p("Dorsal Characteristics: Damaged"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0006.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0006.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0007
  fluidRow(
    column(12,
           tags$h3("Mn0007, aka NJ1202, NJ1304"),
           tags$p("Dorsal Characteristics: Triangular, Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0007.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0007.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0007.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0008
  fluidRow(
    column(12,
           tags$h3("Mn0008, aka NJ1203"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0008.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0008.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0009
  fluidRow(
    column(12,
           tags$h3("Mn0009, aka VA1222"),
           tags$p("Dorsal Characteristics: Rounded, Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0009.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0009.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0010
  fluidRow(
    column(12,
           tags$h3("Mn0010, aka BATCAVE'S '10 CALF"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0010.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0010.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0011
  fluidRow(
    column(12,
           tags$h3("Mn0011, aka SMOG'S '11 CALF"),
           tags$p("Dorsal Characteristics: Triangular, Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0011.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0011.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0011.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0012
  fluidRow(
    column(12,
           tags$h3("Mn0012"),
           tags$p("Dorsal Characteristics: Sharp"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0012.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0012.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0013
  fluidRow(
    column(12,
           tags$h3("Mn0013, aka NJ1301, VA1225"),
           tags$p("Dorsal Characteristics: Sharp"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0013.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0013.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0013.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0015
  fluidRow(
    column(12,
           tags$h3("Mn0015, aka NJ1302"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0015.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0015.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0015.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0016
  fluidRow(
    column(12,
           tags$h3("Mn0016, aka NJ1402"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0016.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0016.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0016.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0017
  fluidRow(
    column(12,
           tags$h3("Mn0017, aka NJ1401"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0017.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0017.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0017.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0018
  fluidRow(
    column(12,
           tags$h3("Mn0018, aka NJ1403"),
           tags$p("Dorsal Characteristics: Hooked, Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0018.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0018.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0018.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0019
  fluidRow(
    column(12,
           tags$h3("Mn0019, aka NJ1404"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0019.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0019.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0020
  fluidRow(
    column(12,
           tags$h3("Mn0020, aka NJ1405"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0020.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0020.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0020.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0021
  fluidRow(
    column(12,
           tags$h3("Mn0021, aka NJ1406"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0021.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0021.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0021.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0022
  fluidRow(
    column(12,
           tags$h3("Mn0022, aka NJ1407"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0022.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0022.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0022.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0023
  fluidRow(
    column(12,
           tags$h3("Mn0023, aka NJ1408"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0023.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0023.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0023.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0024
  fluidRow(
    column(12,
           tags$h3("Mn0024, aka NJ1410"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0024.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0024.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0024.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0025
  fluidRow(
    column(12,
           tags$h3("Mn0025"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0025.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0026
  fluidRow(
    column(12,
           tags$h3("Mn0026"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0026.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0026.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0027
  fluidRow(
    column(12,
           tags$h3("Mn0027, aka NJ1414"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0027.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0027.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0028
  fluidRow(
    column(12,
           tags$h3("Mn0028, aka NJ1411"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0028.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0028.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0028.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0029
  fluidRow(
    column(12,
           tags$h3("Mn0029, aka NJ1412"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0029.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0029.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0030
  fluidRow(
    column(12,
           tags$h3("Mn0030, aka NJ1413"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0030.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0030.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0031
  fluidRow(
    column(12,
           tags$h3("Mn0031, aka NJ1516"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0031.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0031.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0032
  fluidRow(
    column(12,
           tags$h3("Mn0032, aka NJ1500"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0032.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0032.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0032.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0033
  fluidRow(
    column(12,
           tags$h3("Mn0033, aka NJ1503"),
           tags$p("Dorsal Characteristics: Rounded, Emaciated"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0033.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0033.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0033.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0034
  fluidRow(
    column(12,
           tags$h3("Mn0034, aka NJ1513"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0034.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0034.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0034.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0035
  fluidRow(
    column(12,
           tags$h3("Mn0035, aka NJ1504"),
           tags$p("Dorsal Characteristics: Triangular, Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0035.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0035.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0035.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0036
  fluidRow(
    column(12,
           tags$h3("Mn0036, aka NJ1502, NJ1614"),
           tags$p("Dorsal Characteristics: Bulky, Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0036.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0036.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0036.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0037
  fluidRow(
    column(12,
           tags$h3("Mn0037"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0037.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0037.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0038
  fluidRow(
    column(12,
           tags$h3("Mn0038, aka NJ1505"),
           tags$p("Dorsal Characteristics: Bulky, Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0038.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0038.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0038.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0039
  fluidRow(
    column(12,
           tags$h3("Mn0039, aka NJ1501"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0039.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0039.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0039.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0040
  fluidRow(
    column(12,
           tags$h3("Mn0040"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0040.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0040.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0041
  fluidRow(
    column(12,
           tags$h3("Mn0041, aka NJ1507"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0041.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0041.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0041.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0042
  fluidRow(
    column(12,
           tags$h3("Mn0042"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0042.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0042.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0042.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0043
  fluidRow(
    column(12,
           tags$h3("Mn0043, aka DANCER, VA1218"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0043.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0043.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0043.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0044
  fluidRow(
    column(12,
           tags$h3("Mn0044, aka NJ1515"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0044.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0044.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0044.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0045
  fluidRow(
    column(12,
           tags$h3("Mn0045"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0045.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0045.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0045.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0046
  fluidRow(
    column(12,
           tags$h3("Mn0046, aka NJ1510"),
           tags$p("Dorsal Characteristics: Bulky, Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0046.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0046.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0046.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0047
  fluidRow(
    column(12,
           tags$h3("Mn0047"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0047.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0048
  fluidRow(
    column(12,
           tags$h3("Mn0048, aka NJ1514"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0048.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0048.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0048.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0049
  fluidRow(
    column(12,
           tags$h3("Mn0049"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0049.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0049.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0050
  fluidRow(
    column(12,
           tags$h3("Mn0050, aka NJ1511"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0050.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0050.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0050.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0051
  fluidRow(
    column(12,
           tags$h3("Mn0051, aka NJ1512"),
           tags$p("Dorsal Characteristics: Bulky, Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0051.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0051.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0051.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0052
  fluidRow(
    column(12,
           tags$h3("Mn0052, aka NJ1601"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0052.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0052.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0053
  fluidRow(
    column(12,
           tags$h3("Mn0053, aka NJ1602"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0053.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0054
  fluidRow(
    column(12,
           tags$h3("Mn0054"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0054.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0055
  fluidRow(
    column(12,
           tags$h3("Mn0055"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0055.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0055.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0056
  fluidRow(
    column(12,
           tags$h3("Mn0056, aka NJ1603"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0056.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0056.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0056.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0057
  fluidRow(
    column(12,
           tags$h3("Mn0057, aka NJ1605"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0057.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0057.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0057.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0058
  fluidRow(
    column(12,
           tags$h3("Mn0058, aka NJ1610"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0058.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0058.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0058.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0059
  fluidRow(
    column(12,
           tags$h3("Mn0059, aka NJ1604"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0059.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0059.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0059.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0060
  fluidRow(
    column(12,
           tags$h3("Mn0060"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0060.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0061
  fluidRow(
    column(12,
           tags$h3("Mn0061"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0061.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0062
  fluidRow(
    column(12,
           tags$h3("Mn0062, aka NJ1606"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0062.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0062.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0063
  fluidRow(
    column(12,
           tags$h3("Mn0063, aka NJ1600"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0063.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0063.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0063.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0064
  fluidRow(
    column(12,
           tags$h3("Mn0064"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0064.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0064.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0065
  fluidRow(
    column(12,
           tags$h3("Mn0065, aka NJ1618"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0065.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0065.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0065.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0066
  fluidRow(
    column(12,
           tags$h3("Mn0066, aka NJ1611"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0066.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0067
  fluidRow(
    column(12,
           tags$h3("Mn0067, aka NJ1607"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0067.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0067.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0067.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0068
  fluidRow(
    column(12,
           tags$h3("Mn0068, aka NJ1613"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0068.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0068.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0068.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0069
  fluidRow(
    column(12,
           tags$h3("Mn0069, aka NJ1608"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0069.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0069.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0069.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0070
  fluidRow(
    column(12,
           tags$h3("Mn0070"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0070.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0070.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0071
  fluidRow(
    column(12,
           tags$h3("Mn0071, aka CAMPFIRE"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0071.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0071.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0071.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0072
  fluidRow(
    column(12,
           tags$h3("Mn0072, aka NJ1615"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0072.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0072.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0072.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0074
  fluidRow(
    column(12,
           tags$h3("Mn0074"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0074.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0075
  fluidRow(
    column(12,
           tags$h3("Mn0075, aka EMBROIDERY"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0075.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0075.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0076
  fluidRow(
    column(12,
           tags$h3("Mn0076"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0076.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0076.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0077
  fluidRow(
    column(12,
           tags$h3("Mn0077, aka NJ1617"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0077.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0077.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0078
  fluidRow(
    column(12,
           tags$h3("Mn0078, aka NJ1700"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0078.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0078.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0079
  fluidRow(
    column(12,
           tags$h3("Mn0079, aka NJ1701"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0079.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0079.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0080
  fluidRow(
    column(12,
           tags$h3("Mn0080, aka NJ1702"),
           tags$p("Dorsal Characteristics: Triangular, Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0080.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0080.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0080.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0081
  fluidRow(
    column(12,
           tags$h3("Mn0081, aka NJ1714"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0081.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0081.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0082
  fluidRow(
    column(12,
           tags$h3("Mn0082, aka NJ1703"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0082.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0082.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0082.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0083
  fluidRow(
    column(12,
           tags$h3("Mn0083, aka NJ1705"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0083.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0083.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0084
  fluidRow(
    column(12,
           tags$h3("Mn0084"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0084.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0085
  fluidRow(
    column(12,
           tags$h3("Mn0085, aka NJ1712"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0085.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0085.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0086
  fluidRow(
    column(12,
           tags$h3("Mn0086"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0086.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0087
  fluidRow(
    column(12,
           tags$h3("Mn0087, aka NJ1707"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0087.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0087.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0088
  fluidRow(
    column(12,
           tags$h3("Mn0088, aka NJ1715"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0088.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0088.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0089
  fluidRow(
    column(12,
           tags$h3("Mn0089"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0089.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0090
  fluidRow(
    column(12,
           tags$h3("Mn0090"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0090.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0090.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0091
  fluidRow(
    column(12,
           tags$h3("Mn0091, aka DAYBREAK"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0091.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0091.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0091.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0092
  fluidRow(
    column(12,
           tags$h3("Mn0092, aka NJ1713"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0092.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0092.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0093
  fluidRow(
    column(12,
           tags$h3("Mn0093"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0093.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0093.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0093.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0094
  fluidRow(
    column(12,
           tags$h3("Mn0094, aka NJ1711"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0094.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0094.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0094.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0095
  fluidRow(
    column(12,
           tags$h3("Mn0095"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0095.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0095.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0096
  fluidRow(
    column(12,
           tags$h3("Mn0096"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0096.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0096.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0098
  fluidRow(
    column(12,
           tags$h3("Mn0098, aka NJ1708"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0098.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0098.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0099
  fluidRow(
    column(12,
           tags$h3("Mn0099, aka NJ1709"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "L_Dorsal_0099.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0099.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0099.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0100
  fluidRow(
    column(12,
           tags$h3("Mn0100, aka NJ1710"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0100.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0100.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0101
  fluidRow(
    column(12,
           tags$h3("Mn0101"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0101.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0101.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0102
  fluidRow(
    column(12,
           tags$h3("Mn0102"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0102.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0103
  fluidRow(
    column(12,
           tags$h3("Mn0103"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0103.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0103.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0103.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0104
  fluidRow(
    column(12,
           tags$h3("Mn0104"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0104.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0104.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0104.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0105
  fluidRow(
    column(12,
           tags$h3("Mn0105, aka HAMMY"),
           tags$p("Dorsal Characteristics: Damaged"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0105.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0105.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0106
  fluidRow(
    column(12,
           tags$h3("Mn0106"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0106.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0106.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0107
  fluidRow(
    column(12,
           tags$h3("Mn0107"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0107.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0107.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0107.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0108
  fluidRow(
    column(12,
           tags$h3("Mn0108"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0108.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0108.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0108.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0109
  fluidRow(
    column(12,
           tags$h3("Mn0109"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0109.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0109.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0110
  fluidRow(
    column(12,
           tags$h3("Mn0110"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0110.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0110.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0111
  fluidRow(
    column(12,
           tags$h3("Mn0111"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0111.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0111.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0111.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0112
  fluidRow(
    column(12,
           tags$h3("Mn0112"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0112.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0112.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0112.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0113
  fluidRow(
    column(12,
           tags$h3("Mn0113"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0113.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0114
  fluidRow(
    column(12,
           tags$h3("Mn0114"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0114.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0114.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0114.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0115
  fluidRow(
    column(12,
           tags$h3("Mn0115"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0115.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0116
  fluidRow(
    column(12,
           tags$h3("Mn0116"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0116.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0116.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0116.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0117
  fluidRow(
    column(12,
           tags$h3("Mn0117"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0117.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0117.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0117.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0118
  fluidRow(
    column(12,
           tags$h3("Mn0118"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0118.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0118.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0119
  fluidRow(
    column(12,
           tags$h3("Mn0119"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0119.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0119.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0120
  fluidRow(
    column(12,
           tags$h3("Mn0120"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0120.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0120.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0120.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0121
  fluidRow(
    column(12,
           tags$h3("Mn0121, aka SCRATCHY"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0121.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0121.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0122
  fluidRow(
    column(12,
           tags$h3("Mn0122"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0122.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0122.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0123
  fluidRow(
    column(12,
           tags$h3("Mn0123"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0123.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0123.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0124
  fluidRow(
    column(12,
           tags$h3("Mn0124"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0124.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0124.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0124.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0125
  fluidRow(
    column(12,
           tags$h3("Mn0125"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0125.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0125.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0126
  fluidRow(
    column(12,
           tags$h3("Mn0126"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0126.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0126.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0126.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0127
  fluidRow(
    column(12,
           tags$h3("Mn0127"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0127.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0128
  fluidRow(
    column(12,
           tags$h3("Mn0128"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0128.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0128.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0129
  fluidRow(
    column(12,
           tags$h3("Mn0129"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0129.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0129.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0130
  fluidRow(
    column(12,
           tags$h3("Mn0130"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0130.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0130.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0131
  fluidRow(
    column(12,
           tags$h3("Mn0131"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0131.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0132
  fluidRow(
    column(12,
           tags$h3("Mn0132"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0132.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0132.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0132.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0133
  fluidRow(
    column(12,
           tags$h3("Mn0133"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0133.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0133.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0134
  fluidRow(
    column(12,
           tags$h3("Mn0134"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0134.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0134.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0135
  fluidRow(
    column(12,
           tags$h3("Mn0135"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0135.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0135.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0136
  fluidRow(
    column(12,
           tags$h3("Mn0136"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0136.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0136.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0137
  fluidRow(
    column(12,
           tags$h3("Mn0137, aka ZORRO"),
           tags$p("Dorsal Characteristics: Damaged"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0137.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0137.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0137.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0138
  fluidRow(
    column(12,
           tags$h3("Mn0138"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0138.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0138.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0139
  fluidRow(
    column(12,
           tags$h3("Mn0139"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0139.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0139.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0140
  fluidRow(
    column(12,
           tags$h3("Mn0140"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0140.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0140.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0140.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0141
  fluidRow(
    column(12,
           tags$h3("Mn0141"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0141.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0141.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0141.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0142
  fluidRow(
    column(12,
           tags$h3("Mn0142"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0142.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0142.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0143
  fluidRow(
    column(12,
           tags$h3("Mn0143"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0143.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0143.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0144
  fluidRow(
    column(12,
           tags$h3("Mn0144"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0144.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0144.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0144.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0145
  fluidRow(
    column(12,
           tags$h3("Mn0145"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0145.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0145.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0145.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0146
  fluidRow(
    column(12,
           tags$h3("Mn0146"),
           tags$p("Dorsal Characteristics: Bulky, Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0146.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0146.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0146.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0147
  fluidRow(
    column(12,
           tags$h3("Mn0147"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0147.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0147.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0147.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0148
  fluidRow(
    column(12,
           tags$h3("Mn0148"),
           tags$p("Dorsal Characteristics: Rounded, Triangular, Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0148.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0148.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0148.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0149
  fluidRow(
    column(12,
           tags$h3("Mn0149"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0149.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0149.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0149.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0150
  fluidRow(
    column(12,
           tags$h3("Mn0150"),
           tags$p("Dorsal Characteristics: Rounded, Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0150.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0150.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0150.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0151
  fluidRow(
    column(12,
           tags$h3("Mn0151"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0151.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0151.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0151.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0152
  fluidRow(
    column(12,
           tags$h3("Mn0152"),
           tags$p("Dorsal Characteristics: Triangular, Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0152.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0152.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0152.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0153
  fluidRow(
    column(12,
           tags$h3("Mn0153, aka MOJO"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0153.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0153.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0153.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0154
  fluidRow(
    column(12,
           tags$h3("Mn0154"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0154.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0154.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0154.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0155
  fluidRow(
    column(12,
           tags$h3("Mn0155"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0155.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0155.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0155.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0156
  fluidRow(
    column(12,
           tags$h3("Mn0156"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0156.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0156.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0156.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0157
  fluidRow(
    column(12,
           tags$h3("Mn0157"),
           tags$p("Dorsal Characteristics: Damaged"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0157.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0157.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0157.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0158
  fluidRow(
    column(12,
           tags$h3("Mn0158"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0158.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0158.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0159
  fluidRow(
    column(12,
           tags$h3("Mn0159, aka NYC0051"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0159.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0160
  fluidRow(
    column(12,
           tags$h3("Mn0160"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0160.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0160.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0160.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0161
  fluidRow(
    column(12,
           tags$h3("Mn0161"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0161.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0161.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0161.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0162
  fluidRow(
    column(12,
           tags$h3("Mn0162"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0162.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0162.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0162.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0163
  fluidRow(
    column(12,
           tags$h3("Mn0163"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0163.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0164
  fluidRow(
    column(12,
           tags$h3("Mn0164"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0164.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0164.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0164.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0166
  fluidRow(
    column(12,
           tags$h3("Mn0166"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0166.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0166.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0166.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0167
  fluidRow(
    column(12,
           tags$h3("Mn0167"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0167.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0167.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0167.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0168
  fluidRow(
    column(12,
           tags$h3("Mn0168"),
           tags$p("Dorsal Characteristics: Damaged"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0168.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0168.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0168.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0169
  fluidRow(
    column(12,
           tags$h3("Mn0169"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0169.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0169.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0169.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0170
  fluidRow(
    column(12,
           tags$h3("Mn0170"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0170.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0170.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0171
  fluidRow(
    column(12,
           tags$h3("Mn0171"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0171.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0173
  fluidRow(
    column(12,
           tags$h3("Mn0173"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0173.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0173.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0173.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0174
  fluidRow(
    column(12,
           tags$h3("Mn0174"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0174.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0174.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0175
  fluidRow(
    column(12,
           tags$h3("Mn0175"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0175.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0175.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0175.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0176
  fluidRow(
    column(12,
           tags$h3("Mn0176"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0176.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0176.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0176.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0177
  fluidRow(
    column(12,
           tags$h3("Mn0177"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0177.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0177.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0177.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0178
  fluidRow(
    column(12,
           tags$h3("Mn0178"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0178.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0178.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0178.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0179
  fluidRow(
    column(12,
           tags$h3("Mn0179"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0179.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0179.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0180
  fluidRow(
    column(12,
           tags$h3("Mn0180"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0180.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0180.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0181
  fluidRow(
    column(12,
           tags$h3("Mn0181"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0181.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0181.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0181.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0182
  fluidRow(
    column(12,
           tags$h3("Mn0182"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0182.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0182.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0183
  fluidRow(
    column(12,
           tags$h3("Mn0183"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0183.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0184
  fluidRow(
    column(12,
           tags$h3("Mn0184"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0184.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0184.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0185
  fluidRow(
    column(12,
           tags$h3("Mn0185"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0185.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0185.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0185.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0186
  fluidRow(
    column(12,
           tags$h3("Mn0186"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0186.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0186.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0186.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0187
  fluidRow(
    column(12,
           tags$h3("Mn0187"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0187.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0187.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0187.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0188
  fluidRow(
    column(12,
           tags$h3("Mn0188"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0188.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0188.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0188.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0189
  fluidRow(
    column(12,
           tags$h3("Mn0189"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0189.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0189.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0189.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0190
  fluidRow(
    column(12,
           tags$h3("Mn0190"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0190.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0190.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0190.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0191
  fluidRow(
    column(12,
           tags$h3("Mn0191"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0191.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0191.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0191.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0192
  fluidRow(
    column(12,
           tags$h3("Mn0192"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0192.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0192.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0192.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0193
  fluidRow(
    column(12,
           tags$h3("Mn0193"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0193.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0193.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0193.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0194
  fluidRow(
    column(12,
           tags$h3("Mn0194"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0194.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0194.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0194.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0195
  fluidRow(
    column(12,
           tags$h3("Mn0195"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0195.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0195.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0195.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0196
  fluidRow(
    column(12,
           tags$h3("Mn0196"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0196.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0196.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0197
  fluidRow(
    column(12,
           tags$h3("Mn0197"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0197.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0197.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0198
  fluidRow(
    column(12,
           tags$h3("Mn0198"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0198.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0198.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0199
  fluidRow(
    column(12,
           tags$h3("Mn0199, aka MALE"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0199.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0199.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0199.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0200
  fluidRow(
    column(12,
           tags$h3("Mn0200"),
           tags$p("Dorsal Characteristics: -"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0200.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0201
  fluidRow(
    column(12,
           tags$h3("Mn0201"),
           tags$p("Dorsal Characteristics: "),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0201.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0201.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0201.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0202
  fluidRow(
    column(12,
           tags$h3("Mn0202"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0202.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0202.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0202.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0203
  fluidRow(
    column(12,
           tags$h3("Mn0203"),
           tags$p("Dorsal Characteristics: White Pigmentation Trailing, Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0203.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0203.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0204
  fluidRow(
    column(12,
           tags$h3("Mn0204"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0204.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0204.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0204.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0205
  fluidRow(
    column(12,
           tags$h3("Mn0205"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0205.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0205.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0205.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0206
  fluidRow(
    column(12,
           tags$h3("Mn0206"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0206.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0206.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0207
  fluidRow(
    column(12,
           tags$h3("Mn0207, aka CMPSC WHALE"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0207.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0207.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0208
  fluidRow(
    column(12,
           tags$h3("Mn0208"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0208.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0208.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0209
  fluidRow(
    column(12,
           tags$h3("Mn0209"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0209.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$img(src = "Fluke_0209.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0210
  fluidRow(
    column(12,
           tags$h3("Mn0210"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0210.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No left-side photo")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0211
  fluidRow(
    column(12,
           tags$h3("Mn0211"),
           tags$p("Dorsal Characteristics: Bulky, White Pigmentation Trailing"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0211.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0211.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0212
  fluidRow(
    column(12,
           tags$h3("Mn0212"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0212.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0213
  fluidRow(
    column(12,
           tags$h3("Mn0213"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0213.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0213.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0214
  fluidRow(
    column(12,
           tags$h3("Mn0214"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0214.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0214.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0215
  fluidRow(
    column(12,
           tags$h3("Mn0215"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0215.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0215.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0216
  fluidRow(
    column(12,
           tags$h3("Mn0216"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0216.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0216.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0216.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0217
  fluidRow(
    column(12,
           tags$h3("Mn0217"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0217.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0217.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0217.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0218
  fluidRow(
    column(12,
           tags$h3("Mn0218"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0218.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0218.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0218.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0219
  fluidRow(
    column(12,
           tags$h3("Mn0219"),
           tags$p("Dorsal Characteristics: Emaciated, Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0219.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0219.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0220
  fluidRow(
    column(12,
           tags$h3("Mn0220"),
           tags$p("Dorsal Characteristics: Pointed, Emaciated"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0220.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0220.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0221
  fluidRow(
    column(12,
           tags$h3("Mn0221"),
           tags$p("Dorsal Characteristics: Pointed"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0221.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0221.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0221.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0223
  fluidRow(
    column(12,
           tags$h3("Mn0223"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0223.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0223.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0223.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0224
  fluidRow(
    column(12,
           tags$h3("Mn0224"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0224.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0224.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0224.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0225
  fluidRow(
    column(12,
           tags$h3("Mn0225"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0225.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0225.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0226
  fluidRow(
    column(12,
           tags$h3("Mn0226"),
           tags$p("Dorsal Characteristics: Triangular"),
           fluidRow(
             column(4, tags$p("No right-side photo")),
             column(4, tags$img(src = "L_Dorsal_0226.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0226.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0227
  fluidRow(
    column(12,
           tags$h3("Mn0227"),
           tags$p("Dorsal Characteristics: Rounded"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0227.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0227.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0227.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0228
  fluidRow(
    column(12,
           tags$h3("Mn0228"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0228.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0228.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0228.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0229
  fluidRow(
    column(12,
           tags$h3("Mn0229"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0229.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0229.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0229.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0230
  fluidRow(
    column(12,
           tags$h3("Mn0230, aka VARF#1222"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0230.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0230.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0230.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0231
  fluidRow(
    column(12,
           tags$h3("Mn0231"),
           tags$p("Dorsal Characteristics: Damaged"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0231.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0231.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0231.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0232
  fluidRow(
    column(12,
           tags$h3("Mn0232"),
           tags$p("Dorsal Characteristics: Bulky"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0232.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0232.jpg",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0233
  fluidRow(
    column(12,
           tags$h3("Mn0233"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0233.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0233.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0233.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0234
  fluidRow(
    column(12,
           tags$h3("Mn0234"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0234.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0234.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0235
  fluidRow(
    column(12,
           tags$h3("Mn0235"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0235.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0235.png",style = "width: 100%; height: auto;")),
             column(4, tags$p("No fluke photo"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0236
  fluidRow(
    column(12,
           tags$h3("Mn0236"),
           tags$p("Dorsal Characteristics: "),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0236.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0236.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0236.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0237
  fluidRow(
    column(12,
           tags$h3("Mn0237"),
           tags$p("Dorsal Characteristics: Hooked"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0237.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0237.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0237.jpg",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  ),
  #Mn0238
  fluidRow(
    column(12,
           tags$h3("Mn0238"),
           tags$p("Dorsal Characteristics: Notched"),
           fluidRow(
             column(4, tags$img(src = "R_Dorsal_0238.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "L_Dorsal_0238.png",style = "width: 100%; height: auto;")),
             column(4, tags$img(src = "Fluke_0238.png",style = "width: 100%; height: auto;"))
           ),
           tags$hr(style = "border-top: 1px solid #ccc;")
    )
  )
)

# Define UI
ui <- navbarPage(
  title = div(
    img(src = "https://i0.wp.com/www.capemay.com/play/wp-content/uploads/2021/07/Cape-May-Whale-Watch-Logo-01.png?fit=500%2C172&ssl=1",
        height = "30px", style = "margin-right:10px;"),
    span("Whalebase", style = "vertical-align:middle; font-size: 24px;")
  ),
  theme = shinytheme("superhero"),
  
  header = tags$head(
    tags$style(HTML("
      body, h1, h2, h3, h4, h5, h6, p, label, .shiny-text-output {
        color: white !important;
      }
      table.dataTable td, table.dataTable th {
        color: white !important;
      }
      table.dataTable {
        background-color: #2b3e50 !important;
      }
      table.dataTable tbody tr:hover {
        background-color: #3e4d61 !important;
      }
      .dataTables_length select {
        color: black !important;
      }
    "))
  ),
  
  tabPanel("Home",
           h2("Welcome to Whalebase"),
           p("This is the content for the Cape May Whale Watch and Research Center's digital Humpback whale catalogue,
             created by Daniel Iovino."),
           br(),
           h3("The Whalebase app consists of three main tabs:"),
           br(),
           h4("  - Catalogued Individuals - A complete digital and searchable version
              of the CMWWRC's humpback whale catalog"),
           br(),
           h4("  - Data Exploration - A display of the number of new and resighted whales
              observed by the CMWWRC annually"),
           br(),
           h4("  - Map - An interactive map of the surrounding region off Cape May,
              that displays each whale sighting by month"),
           br(), 
           
           br(),
           
           div(
             style = "text-align: center;",
             img(src = "https://capemaywhalewatch.com/wp-content/uploads/sites/6160/2023/01/Remove.jpeg?w=700&h=700&zoom=2",
                 height = "300px", style = "margin-right:10px;"),
             
             br()
             
           )
  ),
  
  tabPanel("Catalogued Individuals",
           h2("Meet the whales!"),
           p("This page displays all catalogued humpback whales observed by
           the Cape May Whale Watch and Research Center between 2011 and 2024"),
           textInput("search", "Search by ID, traits, or date"),
           uiOutput("catalog_results")
  ),
  
  tabPanel("Data Exploration",
           h2("Catalogued Humpback Whale Trends"),
           h4("New Individuals per Year"),
           plotOutput("annualPlot", height = "400px"),
           p("Figure 1:  238 catalogued M. novaeangliae individuals by year between 2011-2023 off Cape May, New Jersey"),
           
           br(),
           
           h4("Resightings per Year"),
           plotOutput("resightingsPlot", height = "400px"),
           p("Figure 2: 50 instances of resighting individuals for catalogued M. novaeangliae individuals between 2011-2023 off Cape May, New Jersey ")
  ),
  
  tabPanel("Map",
           h2("Observe specific sightings by Whale or by Date"),
           checkboxGroupInput("selected_months", "Select Months:",
                              choices = month_choices,
                              selected = month_choices,
                              inline = TRUE),
           leafletOutput("whaleMap", height = 600)
  ),
  
  tabPanel("About",
           h2("About This App"),
           p("Whalebase was created as a teaching and training tool for the
             Cape May Whale Watch and Research Center, as part of a practicum
             project for the Stockton University DSSA program."),
           br(),
           p("Acknowledgments: I would like to thank my professors
              Melissa Laurino and Clifton Baldwin 
              for all of their guidance and support,
              as well as Captain Matt Remmuzi, the crew,
              and the dedicated team of research interns at the
              Cape May Whale Watch and Research Center.")
  )
)


# Define Server
server <- function(input, output, session) {
  output$catalog_results <- renderUI({
    query <- tolower(input$search)
    
    if (query == "") {
      return(all_whales)  # Show all if nothing is typed
    }
    
    matches <- Filter(function(row_ui) {
      row_text <- paste(capture.output(print(row_ui)), collapse = " ")
      grepl(query, tolower(row_text), fixed = TRUE)
    }, all_whales)
    
    if (length(matches) == 0) {
      return(tags$p("No matches found."))
    }
    
    return(matches)
  })
  pal <- colorFactor(
    palette = "Set3",      
    domain = Whale_Coords$Month
  )
  
  filteredData <- reactive({
    req(input$selected_months)  
    Whale_Coords %>% filter(Month %in% input$selected_months)
  })
  
  
  output$whaleMap <- renderLeaflet({
    leaflet(data = filteredData()) %>%
      addTiles() %>%
      addScaleBar(position = "bottomleft") %>%
      addCircleMarkers(
        lng = ~long,
        lat = ~lat,
        radius = 1.5,
        stroke = FALSE,
        fillOpacity = 0.7,
        color = ~pal(Month),
        popup = ~paste("CMMWRC ID:", WhaleID, "<br>Date:", CleanDate, "<br>Month:", Month)
      ) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~Month,
        title = "Month"
      )
  })
  output$annualPlot <- renderPlot({
    # Convert Date
    Humpback$Date <- as.Date(Humpback$Date, format = "%m/%d/%Y")
    
    # Extract Year
    Humpback$year <- format(Humpback$Date, "%Y")
    
    # First sighting per whale
    first_sighting <- Humpback %>%
      group_by(Catologued.Animals) %>%
      summarize(first_year = min(year, na.rm = TRUE))
    
    # Count new individuals per year
    annual_firsts <- first_sighting %>%
      group_by(first_year) %>%
      summarize(new_individuals = n())
    
    # Remove 2024 (or latest year)
    annual_firsts <- annual_firsts %>%
      filter(first_year != max(first_year))
    
    # Plot
    ggplot(annual_firsts, aes(x = first_year, y = new_individuals)) +
      geom_col(fill = "steelblue") +
      labs(title = "New Catalogued Humpback Whales Recognized Per Year off of Cape May, New Jersey",
           x = "Year", y = "Number of New Whales") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$resightingsPlot <- renderPlot({
    # Join to get first sighting per individual
    first_sighting <- Humpback %>%
      group_by(Catologued.Animals) %>%
      summarize(first_year = min(format(as.Date(Date, "%m/%d/%Y"), "%Y"), na.rm = TRUE))
    
    # Add year to full dataset
    Humpback$year <- format(as.Date(Humpback$Date, format = "%m/%d/%Y"), "%Y")
    
    # Merge to get first_year
    Humpback_with_first <- Humpback %>%
      left_join(first_sighting, by = "Catologued.Animals")
    
    # Convert year columns to numeric for comparison
    Humpback_with_first$year <- as.numeric(Humpback_with_first$year)
    Humpback_with_first$first_year <- as.numeric(Humpback_with_first$first_year)
    
    # Filter to resightings (year > first_year)
    resightings <- Humpback_with_first %>%
      filter(year > first_year)
    
    # Count distinct individuals resighted each year
    resightings_per_year <- resightings %>%
      group_by(year) %>%
      summarize(resighted_whales = n_distinct(Catologued.Animals)) %>%
      filter(year != 2024)
    
    # Plot
    ggplot(resightings_per_year, aes(x = year, y = resighted_whales)) +
      geom_col(fill = "steelblue") +
      labs(title = "Number of Resightings of Catalogued Humpback Whales Per Year off Cape May, NJ",
           x = "Year", y = "Number of Resighted Whales") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
