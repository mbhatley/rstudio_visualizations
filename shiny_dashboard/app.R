# Module 8 Exercise 1 file

# loading the required packages to make the plots and manipulate the data frames
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(colorspace)
library(purrr)
library(RColorBrewer)
library(plotly)

# any code here will run only once. 
# load in the data sets
states <- read.csv("states.csv")
bigfoot <- read.csv("bfro_reports_geocoded.csv")
ufo <- read.csv("complete.csv")
military <- read.csv("military_data.csv")

states <- states %>%
  dplyr::mutate(State = as.character(State),
                Region = as.character(Region),
                Abbreviation = as.character(Abbreviation))

military <- military %>%
  dplyr::filter(state != "Hawaii" & state != "Alaska") %>%
  dplyr::filter(!is.na(lat) & lat != "") %>%
  dplyr::filter(!is.na(long & long != "")) %>%
  dplyr::mutate(state = tolower(state),
                branch = tolower(branch))


# create the regional map info (for the FAQ page)
states_map <- map_data("state")
states_mapping <- states %>%
  select(-c("lat", "long"))
states_mapping$State <- tolower(states_mapping$State)
smap_data <- left_join(states_map, states_mapping, by = c("region" = "State"))

# the code segment below aligns the state abbreviations over the states
state_centroids <- smap_data %>%
  group_by(region, Abbreviation) %>%
  dplyr::summarise(
    long = mean(range(long)),
    lat = mean(range(lat))
  ) %>%
  ungroup()

# this function was copied from an 8100 project last year, and makes text black or white depending on the background color
get_text_color <- function(background_color) {
  # Convert background color to LAB color space
  lab_color <- as(hex2RGB(background_color), "LAB")
  
  # Calculate perceived lightness
  L <- lab_color@coords[, "L"]
  
  # Determine text color based on perceived lightness
  ifelse(L < 40, "black", "white")
}
get_luminance_vector <- function(colors) {
  rgb_values <- t(col2rgb(colors))
  luminance <- 0.2126 * rgb_values[, 1] + 0.7152 * rgb_values[, 2] + 0.0722 * rgb_values[, 3]
  return(luminance)
}

unique_regions <- unique(states$Region)
region_colors <- brewer.pal(length(unique_regions), "Paired")
names(region_colors) <- unique_regions

name_data <- states %>%
  dplyr::group_by(Abbreviation, lat, long, Region) %>%
  dplyr::filter(!Abbreviation %in% c('HI', 'AK')) %>%
  dplyr::mutate(text_color = get_text_color(region_colors[Region]))

# Bigfoot Dataset
bigfoot <- bigfoot %>%
  dplyr::select(c(state, county, latitude, longitude, date)) %>%
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::filter(!is.na(date) | date == "") %>%
  dplyr::mutate(date = mdy(date),
                month = month(date, label = TRUE, abbr = TRUE),
                year = year(date),
                decade = year - year %% 10,
                latitude = as.numeric(latitude),
                longitude = as.numeric(longitude),
                state = as.character(state),
                county = stringr::str_remove(county, " County$"),
                county = gsub("\\.", "", as.character(county)),
                county = ifelse(county == "NA", NA, county),
                county = gsub("\\'", "", county),
                county = gsub(" Parish", "", county, ignore.case = TRUE),
                set = "bigfoot",
                season = case_when(
                  month %in% c("Mar", "Apr", "May") ~ "Spring",
                  month %in% c("Jun", "Jul", "Aug") ~ "Summer",
                  month %in% c("Sep", "Oct", "Nov") ~ "Fall",
                  month %in% c("Dec", "Jan", "Feb") ~ "Winter")
  )  %>%
  left_join(states, by = c("state" = "State")) %>%
  dplyr::mutate(mapid = paste(county, Abbreviation, sep=", ")) %>%
  dplyr::select(-c("lat", "long", "Abbreviation")) %>%
  dplyr::filter(year >= 1950 & year < 2020) %>%
  dplyr::rename(zone = Region) %>%
  dplyr::rename(region = state) %>%
  dplyr::filter(region != "Hawaii" & region != "Alaska") %>%
  dplyr::filter(!is.na(region) & region != "")

# UFO Dataset
ufo <- ufo %>%
  dplyr::filter(country == "us" & !is.na(state) & state != "")  %>%
  dplyr::select(c(datetime, state, county, shape, duration..seconds., latitude, longitude)) %>%
  dplyr::rename(duration = duration..seconds.) %>%
  dplyr::rename(date = datetime) %>%
  dplyr::mutate(state = toupper(state),
                date = as.Date(date, format = '%d/%m/%Y'),
                month = month(date, label = TRUE, abbr = TRUE),
                year = year(date),
                decade = year - year %% 10,
                latitude = as.numeric(latitude),
                longitude = as.numeric(longitude),
                duration = as.numeric(duration),
                shape = as.character(shape),
                county = gsub("\\.", "", as.character(county)),
                county = gsub("\\'", "", county),
                county = gsub(" Parish", "", county, ignore.case = TRUE),
                county = ifelse(county == "NA", NA, county),
                mapid = paste(county, state, sep = ", "),
                set = "ufo",
                season = case_when(
                  month %in% c("Mar", "Apr", "May") ~ "Spring",
                  month %in% c("Jun", "Jul", "Aug") ~ "Summer",
                  month %in% c("Sep", "Oct", "Nov") ~ "Fall",
                  month %in% c("Dec", "Jan", "Feb") ~ "Winter")
  ) %>%
  left_join(states, by = c("state" = "Abbreviation"))  %>%
  select(-c("lat", "long", "state")) %>%
  dplyr::filter(year >= 1950 & year < 2020) %>%
  dplyr::filter(duration != "") %>%
  dplyr::rename(region = State) %>%
  dplyr::rename(zone = Region) %>%
  dplyr::filter(region != "Hawaii" & region != "Alaska") %>%
  dplyr::filter(!is.na(region) & region != "")

# Data Frames for Plots
# state level data
bigfoot_count_decade <- bigfoot %>%
  dplyr::group_by(region, zone, month, season, mapid, decade, set) %>%
  dplyr::summarise(count = n())

ufo_count_decade <- ufo %>%
  dplyr::group_by(region, zone, month, season, mapid, decade, set) %>%
  dplyr::summarise(count = n())

total_count <- rbind(bigfoot_count_decade, ufo_count_decade) %>%
  ungroup %>%
  dplyr::mutate(region = tolower(region),
                zone = tolower(zone))

# lat / long data
bf_latlong <- bigfoot %>%
  dplyr::select(c(region, latitude, longitude, decade, set))

ufo_latlong <- ufo %>%
  dplyr::select(c(region, latitude, longitude, decade, set))

latlong_data <- rbind(bf_latlong, ufo_latlong) %>% 
  dplyr::mutate(region = tolower(region))

# ufo specific modification
# this reduces and better groups the shapes
ufo_types <- list(
  light = c("light", "flare", "flash", "fireball"),
  triangle = c("triangle", "delta", "pyramid", "chevron"),
  round = c("circle", "disk", "round", "sphere", "oval", "egg", "teardrop", "crescent"),
  cylinder = c("cigar", "cylinder", "cone"),
  other = c("unknown", "other", " ", "cross"),
  formation = c("formation"),
  rectangle = c("rectangle", "diamond", "hexagon"),
  changing = c("changed", "changing")
)

find_shape <- function(shape, ufo_types) {
  shape <- tolower(shape)
  
  if (is.na(shape) || shape == "" || shape == " ") {
    return("other")
  }
  
  matching_category <- names(ufo_types)[map_lgl(ufo_types, ~ shape %in% .x)]
}

ufo <- ufo %>%
  mutate(shape = map_chr(shape, ~ find_shape(.x, ufo_types)))


unique_states <- unique(total_count$region)
unique_zones <- tolower(unique(states$Region))
unique_branch <- unique(military$branch)

# UI 
ui <- dashboardPage(
  dashboardHeader(title = "Paranormal Sightings 1950 to 2020", titleWidth = 400),
  dashboardSidebar(disable = FALSE, width = 200,
                   sidebarMenu(
                     id = 'sidebar',
                     style = "position: relative; overflow: visible;",
                     menuItem("Dashboard", tabName = 'dashboard', icon = icon('dashboard')),
                     menuItem("Datasets", tabName = 'datasets', icon = icon('list'),
                              menuSubItem("Sasquatch Dataset", tabName = 'bfdata', icon = icon('paw')),
                              menuSubItem("UFO Dataset", tabName = 'ufodata', icon = icon('reddit-alien')),
                              menuSubItem("US Military Bases Dataset", tabName = 'usm', icon = icon('flag'))),
                     menuItem("FAQs", tabName = 'faqs', icon = icon('question-circle'))
                   )),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'dashboard',
              fluidRow(
                column(width = 4,  
                       wellPanel(
                         selectInput("data_select", 
                                     "Select Dataset:", 
                                     choices = c("Select a dataset" = "NULL", 
                                                 "Bigfoot" = "bigfoot", 
                                                 "UFO" = "ufo"), selected = "NULL"),
                         sliderInput("decade_slider", "Select Decade", min = 1950, max = 2010, 
                                     step = 10, value = 1950, 
                                     sep = "", animate = animationOptions(
                                       interval = 4000, loop = TRUE, playButton = "Play",
                                       pauseButton = "Pause")),
                         p("Please select a State view more detailed data"),
                         selectInput("state_select", "Select State:", 
                                     choices = c("Select a state" = "", sort(as.character(unique_states))),
                                     selected = ""),
                         # selectInput("zone_select", "Select Zone:",
                         #             choices = c("Select a zone" = "", sort(as.character(unique_zones))),
                         #             selected = ""),
                         radioButtons("base_select", "Select Service Branch Base",
                                      choices = c("All Branches" = "", unique_branch), 
                                      selected = "", inline = TRUE, width = "100%")
                       )
                ),
                column(width = 8,
                       plotOutput("state_map", height = "450px")
                )
              ),
              br(),
              fluidRow(
                column(width = 6,
                       plotOutput("top_states", height = "400px")),
                column(width = 6,
                       plotOutput("top_county", height = "400px"))
              ),
              br(),
              fluidRow(
                column(width = 6,
                       plotOutput("top_month", height = "400px")),
                column(width = 6,
                       plotOutput("top_season", height = "400px"))
              ),
              br(),
              fluidRow(
                column(width = 6,
                       plotOutput("ufo_shapes", height = "400px")),
                column(width = 6,
                       plotOutput("ufo_shapes_count", height = "400px"))
              )
      ),
      tabItem(tabName = 'bfdata',
              h2("Sasquatch Dataset"),
              br(),
              fluidRow(
                column(width = 12,
                       dataTableOutput("bigfoot_table")
                )
              )
      ),
      tabItem(tabName = 'ufodata',
              h2("UFO Dataset"),
              br(),
              fluidRow(
                column(width = 12,
                       dataTableOutput("ufo_table")
                )
              )
      ),
      tabItem(tabName = 'usm',
              h2("US Military Dataset"),
              br(),
              fluidRow(
                column(width = 12,
                       dataTableOutput("bases_tables"))
              )
      ),
      tabItem(tabName = 'faqs',
              h2("FAQs"),
              p("All answers displayed here are excerpts taken from Wikipedia."),
              br(),
              h4("What is a Bigfoot?"), 
              p("Bigfoot, also commonly referred to as Sasquatch, Skunk Ape, Almas, Yeren, Yeti or Yowie, is a large, hairy mythical 
                creature said to inhabit forests in North America, particularly in the Pacific Northwest. Bigfoot is often described as a large, 
                muscular, and bipedal human or ape-like creature covered in black, dark brown, or dark reddish hair. 
                Anecdotal descriptions estimate a height of roughly 6–9 feet."),
              br(),
              h4("What are UAP's / UFO's?"),
              p("An unidentified flying object (UFO), or unidentified anomalous phenomenon (UAP), is any perceived airborne, submerged or 
                transmedium phenomenon that cannot be immediately identified or explained. While unusual sightings have been reported in the sky throughout history, 
                UFOs became culturally prominent after World War II, escalating during the Space Age. While technically a UFO refers to any unidentified flying object, 
                in modern popular culture the term UFO has generally become synonymous with alien spacecraft. The term 'extra-terrestrial vehicle' (ETV) 
                is sometimes used to separate this explanation of UFOs from totally earthbound explanations."),
              br(),
              h4("Can you explain how you reduced the UFO shapes down in your dataset?"),
              p("light: light, flare, flash, fireball"),
              p("triangle: triangle, delta, pyramid, chevron"),
              p("round: circle, disk, round, sphere, oval, egg, teardrop, crescent"),
              p("cylinder: cigar, cylinder, cone"),
              p("other: unknown, other, cross"),
              p("formation: formation"),
              p("rectangle: rectangle, diamond, hexagon"),
              p("changing: changed, changing"),
              br(),
              h4("How do I interpret the decades on the plots?"),
              p("The decade indicated on the plots is the start of the decade; i.e. 1950 covers the years 1950 to 1959, and so forth. This dashboard excludes sightings 
                and reports from before 1950 and after 2019."),
              br(),
              h4("Where can I find the datasets?"),
              p("The datasets are all available on Kaggle. The links to each dataset are listed below:"),
              p(tags$a(href = "https://www.kaggle.com/datasets/thedevastator/unlocking-mysteries-of-bigfoot-through-sightings", 
                       "Bigfoot Dataset", target = "_blank")),
              p(tags$a(href = "https://www.kaggle.com/datasets/NUFORC/ufo-sightings/data", 
                       "UFO Dataset", target = "_blank")),
              p(tags$a(href = "https://www.kaggle.com/datasets/mexwell/us-military-bases", 
                       "US Military Bases", target = "_blank")),
              br(),
              h4("How do you define the regions for the United States?"),
              plotOutput("region_map", height = "500px")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Get map data
  county <- map_data("county")
  states <- map_data("state")
  
  # Initialize reactive values
  reac <- reactiveValues(decade = 1950, is_playing = FALSE, set = NULL)
  
  # Update reactive values
  observeEvent(input$data_select, {
    reac$set <- input$data_select
  })
  
  observeEvent(input$decade_slider, {
    reac$decade <- input$decade_slider
  })
  
  # Update is_playing status on start/stop
  observeEvent(input$slider_animate, {
    reac$is_playing <- input$slider_animate
  })
  
  observeEvent(input$state_select, {
    reac$state <- input$state_select
    # reac$column <- as.character("region")
  })
  
  observeEvent(input$zone_select, {
    reac$zone <- input$zone_select
    # reac$column <- as.character("zone")
  })
  
  observeEvent(input$base_select, {
    reac$base <- input$base_select
  })
  
  # Reactive dataset based on paranormal type user selection
  filtered_map_state <- reactive({
    if (reac$state == "") {
      return(states)
    }
    states %>%
      dplyr::filter(region == reac$state)
  })
  
  filtered_map_county <- reactive({
    if (reac$state == "") {
      return(county)
    }
    county %>%
      dplyr::filter(region == reac$state)
  })
  
  filtered_military_map <- reactive({
    if (reac$state == "" & reac$base == "") {
      return(military)
    }
    else if (reac$state != "" & reac$base == "") {
      return(military %>%
               dplyr::filter(state == reac$state))
    }
    else if (reac$state != "" & reac$base != "") {
      return(military %>%
               dplyr::filter(state == reac$state) %>%
               dplyr::filter(branch == reac$base))
    }
    else if (reac$state == "" & reac$base != "") {
      return(military %>%
               dplyr::filter(branch == reac$base))
    }
  })
  
  filtered_total_data <- reactive({
    if (reac$set == "NULL") {
      return(total_count %>%
               dplyr::filter(decade == reac$decade) %>%
               dplyr::group_by(region) %>%
               dplyr::summarise(count = as.integer(sum(count)))
      )}
    total_count %>%
      dplyr::filter(set == reac$set) %>%
      dplyr::filter(decade == reac$decade) %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(count = as.integer(sum(count)))
  })
  
  filtered_state_data <- reactive({
    if (reac$state == "" & reac$set == "NULL") {
      return(total_count %>%
               dplyr::filter(decade == reac$decade)
      )} else if (reac$state == "" & reac$set != "NULL") {
        return(total_count %>%
                 dplyr::filter(decade == reac$decade) %>%
                 dplyr::filter(set == reac$set)
        )} else if (reac$state != "" & reac$set == "NULL") {
          return(total_count %>%
                   dplyr::filter(decade == reac$decade) %>%
                   dplyr::filter(region == reac$state)
          )}
    total_count %>%
      dplyr::filter(region == reac$state) %>%
      dplyr::filter(decade == reac$decade) %>%
      dplyr::filter(set == reac$set)
  })
  
  filtered_zone_data <- reactive({
    if (reac$zone == "" & reac$set == "NULL") {
      return(total_count %>%
               dplyr::filter(decade == reac$decade)
      )} else if (reac$zone == "" & reac$set != "NULL") {
        return(total_count %>%
                 dplyr::filter(decade == reac$decade) %>%
                 dplyr::filter(set == reac$set)
        )} else if (reac$zone != "" & reac$set == "NULL") {
          return(total_count %>%
                   dplyr::filter(decade == reac$decade) %>%
                   dplyr::filter(zone == reac$zone)
          )}
    total_count %>%
      dplyr::filter(zone == reac$zone) %>%
      dplyr::filter(decade == reac$decade) %>%
      dplyr::filter(set == reac$set)
  })
  
  filtered_ufoshapes_data <- reactive({
    if (reac$set == "NULL" || reac$set == "bigfoot") {
      return(NULL)
    }
    else if (reac$set == "ufo" & reac$state == "") {
      return(ufo %>%
               mutate(region = tolower(region)) %>%
               dplyr::filter(set == reac$set) %>%
               dplyr::filter(decade == reac$decade))
    }
    ufo %>%
      mutate(region = tolower(region)) %>%
      dplyr::filter(set == reac$set) %>%
      dplyr::filter(decade == reac$decade) %>%
      dplyr::filter(region == reac$state)
  })
  
  filtered_latlong_data <- reactive({
    if (reac$state == "" & reac$set == "NULL") {
      return(latlong_data %>%
               dplyr::filter(!is.na(latitude) & latitude != "") %>%
               dplyr::filter(!is.na(longitude & longitude != "")) %>%
               dplyr::filter(decade == reac$decade) %>%
               dplyr::filter(latitude >= 24.3964 & latitude <= 49.3844  &
                               longitude >= -124.848 & longitude <= -66.886))
    }  
    else if (reac$state == "" & reac$set != "NULL") {
      return(latlong_data  %>%
               dplyr::filter(!is.na(latitude) & latitude != "") %>%
               dplyr::filter(!is.na(longitude & longitude != "")) %>%
               dplyr::filter(set == reac$set) %>%
               dplyr::filter(decade == reac$decade) %>%
               dplyr::filter(latitude >= 24.3964 & latitude <= 49.3844  &
                               longitude >= -124.848 & longitude <= -66.886))
    } 
    else if (reac$state != "" & reac$set == "NULL") {
      return(latlong_data  %>%
               dplyr::filter(!is.na(latitude) & latitude != "") %>%
               dplyr::filter(!is.na(longitude & longitude != "")) %>%
               dplyr::filter(decade == reac$decade) %>%
               dplyr::filter(region == reac$state))
    }
    else if (reac$state != "" & reac$set != "NULL") {
      return(latlong_data  %>%
               dplyr::filter(!is.na(latitude) & latitude != "") %>%
               dplyr::filter(!is.na(longitude & longitude != "")) %>%
               dplyr::filter(set == reac$set) %>%
               dplyr::filter(decade == reac$decade) %>%
               dplyr::filter(region == reac$state))
    }
  })
  
  # Render plots
  output$state_map <- renderPlot({
    dmap_data <- filtered_state_data() %>%
      dplyr::group_by(region) %>%
      dplyr::summarise(count = n()) %>%
      mutate(hover_text = paste("State:", region,
                                "<br>Sightings:", count))
    dmap_latlong <- filtered_latlong_data()
    dmap_mil <- filtered_military_map()
    dmap_states <- filtered_map_state()
    dmap_county <- filtered_map_county()
    
    ggplot() +
      geom_map(data = dmap_data, map = dmap_states, aes(map_id = region, fill = count), alpha = 0.7, color = "white", size = 0.25) +
      geom_map(data = dmap_states, map = dmap_states, aes(map_id = region), fill = NA, color = "black", size = 0.3) +
      geom_map(data = dmap_states, map=dmap_states, aes(map_id=region), color="black", fill=NA, size=0.3) +
      geom_map(data = dmap_county, map=dmap_county, aes(map_id=region), color="grey40", fill=NA, size=0.1) +
      geom_point(data = dmap_mil, aes(x = long, y = lat), color = "purple", size=3) +
      geom_point(data = dmap_latlong, aes(x = longitude, y = latitude, color = set), size=3) +
      scale_color_manual(values = c("ufo" = "orange", "bigfoot" = "red")) +
      expand_limits(x = dmap_states$long, y = dmap_states$lat) +
      scale_fill_distiller(palette = "Blues", direction = 1, name = "Count", na.value = "gray90") +
      coord_map("mercator") +
      theme_minimal() +
      labs(title = paste("Paranormal Sightings by State during the", reac$decade, "'s"),
           subtitle = paste(reac$set, "data; select state: ", reac$state, "Military bases are indicated by purple dots")) +
      theme(legend.position = "left",
            panel.grid = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14, face = "bold"))
  })
  
  output$top_states <- renderPlot({
    dmap_data <- filtered_total_data() %>%
      dplyr::arrange(desc(count)) %>%
      head(10) %>%
      dplyr::mutate(region = forcats::fct_reorder(region, count, .desc = FALSE))
    
    
    ggplot(data = dmap_data, aes(x = count, y = region, fill = region)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Paired")+
      scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      theme_minimal() +
      labs(
        x = "Number of Sightings", y = "", 
        title = paste("Top 10 States by Sightings during the", reac$decade, "'s"),
        subtitle = paste(reac$set, "data")
      ) +
      theme(legend.position = "none",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$top_county <- renderPlot({
    dmap_data <- filtered_state_data() %>%
      dplyr::group_by(mapid) %>%
      dplyr::summarise(count = as.integer(sum(count))) %>%
      dplyr::arrange(desc(count)) %>%
      head(10) %>%
      dplyr::mutate(mapid = forcats::fct_reorder(mapid, count, .desc = FALSE))
    
    
    ggplot(data = dmap_data, aes(x = count, y = mapid, fill = mapid)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Paired")+
      scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      theme_minimal() +
      labs(
        x = "Number of Sightings", y = "", 
        title = paste("Top Counties by Sightings during the", reac$decade, "'s"),
        subtitle = paste(reac$set, "data; select state: ", reac$state)
      ) +
      theme(legend.position = "none",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$top_month <- renderPlot({
    dmap_data <- filtered_state_data() %>%
      dplyr::group_by(month, season) %>%
      dplyr::summarise(count = as.integer(sum(count))) %>%
      dplyr::mutate(month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar",
                                                     "Apr", "May", "Jun", "Jul",
                                                     "Aug", "Sep", "Oct", "Nov"))) %>%
      dplyr::arrange(month)
    
    ggplot(data = dmap_data) +
      geom_line(aes(x = month, y = count, color = season, group = season)) + 
      geom_point(aes(x = month, y = count, size = count, color = season), alpha=0.5) +
      scale_size_continuous(range = c(5, 30)) +
      scale_color_brewer(palette = "Dark2") +
      theme_minimal() +
      labs(
        y = "Number of Sightings", x = "",
        title = paste("Cycle Plot of Sighting for the", reac$decade, "'s"),
        subtitle = paste(reac$set, "data; select state: ", reac$state)
      ) +
      theme(legend.position = "none",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$top_season <- renderPlot({
    dmap_data <- filtered_state_data() %>%
      dplyr::group_by(month, season) %>%
      dplyr::summarise(count = as.integer(sum(count))) %>%
      dplyr::mutate(season = factor(season, levels = c( "Winter", "Spring", "Summer", "Fall"))) %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(count = as.integer(sum(count))) %>%
      dplyr::arrange(season)
    
    ggplot(data = dmap_data, aes(x = season, y = count, fill = season)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Dark2")+
      scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      theme_minimal() +
      labs(
        y = "Number of Sightings", x = "", 
        title = paste("Sighting per Season during the", reac$decade, "'s"),
        subtitle = paste(reac$set, "data; select state: ", reac$state)
      ) +
      theme(legend.position = "none",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  output$ufo_shapes <- renderPlot({
    dmap_data <- filtered_ufoshapes_data()
    
    if (is.null(dmap_data) || nrow(dmap_data) == 0) {
      ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Please select UFO data to view"), 
                  size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void()
    } else {
      dmap_data <- dmap_data %>%
        dplyr::filter(!is.na(duration)) %>%
        dplyr::filter(duration >= 10) %>%
        dplyr::filter(duration <= 3600) %>%
        dplyr::mutate(duration = ceiling(round(duration / 60, 2)))
      
      ggplot(data = dmap_data, aes(x = duration, y = shape, color = region)) +
        geom_jitter(aes(color = shape, size=5), alpha = 0.6) +
        scale_color_brewer(palette = "Dark2") +
        theme_minimal() +
        labs(
          x = "Duration (minutes)", 
          y = "UFO Shape", 
          title = paste("Duration of UFO Sightings by Craft Shape,", reac$decade, "'s"),
          subtitle = "Sighting limited to 1 hour or shorter"
        ) +
        theme(legend.position = "none", 
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14))
    }
  })
  
  output$ufo_shapes_count <- renderPlot({
    dmap_data <- filtered_ufoshapes_data()
    
    if (is.null(dmap_data) || nrow(dmap_data) == 0) {
      ggplot() + 
        geom_text(aes(x = 0.5, y = 0.5, label = "Please select UFO data to view"), 
                  size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void()
    } else {
      dmap_data <- dmap_data %>%
        dplyr::group_by(shape) %>%
        dplyr::summarise(count = n())
      
      ggplot(data = dmap_data, aes(x = count, y = shape, fill = shape)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Dark2")+
        scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
        theme_minimal() +
        labs(
          x = "Number of Sightings", y = "", 
          title = paste("Observations by Vehicle Shape, ", reac$decade, "'s"),
          subtitle = paste(reac$set, "data")
        ) +
        theme(legend.position = "none",
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14))
    }
  })
  
  ## Static Plots and Tables
  output$ufo_table <- renderDataTable({
    ufo
  }, options = list(pageLength = 10, scrollX = TRUE
  ))
  
  output$bigfoot_table <- renderDataTable({
    bigfoot
  }, options = list(pageLength = 10, scrollX = TRUE
  ))
  
  output$bases_tables <- renderDataTable({
    military
  }, options = list(pageLength = 10, scrollX = TRUE
  ))
  
  # FAQ page map plot - this is a static plot
  output$region_map <- renderPlot({
    ggplot(data = states_mapping) +
      geom_map(map = smap_data, aes(map_id=State, fill = Region), color="lightblue", linewidth=0.1) +
      geom_text(data = name_data, aes(x = long, y = lat, label = Abbreviation, color = I(text_color))) + 
      expand_limits(x = smap_data$long, y = smap_data$lat) +
      scale_fill_manual(values = region_colors) +
      coord_map("polyconic") +
      theme_void() +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold")
      ) +
      labs(fill = "Region") +
      theme(legend.position = "left")
  })
}

shinyApp(ui, server)