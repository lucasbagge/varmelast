# LEARNING LAB 46: MODELTIME PANEL DATA & ENSEMBLES ----
# BONUS - SHINY APP - FORECAST MANY TIME SERIES

# LIBRARIES ----

library(lubridate)
library(magrittr)
library(tibble)
library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(plotly)


# Plotting
library(plotly)
library(reactable)
library(highcharter)

# Shiny
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(fresh)
library(shinyjs)

# Modeling
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
library(modeltime.resample)
library(ranger)


# Data
library(tidyquant)

# Time Series
library(timetk)
library(lubridate)
library(anomalize)

# Core
library(tidyverse)

# Setup & Functions
source("auto_forecast/setup.R")
source("auto_forecast/data_functions.R")
source("auto_forecast/spotify_theme.R")
source("auto_forecast/color_theme.R")


# ---- 1.0 UI ----
ui <- dashboardPage(
  title = "Time series analysis",
  skin  = "black-light",
  
  options = list(
    sidebarExpandOnHover = FALSE
  ),
  # 1.1 Header ----
  header = dashboardHeader(
    # title = "Nostradamus",
    title = tagList(
      span(class = "logo-lg", "Time series analysis")
    ),
    leftUi = tagList(
    )
  ),
  # 1.2 Sidebar ----
  sidebar = dashboardSidebar(
    id = "sidebar_1",
    minified   = TRUE,
    collapsed  = FALSE,
    sidebarMenu(
      menuItem(
        text       = "Auto Forecast",
        tabName    = "auto_forecast",
        #badgeLabel = "modeltime",
        badgeColor = "light-blue",
        icon       = icon("cogs")
      ),
      menuItem(
        text       = "TS Explorer",
        tabName    = "data_explorer",
        #badgeLabel = "timetk",
        badgeColor = "red",
        icon       = icon("search")
      )
    ),
    hr(),
    div(
      class = "text-center",
      p(HTML("<strong>Learn Shiny & Time Series</strong><br>with Lucas Bagge"),
        class="text-light-blue hide-sidebar-collapse")
    )
  ),
  # 1.3 Control Bar ----
  controlbar = dashboardControlbar(
    disable = TRUE
  ),
  # 1.4 Dashboard Body ----
  body = dashboardBody(
    setShadow(class = "dropdown-menu"),
    setShadow(class = "box"),
    use_theme(my_theme),
    useShinyjs(),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )
    ),
    
    tabItems(
      # * Auto Forecast ----
      tabItem(
        tabName = "auto_forecast",
        h1("Auto Forecast"),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = "test22",
              "Time granularity",
              selected = "Day",
              choices = c("Hour", "Day", "Week", "Month", "Year")
            )
          ),
          column(
            width = 3,
            dateRangeInput("daterange1",
                           "Date range:",
                           start = today() - 10,
                           end   = today())
            
          ),
          box(
            width = 12,
            
            column(
              width = 12,
              plotOutput("box_forecast_inputs")
            )
          ),
          column(
            width = 8,
            uiOutput("box_ensemble_plot")
          )
        )
      ),
      # * TS Explorer ----
      tabItem(
        tabName = "data_explorer",
        h1("TS Explorer"),
        fluidRow(
          column(
            width = 12,
            uiOutput("ts_summary")
          ),
          column(
            width = 6,
            uiOutput("box_time_plot")
          ),
          column(
            width = 6,
            uiOutput("box_time_plot2")
          )
        )
      )
    )
  ),
  # 1.5 Footer ----
  footer = dashboardFooter(
    left = p("Created to analysis time series "),
    right = "2021"
  )
)

# ---- 2.0 SERVER ----
server <- function(session, input, output) {
  
  varme_last <- function(from = "",  to = "") {
    
    resp <- GET(
      paste0('https://www.varmelast.dk/api/v1/heatdata/historical?from=',
             from,'&to=',to,'&intervalMinutes=60&contextSite=varmelast_dk')
    )
    content <- fromJSON(content(resp, 'text'))
    
    date <- content$times$timestamp
    
    df_list <- content$times$values
    
    date_tibble <- map_df(date, as_tibble)
    
    df_tibble <- map_df(df_list, as_tibble)
    
    df_wide <-
      df_tibble %>%
      select(-valueError) %>%
      pivot_wider(names_from =  key,
                  values_from = value) %>%
      unnest()
    
    df <-
      df_wide %>% cbind(date_tibble) %>%
      mutate(date = ymd_hms(value)) %>%
      janitor::clean_names() %>%
      select(-value) %>%
      rename(
        Affaldsenergianlaeg = be_vl_affald_ef,
        Kraftvarmeanlaeg = be_vl_kraftv_ef,
        'Spidslast gas' = be_vl_spids_gas_ef,
        'Spidslast olie' = be_vl_spids_olie_ef,
        'Spidslast træpiller' = be_vl_bio_ef,
        'CO2 - Udledning' = be_vl_total_fak,
        'Lokal produktion' = local
      )
    df %>%
      as_tibble() %>%
      janitor::clean_names() %>%
      pivot_longer(cols = -c(date),
                   names_to = "metric",
                   values_to = "value") %>%
      group_by(date = date, metric) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup()
  }
  
  data <- reactive({
    
    df <- varme_last(today() - 50, today()) %>%
      filter(metric %in% c("affaldsenergianlaeg",
                           "lokal_produktion",
                           "kraftvarmeanlaeg",
                           "spidslast_gas",
                           "spidslast_olie",
                           "spidslast_traepiller") ) %>%
      pivot_wider(
        names_from = metric,
        values_from = value) %>%
      mutate(spidslast =
               (spidslast_gas + spidslast_olie + spidslast_traepiller) ) %>%
      select(date,
             affaldsenergianlaeg,
             lokal_produktion,
             kraftvarmeanlaeg,
             spidslast) %>%
      pivot_longer(-date) %>%
      mutate(name = factor(name,
                           levels = c("spidslast",
                                      "kraftvarmeanlaeg",
                                      "lokal_produktion",
                                      "affaldsenergianlaeg")))
    
    df_1 <-
      df %>%
      pivot_wider(
        names_from = "name",
        values_from = "value") %>%
      mutate(
        Hour  = as.character(date),
        Day   = format(date, "%Y-%m-%d"),
        Week  = format(date, "%Y-%U"),
        Month = format(date, "%Y-%m"),
        Year  = format(date, "%Y" )
      )
    df_1
  })
  
  output$box_forecast_inputs <- renderPlot({
    req(input$test22)
    
    test <-
      data() %>%
      group_by_(input$test22) %>%
      summarise(
        across(where(is.numeric), ~ mean(., na.rm = TRUE))
      )    %>%
      rename(
        'Peak load' = spidslast,
        'Local produktion' = lokal_produktion,
        'Incineration' = affaldsenergianlaeg
      )   %>%
      pivot_longer(-input$test22)   %>%
      mutate(name = factor(name,
                           levels = c(
                             "kraftvarmeanlaeg",
                             "Peak load",
                             "Local produktion",
                             "Incineration")),
             value = ceiling(value)
      ) %>%
      drop_na()
    
    ggplot(data = test,
           aes_string(x = input$test22,
                      y = "value",
                      fill = "name"))  +
      geom_bar(stat="identity") +
      labs(title = "Production (MJ/s)",
           subtitle = "Heat flow in CPH area",
           caption = "Data source: Varmelast and own data",
           y ="",
           x = "") +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
      scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99", "blue")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0,
                                      size = 15,
                                      face = "bold"
      ),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(hjust = 0),
      legend.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.major.y = element_line(
        size = 0.5,
        linetype = 1))
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
