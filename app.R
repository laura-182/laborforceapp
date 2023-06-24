#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#----Libraries----

library(shiny)
library(tidyverse)
library(dygraphs)
library(lubridate)
library(leaflet)
library(shinyWidgets)
options(tigris_use_cache = TRUE)

source("global.R")

#----UI----


ui <- fluidPage(
  # Application title
  titlePanel("ICR Iowa Labor Force Metrics"),
  
  # Sidebar with a slider input for number of bins
  verticalLayout(
    sidebarLayout(
      sidebarPanel(
        width = 4,
        # https://shiny.rstudio.com/reference/shiny/0.14/dateRangeInput.html
        div(
          dateRangeInput(
            inputId = "daterange",
            label = "Select date range",
            format = "MM yyyy",
            start = "2019-01-01",
            end = mostRecent,
            min = leastRecent,
            max = mostRecent
          ),
          # https://stackoverflow.com/questions/69017707/how-to-adjust-margin-breaks-in-a-shiny-app
          tags$style("#daterange {margin-bottom: 0;}"),
        ),
        tags$div(style = "text-align:right; font-size: 0.8em;",
                 actionLink(inputId = "mostRecent", label = "Most Recent")),
        selectInput(
          inputId = "metric",
          label = "Select metric:",
          choices = c(
            "Labor Force" = "LF_ICRIowa",
            "Employment" = "Emp_ICRIowa",
            "Unemployment" = "Unemp_ICRIowa"
          ),
          
        ),
        fluidRow(
          column(
            4,
            checkboxGroupInput(
              inputId = "counties",
              label = "Select counties:",
              choices = c(
                "Benton",
                "Linn",
                "Jones",
                "Johnson",
                "Washington",
                "Cedar",
                "Iowa"
              ),
              selected = c("Benton", "Linn", "Jones")
            )
          ),
          column(
            8,
            # TO DO # this stuff overflows when the window is too narrow
            br(),
            br(),
            br(),
            actionButton("CRMSA", "Cedar Rapids, IA MSA"),
            br(),
            br(),
            actionButton("ICMSA", "Iowa City, IA MSA"),
            br(),
            br(),
            actionButton("ICR", "ICR Iowa")
          ),
        ),
        
        leafletOutput("map", width = "100%", height = "270px"),
        
        
        
      ),
      
      
      mainPanel(
        plotOutput("LFgraph"),
        br(),
        tabsetPanel(
          tabPanel("Summary",
                   br(),
                   textOutput("kpis")),
          tabPanel(
            "About the Data",
            br(),
            textOutput("text"),
            tableOutput("schedule")
          )
        )
      )
      
      
      
      # starts where side panel ends
    ),
    plotOutput("stackedbar")
  ),
  tableOutput("table")
)

#----Server----

server <- function(input, output, session) {
  ## Reactive Expressions ----
  
  # make variables for date range values reactive
  # https://lubridate.tidyverse.org/reference/round_date.html
  react_begOfMonth <-
    reactive({
      floor_date(input$daterange[1], "month")
    })
  react_endOfMonth <-
    reactive({
      ceiling_date(input$daterange[2], "month")
    })
  timeperiod <- 
    reactive({
      difftime(react_endOfMonth(), react_begOfMonth(), units = "days") %>% as.numeric()
    })
  
  axisIntervals <- 
    reactive({
      case_when(
        timeperiod() < 1800 ~ "6 months",
        timeperiod() < 3600 ~ "1 year",
        timeperiod() < 7200 ~ "2 years",
        TRUE ~ "5 years"
      )
    })
  
  # which metric is selected
  activeMetric <- reactive({
    get(input$metric) %>%
      select(metric_name) %>%
      distinct()
  })
  
  lineChart <- reactive({
    get(input$metric) %>%
      filter(date >= react_begOfMonth() &
               date <= react_endOfMonth()) %>%
      filter(county %in% input$counties) %>%
      group_by(date) %>%
      summarize(total = sum(value))
  })
  
  lastValue <- reactive({
    lineChart() %>%
      last() %>%
      select(total) %>%
      pull()
  })
  
  firstValue <- reactive({
    lineChart() %>%
      first() %>%
      select(total) %>%
      pull()
  })
  
  howItChanged <- reactive({
    case_when(
      lastValue() > firstValue() ~ "increased",
      lastValue() < firstValue() ~ "decreased"
    )
  })
  
  percChange <- reactive({
    scales::percent((lastValue()-firstValue())/firstValue(), accuracy = 0.1)
  })
  
  # https://shiny.rstudio.com/reference/shiny/0.14/updateCheckboxGroupInput.html
  # https://shiny.rstudio.com/articles/action-buttons.html
  
  # clicking CR MSA button will select respective counties
  observeEvent(input$CRMSA, {
    updateCheckboxGroupInput(session, "counties",
                             selected = c("Benton", "Linn", "Jones"))
  })
  
  # clicking IC MSA button will select respective counties
  observeEvent(input$ICMSA, {
    updateCheckboxGroupInput(session, "counties",
                             selected = c("Johnson", "Washington"))
  })
  
  # clicking ICR button will select all counties
  observeEvent(input$ICR, {
    updateCheckboxGroupInput(
      session,
      "counties",
      selected = c(
        "Johnson",
        "Washington",
        "Benton",
        "Linn",
        "Jones",
        "Iowa",
        "Cedar"
      )
    )
  })
  
  # https://shiny.rstudio.com/reference/shiny/0.14/updateDateRangeInput.html
  # clicking Most Recent link will set date to most recent end date
  observeEvent(input$mostRecent, {
    updateDateRangeInput(session,
                         "daterange",
                         end = mostRecent)
  })
  
  # https://stackoverflow.com/questions/31139791/checkboxgroupinput-set-minimum-and-maximum-number-of-selections-ticks
  # also want to add a pop-up message to alert user that they need to select at least one county
  # or I could program the charts to wait until something is selected before drawing
  observe({
    if (length(input$counties) < 1) {
      updateCheckboxGroupInput(session, "counties",
                               selected = c("Linn"))
    }
  })
  
  ## END of Reactive Expressions
  
  ## Chart ----
  
  output$LFgraph <- renderPlot({
    # the way I included reactive expression variables before the reactive expressions above
    # begOfMonth <- floor_date(input$daterange[1], "month")
    # endOfMonth <- ceiling_date(input$daterange[2], "month") - 1
    dataset <- get(input$metric)
    
    dataForStatSumm <-
      filter(dataset,
             date >= react_begOfMonth() & date <= react_endOfMonth()) %>%
      filter(date == max(date)) %>%
      filter(county %in% input$counties)
    
    ggplot(dataset %>%
             filter(county %in% input$counties),
           aes(x = date,
               y = value)) +
      # https://ggplot2tutor.com/tutorials/summary_statistics
      stat_summary(
        na.rm = TRUE,
        fun = "sum",
        geom = "line",
        linewidth = 2
      ) +
      # add a point at the end of the line
      stat_summary(
        na.rm = TRUE,
        fun = "sum",
        geom = "point",
        size = 5,
        data = dataForStatSumm
      ) +
      stat_summary(
        na.rm = TRUE,
        fun = "sum",
        geom = "label",
        aes(label = scales::comma(sum(value))),
        # https://stackoverflow.com/questions/56490813/how-do-i-add-a-comma-separator-to-a-text-label-in-geom-text
        size = 5,
        hjust = -0.2,
        data = dataForStatSumm
      ) +
      scale_x_date(
        limits = c(react_begOfMonth(), react_endOfMonth()-days(1)),
        date_breaks = axisIntervals(),
        date_labels = "%b %Y",
        expand = expansion(mult = c(0, 0.15))
      ) + # https://www.rdocumentation.org/packages/ggplot2/versions/3.3.6/topics/expansion
      scale_y_continuous(labels = scales::comma) +
      ggtitle(paste0(
        "Total ",
        unique(dataset$metric_name),
        " from ",
        format(react_begOfMonth(), "%b %Y"),
        " to ",
        format(react_endOfMonth()-days(1), "%b %Y")
      )) +
      xlab(NULL) +
      ylab(NULL) +
      theme(
        # http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
      )
    
    
  })
  
  # in between these two I have some space
  # may do a table
  # https://shiny.rstudio.com/reference/shiny/1.6.0/renderTable.html
  # or some callout metrics the percent growth
  
  
  output$data_highlights <- renderText({
    paste0(format(react_endOfMonth(), "%b %Y"), " ")
  })
  
  ## Stuff for Tabs ----
  
  output$text <- renderText({
    paste0(
      "Data is available back to January 1990. The data source is the Bureau of Labor Statistic's Local Area Unemployment Survey. ",
      nextReleaseSchedule,
      " "
    )
  })
  
  output$schedule <- renderTable({
    fullReleaseSchedule
  })
  
  output$kpis <- renderText({
    paste0(
      "From ",
      format(react_begOfMonth(), "%b %Y"),
      " to ",
      format(react_endOfMonth()-days(1), "%b %Y"),
      ", ",
      tolower(activeMetric()),
      " ",
      howItChanged(),
      " by ",
      percChange(),
      "."
    )
  })
  
  # Stacked Bar Chart ----
  
  output$stackedbar <- renderPlot({
    stackeddata <- ICRIowaData %>%
      select(-footnotes,-period,-year) %>%
      filter(!grepl("3$", series_id)) %>%
      mutate(metricname = case_when(
        grepl("6$", series_id) ~ "Labor Force",
        grepl("5$", series_id) ~ "Employment",
        grepl("4$", series_id) ~ "Unemployment"
      )) %>%
      mutate(date = as.Date(as.character(date))) # this might not be necessary # TO DO # test removing
    
    # This reverses the metrics in the chart so employment shows on the bottom.
    stackeddata$metricname <-
      as.factor(stackeddata$metricname) %>% fct_rev()
    
    # This is the first step in adjusting the y axes so it's easier to see the nuances in the data better
    y_axis_min <- stackeddata %>%
      filter(county %in% input$counties) %>%
      filter(date >= react_begOfMonth() &
               date <= react_endOfMonth()) %>%
      filter(metricname == "Employment") %>%
      group_by(date) %>%
      summarise(total = sum(value)) %>%
      ungroup() %>%
      select(total) %>%
      min()
    # This is the second step that takes 95% of the lowest value to add some buffer.
    lowestEmp <- y_axis_min * .95
    
    # TO DO # pretty this up more, add labels, switch colors
    ggplot(
      stackeddata %>%
        filter(!grepl("6$", series_id)) %>% # Filter out total labor force
        filter(county %in% input$counties),
      aes(x = date,
          y = value)
    ) +
      geom_col(aes(fill = metricname, group = metricname)) +
      # https://stackoverflow.com/questions/57359552/missing-only-one-bar-in-bar-chart
      scale_x_date(
        limits = c(react_begOfMonth() %m-% months(1), react_endOfMonth()),
        date_breaks = "6 months",
        date_labels = "%b %Y"
      ) +
      xlab(NULL) +
      ylab(NULL) +
      scale_y_continuous(labels = scales::comma) +
      coord_cartesian(ylim = c(lowestEmp, NA)) +
      theme(
        # http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
        plot.title = element_text(size = 24, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)
      )
    # https://www.cedricscherer.com/2021/07/05/a-quick-how-to-on-labelling-bar-graphs-in-ggplot2/
    # stat_summary(na.rm = TRUE, fun = "sum", geom = "text",
    #              aes(label = date), # https://stackoverflow.com/questions/56490813/how-do-i-add-a-comma-separator-to-a-text-label-in-geom-text
    #              size = 5, angle = 90) # only geom text lets you set the angle
    
    
  })
  
  
  # Map ----
  
  # Tutorial for Reactive Expressions #
  # https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
  
  selectedCounties <- reactive({
    ICRcounties %>% filter(NAME %in% input$counties)
  })
  
  # may want to do something like this at some point
  # https://community.rstudio.com/t/shiny-leaflet-link-map-polygons-to-reactive-plotly-graphs/40527/2
  # or this
  # https://stackoverflow.com/questions/59342680/can-i-use-in-r-the-leaflet-map-shape-click-event-to-populate-a-box-with-a-da
  
  
  # https://rstudio.github.io/leaflet/shiny.html
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # addPolygons(data = IAcounties,
      #             fillColor = "white",
      #             opacity = 1.0,
      #             fillOpacity = 0.5,
      #             color = "grey",
      #             weight = 1,
      #             layerId = ~COUNTYFP) %>%
      setView(lng = -91.5849977,
              lat = 41.773762,
              zoom = 7)
  })
  
  observe({
    leafletProxy("map", data = selectedCounties()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = "grey",
        color = "black",
        opacity = 0.5,
        fillOpacity = 0.7,
        weight = 2,
      )
    
  })
  
  
}

# Run the application ----
shinyApp(ui = ui, server = server)
