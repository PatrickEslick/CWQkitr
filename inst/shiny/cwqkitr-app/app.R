library(shinydashboard)
library(shiny)
library(dplyr)
library(CWQkitr)

ui <- dashboardPage(
   
  dashboardHeader(title = "CWQKitR"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("Information", tabName = "information"),
      menuItem("Corrections/Grades/Gaps", tabName = "corrections"),
      menuItem("Download Time Series", tabName = "download"),
      menuItem("Sample Flows", tabName = "sampleq")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "information",
        fluidRow(
          box( 
            includeMarkdown("info.md")
          , width = 8)
        )
      ),
      tabItem(tabName = "corrections",
        fluidRow(
          box(
            fluidRow(
              column(3, 
                textInput("cgg_site", "Location", placeholder="XXXXXXXX"),
                selectInput("cgg_parameter", "Parameter", 
                            choices=c("Specific cond at 25C", "Turbidity, FNU", "Dissolved oxygen", 
                                      "pH", "Temperature, water"))
              ),
              column(2, 
                uiOutput("cgg_startSelect"),
                uiOutput("cgg_endSelect"),
                helpText("Maximum date range is 18 months")
              ),
              column(3, uiOutput("cgg_tsUI")),
              column(2, 
                helpText("Click to run time series (no need to click for approvals tab)"), 
                actionButton("cgg_go", "Go")
              )
            ),
            textOutput("cgg_text")
          , width = 10)
        ),
        fluidRow(
          box(
            tabsetPanel(
              tabPanel("Summary", tableOutput("cgg_summary")),
              tabPanel("Time Series", dataTableOutput("cgg_table")),
              tabPanel("Record completeness", 
                selectInput("cgg_gapTolerance", label = "Gap Tolerance (in minutes)",
                         choices = c("Use AQUARIUS gap tolerance" = 0, 
                                     30, 60, 120, 180, 240, 300, 360, 720),
                         selected = "Use AQUARIUS gap tolerance", width = "25%"),
                tableOutput("cgg_gapTable"),
                selectInput("cgg_complete_freq", label = "Observation frequency (in minutes)",
                         choices = c("Auto detect" = 0, 15, 30, 60, 120), selected = "Auto detect", width = "25%"),
                tableOutput("cgg_completeTable")
              ),
              tabPanel("Approval", 
                helpText("All unapproved periods:"),
                tableOutput("cgg_unApprovedTable")
              )
            )
          , width = 12)
        )
      ),
      tabItem(tabName = "download",
        box(
          fluidRow(
            column(4, textInput("dts_location", label = "Location, multiples separated by commas", placeholder = "xxxxxxxx,xxxxxxxx")),
            column(3, uiOutput("dts_dateRangeUI")),
            column(1, checkboxInput("dts_publish", label = "Publish", value = TRUE)),
            column(1, style = "margin-top: 25px;", actionButton("find_time_series", "Find time series"))
          ),
          fluidRow(
            column(4, uiOutput("timeSeriesChoices"))
          ),
          fluidRow(
            column(3, selectInput("dts_outpuTz", "Time zone for output",
                                  choices = c("Eastern" = "America/New_York", 
                                              "Central" = "America/Chicago", 
                                              "Mountain" = "America/Denver",
                                              "Arizona" = "America/Phoenix", 
                                              "Pacific" = "America/Los_Angeles",
                                              "Alaska" = "America/Juneau",
                                              "Hawaii" = "Pacific/Honolulu",
                                              "UTC" = "GMT",
                                              "UTC-4" = "Etc/GMT-4",
                                              "UTC-5" = "Etc/GMT-5",
                                              "UTC-6" = "Etc/GMT-6",
                                              "UTC-7" = "Etc/GMT-7",
                                              "UTC-8" = "Etc/GMT-8",
                                              "UTC-9" = "Etc/GMT-9",
                                              "UTC-10" = "Etc/GMT-10"))),
            column(3, selectInput("dts_interval", "Time interval",
                                  choices = c("All", "Hourly", "30-minute", "15-minute", "5-minute"))),
            column(1, style = "margin-top: 25px;", downloadButton("download_data", "Download"))
          )
        , width = 9)        
      ),
      tabItem(tabName = "sampleq",
        box(
          fluidRow(
            column(2, textInput("sf_stationID", "Location", value="", placeholder="12345678")),
            column(2, dateRangeInput("sf_dateRangeInput", "Dates", start="2017-01-01", format="yyyy-mm-dd")),
            column(2, sliderInput("sf_maxDiff", "Maximum gap to use for interpolation (in hours)", min=1, max=5, value=4, step=1)),
            column(2, selectInput("sf_mergeMethod", "Method to merge", 
                                  choices = c("Interpolate between two closest time series points" = "interpolate", 
                                              "Use closest time series point" = "closest"))),
            column(2, selectInput("sf_timeZone", "Time zone",
                                  choices = c("UTC" = "UTC", "Eastern" = "America/New_York", "Central" = "America/Chicago",
                                              "Mountain" = "America/Denver", "Arizona" = "America/Phoenix",
                                              "Pacific" = "America/Los_Angeles")))
          ),
          fluidRow(
            column(8,
                   helpText("Click 'Get data' and wait for the data to load before downloading it")       
            )
          ),
          fluidRow(
            column(1, actionButton("sf_go", "Get data")),
            column(1, downloadButton("downloadFlow", "Download"))
          )
        , width = 10),
        box(
          column(6,tableOutput("flowDataTable"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ##Corrections/Grades/Gaps########################################################################
  cgg_tsChoices <- reactive({
    
    tkn <- retryToken(id = Sys.getenv("apiid"), pw = Sys.getenv("apipw"))
    
    if(nchar(input$cgg_site) >= 8) {
      ts <- getTimeSeriesIDs(input$cgg_site, input$cgg_parameter)
      if(!is.null(ts)) {
        ts <- ts[,c("Identifier", "UniqueId")]
      } else {
        ts <- data.frame(Identifier="None", UniqueId="None")
      }
    } else {
      ts <- data.frame(Identifier="None", UniqueId="None")
    }
    ts
  })
  
  output$cgg_tsUI <- renderUI({
    timeSeries <- cgg_tsChoices()
    ch <- split(timeSeries$UniqueId, timeSeries$Identifier)
    selectInput("cgg_tsID", "Time series", ch, width="100%")
    
  })
  
  output$cgg_startSelect <- renderUI({
    dateInput("cgg_start", "Start")
  })
  
  output$cgg_endSelect <- renderUI({
    start <- input$cgg_start
    maxDate <- start + as.difftime(72, units="weeks")
    dateInput("cgg_end", "End", max=maxDate)
  })
  
  output$cgg_text <- renderText({
    input$cgg_go
    isolate({
      loc <- input$cgg_site
      parm <- input$cgg_parameter
      start <- input$cgg_start
      end <- input$cgg_end
    })
    if(loc != "") {
      text <- paste0("Showing data for: ", loc, "; ", parm, " from ", start, " to ", end)
    } else {
      text <- ""
    }
    text
  })
  
  table <- reactive({
    
    input$cgg_go 
    
    isolate({
      location <- input$cgg_site
      start <- as.character(input$cgg_start)
      end <- as.character(input$cgg_end)
      tsID <- input$cgg_tsID
      parm <- input$cgg_parameter
    })
    
    if(location != "") {
      output <- makeTable(tsID, start, end, parm)
    } else {
      output <- data.frame()
    }
    
  })
  
  output$cgg_summary <- renderTable({
    table() %>%
      summarizeGrades()
  })
  
  output$cgg_table <- renderDataTable({
    table()
  })
  
  output$cgg_unApprovedTable <- renderTable({
    tsID <- input$cgg_tsID
    table <- findDisapproval(getApprovalList(tsID, "0002-01-01", "9998-02-01"))
    table
  })
  
  output$cgg_completeTable <- renderTable({
    
    if(length(table()) == 0)
      return(NULL)
    
    freq <- as.numeric(input$cgg_complete_freq)
    if(freq == 0) {
      freq <- "auto"
    }
    
    table() %>%
      pull(datetime) %>%
      recordCompleteness(freq = freq)
    
  })
  
  output$cgg_gapTable <- renderTable({
    
    input$cgg_go 
    
    if(nrow(table()) == 0)
      return(NULL)
    
    if(testToken() == FALSE) {
      getToken()
    }
    
    isolate({
      location <- input$cgg_site
      start <- input$cgg_start
      end <- input$cgg_end
      tsID <- input$cgg_tsID
      parm <- input$cgg_parameter
    })
    
    gapTol <- as.numeric(input$cgg_gapTolerance)
    
    if(gapTol == 0) {
      gapTol <- getGapTolerance(tsID, 
                                as.character(start, format="%Y-%m-%d"),
                                as.character(end, format="%Y-%m-%d"))
    }
    
    table() %>%
      pull(datetime) %>%
      findGaps(gapTol) %>%
      summarizeGaps(gapTol)
    
  })
  
  ##Download Time Series###########################################################################
  
  output$dts_dateRangeUI <- renderUI({
    
    today <- Sys.Date()
    last_year <- today - as.difftime(365, units = "days")
    
    dateRangeInput("dts_dateRange", label = "Date range", start = last_year, end = today)
    
  })
  
  dts_location_split <- reactive({
    
    location <- input$dts_location
    
    if(is.null(location))
      return(NULL)
    
    if(location == "") 
      return("")
    
    location_split <- strsplit(location, "\\D")[[1]]
    location_split <- location_split[location_split != ""]
    
    return(location_split)
    
  })
  
  available_time_series <- reactive({
    
    tkn <- retryToken(id = Sys.getenv("apiid"), pw = Sys.getenv("apipw"))
    
    input$find_time_series
    
    isolate({
      locations <- dts_location_split()
      date_range <- as.character(input$dts_dateRange, format = "%Y-%m-%d")
      publish <- input$dts_publish
    })
    
    if(locations == "")
      return(NULL)
    
    ats <- data.frame()
    for(i in locations) {
      ats <- bind_rows(ats, getAvailableTimeSeries(i, date_range[1], date_range[2], publish))
    }
    return(ats)
  })
  
  output$timeSeriesChoices <- renderUI({
    
    ts <- available_time_series()
    print(ts)
    if(!is.null(ts)) {
      ch <- ts$UniqueId
      names(ch) <- paste(ts$Parameter, ts$Label, ts$LocationIdentifier)
    } else {
      showNotification("No time series found", type = "error")
      ch <- NULL
    }
    selectInput("dts_timeSeries", "Time series to download", choices = ch, width = "100%", multiple = TRUE)
    
  })
  
  output_data <- reactive({
    
    date_range <- as.character(input$dts_dateRange, format = "%Y-%m-%d")
    
    time_zone <- input$dts_outpuTz
    interval <- input$dts_interval
    
    selected <- data.frame(UniqueId = input$dts_timeSeries) %>%
      left_join(available_time_series(), by = "UniqueId") %>%
      select(UniqueId, Identifier)
    
    ts_id <- selected$UniqueId
    names <- selected$Identifier
    
    getTimeSeries(ts_id, date_range[1], date_range[2], names, time_zone, interval)
    
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$dts_location, "_ts_data.csv")
    },
    content = function(file) {
      showNotification("Downloading...")
      output_data() %>%
        write.csv(file = file, row.names = FALSE)
    }
  )
  
  ##Sample Flows###################################################################################
  
  flowData <- eventReactive(input$sf_go, {
    
    stationID <- input$sf_stationID
    dateRange <- input$sf_dateRangeInput
    maxDiff <- input$sf_maxDiff
    method <- input$sf_mergeMethod
    tz <- input$sf_timeZone
    
    showNotification("Working...", duration=NULL, id="wrk")
    
    sq <- getSampleQ(stationID, dateRange[1], dateRange[2], maxDiff, method, tz = tz)
    
    removeNotification("wrk")
    
    if(class(sq) == "character") {
      msg <- sq
      showNotification(msg, duration = 10, id="err", type="error")
    }
    sq
    
  })

  output$flowDataTable <- renderTable({
    flowData()
  })

  output$downloadFlow <- downloadHandler(
    filename = function() {
      paste0("intFlow", input$sf_stationID, ".csv")
    },
    content = function(file) {
      write.csv(flowData(), file, row.names=FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)

