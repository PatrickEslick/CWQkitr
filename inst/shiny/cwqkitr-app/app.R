library(shinydashboard)
library(shiny)
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
        includeMarkdown("info.md")
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
                actionButton("go", "Go")
              )
            ),
            textOutput("cgg_text")
          )
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
                         selected = "Use AQUARIUS gap tolerance"),
                tableOutput("cgg_gapTable"),
                selectInput("cgg_complete_freq", label = "Observation frequency (in minutes)",
                         choices = c("Auto detect" = 0, 15, 30, 60, 120), selected = "Auto detect"),
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
        box()        
      ),
      tabItem(tabName = "sampleq",
        box()        
      )
    )
  )
)

server <- function(input, output) {
   
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
    input$go
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
    
    input$go 
    
    isolate({
      location <- input$cgg_site
      start <- as.character(input$cgg_start)
      end <- as.character(input$cgg_end)
      tsID <- input$cgg_tsID
      parm <- input$cgg_parameter
    })
    
    if(location != "") {
      output <- makeTableConnect(tsID, start, end, parm,
                                          id = Sys.getenv("apiid"),
                                          pw = Sys.getenv("apipw"))
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
    
    freq <- as.numeric(input$cgg_complete_freq)
    if(freq == 0) {
      freq <- "auto"
    }
    
    table() %>%
      pull(datetime) %>%
      recordCompleteness(freq = freq)
    
  })
  
  output$cgg_gapTable <- renderTable({
    
    input$go 
    
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
  
}

shinyApp(ui = ui, server = server)

