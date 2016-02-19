## app.R ##
library(shinydashboard)
library(quantmod)

asxlisted <- read.table("data/ASXListedCompanies.csv", skip = 2, sep = ",", stringsAsFactors = FALSE, header = TRUE)
asxhealth <- subset(asxlisted, GICS.industry.group == "Health Care Equipment & Services")
industries <- sort(unique(asxlisted$GICS.industry.group))

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "ASX Explorer"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(6,
             box(width = NULL,
       selectInput("gics", "Industry:", choices = setNames(industries, industries), multiple = FALSE, selected = sort(unique(asxlisted[, 3]))[1]) 
      ),
      box(width = NULL,
          uiOutput("renderCompany")),
      box(width = NULL,
          uiOutput("renderSlider"))
    ),
    column(6,
           box(width = NULL,
           plotOutput("chart")))
  )
  )
)

server <- function(input, output) {
  
  ts <- reactive({
    getSymbols(paste(input$company, "AX", sep = "."), source = "google", auto.assign = FALSE)
})
  
  output$renderCompany <- renderUI({
    companies <- subset(asxlisted, GICS.industry.group == input$gics)
    companies <- companies[order(companies$Company.name, decreasing = FALSE), ]
    selectInput("company", "Company:", choices = setNames(companies[, 2], companies[, 1]), multiple = FALSE)
  })
  

  output$renderSlider <- renderUI({
      start <- min(index(ts()))
      end   <- max(index(ts()))
      sliderInput(inputId = "range", label = "Dates:", min = start, max = end, step = 7, value = c(start, end))
  })
  
  output$chart <- renderPlot({
      start <- as.Date(input$range[1], origin = "1970-01-01")
      end   <- as.Date(input$range[2], origin = "1970-01-01")
      #      chartSeries(ts()[paste(start, end, sep = "::")], theme = chartTheme("white"), type = "line", TA = NULL, name = as.character(input$company))
      candleChart(ts()[paste(start, end, sep = "::")], theme = chartTheme("white"), name = as.character(input$company))
  })
  
}

shinyApp(ui, server)