library(shiny)
library(DT)
library(futile.logger)
library(readr)
source("utils.R")


loadLatestFile <- function() {
  path <- max(dir("www/data", "\\.csv$", full.names=TRUE))
  flog.info("Latest data path: %s", path)
  x <- read_csv(path)
  flog.info("%d records", nrow(x))
  x
}

ui <- fluidPage(
  titlePanel("\u30a2\u30f3\u30d1\u30f3\u30de\u30f3\u20\u30a8\u30d4\u30bd\u30fc\u30c9\u691c\u7d22")
  #,textInput("keyword", "\u30ad\u30fc\u30ef\u30fc\u30c9")
  ,shiny::HTML(
    "<p><ul>"
    ,sprintf('<li>\u30bd\u30fc\u30b9: <a href="%s" target="_blank" rel="noopener noreferrer">Wikipedia</a>', URL)
    ,"<li>\u30c6\u30ec\u30d3\u30b8\u30e3\u30d1\u30f3\u306e\u653e\u9001\u65e5\u306f\u6982\u7b97\u3067\u3059"
    ,"</ul></p>"
  )
  ,actionButton("recentData", "\u6700\u65b0\u306e\u30c7\u30fc\u30bf\u3092\u53d6\u5f97")
  ,DT::dataTableOutput("tbl")
)

server <- function(input, output) {
  rv <- reactiveValues()
  rv$data <- loadLatestFile() 
  
  output$tbl <- DT::renderDataTable({
    datatable(
      rv$data, filter="top", rownames=FALSE,
      options=list(
        pageLength=25, autowidth=TRUE,
        dom='<"top"i>rt<"bottom"lp><"clear">')
    )
  })
  
  observeEvent(input$recentData, {
    flog.info("Retrieving recent data")
    x <- latestData()
    flog.info("Number of records: %d", nrow(x))
    rv$data <- x
  })  
  
}


shinyApp(ui=ui, server=server)