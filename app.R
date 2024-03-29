# This is a simple shiny app for trying the ngrams-based word prediction.

source('predict.R')

maxPredictions <- 5
predictor <- makePredictor('data/model.feather', nPredictions=maxPredictions)

getSuggestions <- function(text, index=nchar(text)) {
  candidates <- predictWords(predictor, text, debug=FALSE)
}

ui <- shinyUI(fluidPage(
  titlePanel("Ngram-Based Word Prediction"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("details", "Show details:",
                   c("No" = FALSE,
                     "Yes" = TRUE)),
      numericInput("nalternatives", "Number of best candidates to show:",
                   min=1, max=maxPredictions, value=1),
      actionButton('clear', label=span('Clear text'))
    ),
    mainPanel(
      fluidRow(
        textAreaInput("text", label=span("Type text here"), value="", cols=120, rows=5),
        tableOutput("prediction")
      )
))))

server <- function(input, output, session) {
    output$prediction <- renderTable({
        sug <- getSuggestions(input$text, nchar(input$text))
        showN <- min(input$nalternatives, nrow(sug))
        if (input$details) {
          sug[1:showN,.(prediction=lastword,probability=pkn,ngram)]
        } else {
           sug[1:showN,.(prediction=lastword)]
        }
    })
    observeEvent(input$clear, {
      updateTextAreaInput(session, "text", value = "")
    })
}

shinyApp(ui = ui, server = server)
