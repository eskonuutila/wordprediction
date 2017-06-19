# Todo:
# - Help

source('predict.R')

maxPredictions <- 5
predictor <- makePredictor('data/small.feather', nPredictions=maxPredictions)
# print(predictor)

getSuggestions <- function(text, index=nchar(text)) {
  candidates <- predictWords(predictor, text, debug=FALSE)
}

ui <- shinyUI(fluidPage(
  titlePanel("Ngram-Based Word Prediction"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("nalternatives", "Number of best candidates to show:",
                  min=1, max=maxPredictions, value=1),
      radioButtons("details", "Show details:",
                   c("No" = FALSE,
                     "Yes" = TRUE)),
      # actionButton('predict', label=span('Predict')),
      actionButton('submit', label=span('Submit')),
      actionButton('clear', label=span('Clear text'))
    ),
    mainPanel(
      fluidRow(
        textAreaInput("text", label=span("Type text here"), value="", cols=80, rows=5),
        tableOutput("prediction")
      )
))))

server <- function(input, output, session) {
    output$prediction <- renderTable({
      input$submit
      # if (!grepl('^\\s*$', input$text)) {
        sug <- getSuggestions(input$text, nchar(input$text))
        showN <- min(input$nalternatives, nrow(sug))
        if (input$details) {
          sug[1:showN,.(prediction=lastword,probability=pkn,ngram)]
        } else {
           sug[1:showN,.(prediction=lastword)]
        }
      # } else {
      #   data.frame(prediction=character(0))
      # }
    })
    observeEvent(input$clear, {
      updateTextAreaInput(session, "text", value = "")
    })
}

shinyApp(ui = ui, server = server)
