# Todo:
# - Clear button
# - Better layout
# - Reacting to changes in textInput
# - Explanation and alternative candidates
# - Help

source('predict.R')

predictor <- makePredictor('data/kn.feather')
print(predictor)

getSuggestions <- function(text, index=nchar(text)) {
  predictWords(predictor, substring(text, 1, index-2), debug=TRUE)
}

ui <- shinyUI(fluidPage(
  titlePanel("Ngram-Based Word Prediction"),
  fluidRow(
           textInput("text", label=span("Type text"), value=""),
           submitButton("Predict next word"),
           textOutput("prediction")
    )
))

server <- function(input, output, session) {
    output$prediction <- renderText({
      if (!grepl('^\\s*$', input$text)) {
        sug <- getSuggestions(input$text, nchar(input$text))
        sug[1]
      } else {
        ""
      }
    })
}

shinyApp(ui = ui, server = server)
