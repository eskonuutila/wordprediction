# Todo:
# - Focus handling in text area
# - Clear button

source('cleanData.R')
source('kn.R')
library(shiny)
library(shinyjs)

initializePredictor()

getSuggestions <- function(text, index) {
        if (index == 1 || grepl("\\s", substring(text, index - 1, index - 1))) {
                predictWords(substring(text, 1, index-2), debug=FALSE)
        } else {
                NULL
        }
}
# options(shiny.sanitize.errors=FALSE)
# options(shiny.fullstacktrace = TRUE)
maxSuggestions <- 10
suggestionButtonNames <- paste0("suggestion", 1:maxSuggestions)

jsCodeAddSuggestion <- 'shinyjs.addSuggestion = function(params) {
  var defaultParams = {
    id : null,
    buttonId: null
  };
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  var selection = selector.getSelection();
  var alltext = selector.val();
  var word = $("#" + params.buttonId).text();
  if (selection.start > 0 && !/\\s/.test(alltext.charAt(selection.start-1))) {
     word = " " + word;
  }
  if (!(selection.end < alltext.length - 1 && /\\s/.test(alltext.charAt(selection.end + 1)))) {
     word = word + " ";
  }
  if (selection.start != selection.end) {
      selector.replaceSelectedText(word, "collapseToEnd");
  } else {
      selector.insertText(word, selection.start, "collapseToEnd");
  }
  var newpos =  selection.start+word.length;
  // selector.focus();
  // selector.setSelection(newpos, newpos);
  // selector[0].scrollIntoView({behavior: "smooth", block: "end"});
  Shiny.onInputChange("cursorPos",newpos);
};
'

jsCodeClicked <- 'shinyjs.clicked = function(params) {
  var defaultParams = {
    id : null
  };
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  var selection = selector.getSelection();
  Shiny.onInputChange("cursorPos", selection.start)
};
'

jsCodeKeyUp <- 'shinyjs.keyUp = function(params) {
  var defaultParams = {
    id : null,
    event: null
  };
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  var event = params.event;
  if (event != 0) {
    var selection = selector.getSelection();
    Shiny.onInputChange("cursorPos", selection.start);
  }
};
'

jsCodeSetSuggestionButton <- 'shinyjs.setSuggestionButton = function(params) {
  var defaultParams = {
    id : null,
    word: ""
  };
  params = shinyjs.getParams(params, defaultParams);
  var word = params.word;
  $("#" + params.id).text(word);
  if (word == "") {
    $("#" + params.id).attr("disabled", "disabled");
    $("#" + params.id).hide();
  } else {
    $("#" + params.id).removeAttr("disabled");
    $("#" + params.id).show();
  }
  // alert("SetSuggestionButton " + $("#" + params.id) + ", " + params.word);
};
'

ui <- shinyUI(fluidPage(
        useShinyjs(),
        extendShinyjs(text = jsCodeAddSuggestion, functions = "addSuggestion"),
        extendShinyjs(text = jsCodeClicked, functions = "clicked"),
        extendShinyjs(text = jsCodeKeyUp, functions = "keyUp"),
        extendShinyjs(text = jsCodeSetSuggestionButton, functions = "setSuggestionButton"),
        tags$head(tags$script(src="rangyinputs/rangyinputs-jquery.js")),
        titlePanel("Ngram-Based Word Prediction"),
        fluidRow(
                 column(12,
                       # plotOutput("distPlot"),
                       fluidRow(class = "suggestionContainer",
                                actionButton("suggestion1", ""),
                                actionButton("suggestion2", ""),
                                actionButton("suggestion3", ""),
                                actionButton("suggestion4", ""),
                                actionButton("suggestion5", ""),
                                actionButton("suggestion6", ""),
                                actionButton("suggestion7", ""),
                                actionButton("suggestion8", ""),
                                actionButton("suggestion9", ""),
                                actionButton("suggestion10", ""),
                                tags$head(tags$style(".suggestionContainer{height:50px;}"))
                       ),
                       # textInput("text", label = span("Text input"), value = "")
                       textAreaInput("text", rows=6, cols=80, label=span("Type text"), value="")
                )
        )
        ))

server <- function(input, output, session) {
        buttonCounts <- rep(0, maxSuggestions)
        inputCounts <- reactive({c(input$suggestion1,
                                   input$suggestion2,
                                   input$suggestion3,
                                   input$suggestion4,
                                   input$suggestion5,
                                   input$suggestion6,
                                   input$suggestion7,
                                   input$suggestion8,
                                   input$suggestion9,
                                   input$suggestion10)})
        textInput <- reactive({c(input$text, input$cursorPos)})
        observeEvent(inputCounts(), {
                ind <- which(inputCounts() != buttonCounts)
                buttonCounts <<- inputCounts()
                js$addSuggestion("text", suggestionButtonNames[ind])
        })
        observeEvent(textInput(), {
                if (length(input$cursorPos) != 1) {
                        index = 1;
                } else {
                        index = input$cursorPos + 1
                }
                sug <- getSuggestions(input$text, index)
                # print('sug')
                # str(sug)
                sug <- c(sug, rep('', maxSuggestions-length(sug)))
                for (i in 1:maxSuggestions) {
                        # print(paste0("js$setSuggestionButton(", suggestionButtonNames[i], ", ", sug[i], ")"))
                        js$setSuggestionButton(suggestionButtonNames[i], sug[i])
                }
                # print(paste('Add here processing of text (Cursor at pos', input$cursorPos, 'in', input$text, ')'))
        })
        onclick("text", function (event) {
                js$clicked('text')
        })
        onevent("keyup", "text", function (event) {
                # js$cursorPositionChanged('text')
                js$keyUp("text", event)
        })
}

shinyApp(ui = ui, server = server)
