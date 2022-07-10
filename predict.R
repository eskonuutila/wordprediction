library(stringi)
library(tidyverse)
library(quanteda)
library(stringr)
library(data.table)
library(R.utils)
library(feather)
library(shiny)
library(shinyjs)

# The model (created elsewhere) is stored into a .feather file
loadModel <- function(modelPath = 'data/model.feather') {
  as.data.table(feather::read_feather(modelPath))
}

# The model is internally stored as a dictionary
makePredictionDictionary <- function(ngrams, pfield = "pkn") {
  dict <- ngrams[level == 1 & grepl('^[^A-Z]*$', ngram)]
  setorderv(dict, cols = c(pfield), order = c(-1))
  dict
}

# This function executes the model to find the best candidates for the next word after the given words.
candidates <- function(model, dict, words, n=length(words), maxCandidates=5, pfield='pkn', debug=FALSE) {
  wl <- length(words)
  n <- min(wl, n)
  i <- wl - n
  result <- list()
  lastwords <- c()
  need <- maxCandidates
  if (debug) printf('\ngetting %s best candidates to follow "%s" using ngrams=%d:%d\n', maxCandidates, paste(words[(i+1):wl], collapse=' '), 1, n)
  for (j in 1:n) {
    pref <- paste0(paste(words[(i+j):wl], collapse = '_'), '_')
    if (debug) printf('  looking for ngrams starting with "%s"\n', pref)
    hits <- model[prefix == pref][grepl('^[^A-Z]*$', lastword)][!(lastword %in% lastwords)]
    setorderv(hits, cols=c(pfield), order=c(-1))
    if (debug) {
      printf('  Got %d hits\n', nrow(hits))
      str(hits)
    }
    take <- min(need, nrow(hits))
    if (take > 0) {
      newsuggestions <- hits[1:take]
      result <- c(result, list(newsuggestions))
      lastwords <- c(lastwords,newsuggestions$lastword)
      need <- need - take
    }
    if (debug) printf('    found %d ngrams, taking %d of them, %s\n', nrow(hits), take, if (need > 0) sprintf('still missing %d', need) else 'done')
    if (need == 0) break
    if (debug) {
      printf('    result now\n')
      str(result)
    }
  }
  if (need > 0) {
    if (debug) printf('  finally taking %d most common words\n', need)
    hits <- head(dict, need)
    result <- c(result, list(hits))
  }
  result <- bind_rows(result)
  if (debug) {
    printf('The result is\n')
    print(result)
  }
  result
}

# Creates an ngram-based predictor. The predictor is a function that
# calls the function `candidates` to return the candidates for the
# next word after the given text.
makePredictor <- function(modelOrPath, n=4, nPredictions=5, debug=FALSE) {
  N <- if (is.null(n)) max(ngrams$level)-1 else n
  if (is.character(modelOrPath)) {
    model <- loadModel(modelOrPath)
  } else {
    model <- modelOrPath
  }
  dict <- makePredictionDictionary(model)
  function (words, nPredictions=nPredictions, debug=debug) {
    candidates(model, dict, words, n=N, maxCandidates=nPredictions, debug=debug)
  }
}

# The input data must be cleaned the same way as the training data was
# cleaned for the model. The function quanteda::tokens takes care of
# a part of the cleaning and the rest is done here.
cleanLines <- function(lines,
                       downcase=TRUE,
                       sentences=TRUE,
                       remove_numbers=FALSE,
                       replace_numbers=TRUE,
                       remove_email=FALSE,
                       replace_email=TRUE,
                       remove_url=FALSE,
                       remove_symbols=TRUE,
                       remove_punct=TRUE,
                       remove_ellipses=TRUE,
                       remove_underscores=TRUE,
                       remove_twitter=FALSE,
                       replace_twitter=TRUE) {
  if (sentences) lines <- unlist(as.list(tokens(lines, what = "sentence")))
  if (downcase) lines <- unname(sapply(lines, tolower))
  if (replace_email) lines <- gsub('[a-z0-9_.-]+@[a-z0-9_.-]+', 'EMAIL', lines)
  else if (remove_email) lines <- gsub('[a-z0-9_.-]+@[a-z0-9_.-]+', '', lines)
  if (replace_twitter) lines <- gsub('@[a-zA-Z0-9]+', 'AT', gsub('#[a-zA-Z0-9]+','HASHTAG', lines))
  if (replace_numbers) lines <- gsub('[0-9]+([.][0-9]+)?', 'NUMBER', lines)
  if (remove_underscores) lines = gsub('_+', ' ', lines)
  if (remove_ellipses) lines <- unlist(strsplit(lines, '[.][.]+'))
  if (length(lines) > 0) {
    lines <- sapply(tokens(lines,
                           remove_numbers=remove_numbers,
                           remove_url=remove_url,
                           remove_symbols=TRUE,
                           remove_punct=TRUE),
                    function (x)  paste(x, collapse=" "))
    lines <- lines[lines != '']
  }
  lines
}

# Predict the next word after the given text using a predictor. Parameter maxCandidates
# is the (maximum) number of best candidates to return
predictWords <- function(predictor, text, n=3, maxCandidates=5, pfield="pkn", caseHandling=FALSE, debug=FALSE) {
  sentenceStart <- grepl('([.!?]\\s*$|^\\s*$)', text)
  if (sentenceStart) {
    sentence <- ""
  } else {
    lines <- cleanLines(text)
    if (length(lines) == 0) {
      sentence <- ""
    } else {
      sentence <- tail(lines, 1)
    }
  }
  words <- c(paste0('S', 1:n), unlist(strsplit(sentence, ' ')))
  candidates <- predictor(words, nPredictions=maxCandidates, debug=debug)
  if (debug) {
    printf('predictWords:\n')
    str(candidates)
  }
  if (debug) printf('Prediction %s\n', candidates)
  candidates
}
