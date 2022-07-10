# wordprediction

This is a simple ngram-based next word prediction app. You write a piece of text and the app presents the most likely next word candidate(s).

## Running

You can run the app either using the R executable or the RStudio. You can install the R executable from [https://cran.r-project.org/](https://cran.r-project.org/) and the RStudio from [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/).

To run the app, clone the repo into your computer. After this you can run the app in two different ways:

1. Command line R + a web browser.
   * cd to the repo root directory
   * Start the app by running `R --vanilla -f app.R`.
   * Open the address http://127.0.0.1:3049 on your browser.

2. RStudio
   * Open RStudio
   * `File > Open Project`
   * Locate file `wordprediction.Rproj` in the repo.
   * Click `app.R` in the `Files` tab.
   * Click `Run App` in the `app.R` window.
   
