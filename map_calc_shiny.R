library(BAMMtools)
library(shiny)

# Obtain percentage lower-bound errors
lower <- function(x, i){
  pnorm(as.numeric(as.character(x[i][, 5])),
        mean = x[i][, 1], sd = x[i][, 3],
        lower.tail = TRUE) * 100
}
# Obtain percentage upper-bound errors
upper <- function(x, i){
  pnorm(as.numeric(as.character(x[i][, 6])),
        mean = x[i][, 1], sd = x[i][, 3],
        lower.tail = FALSE) * 100
}
# Obtain the mean error by map class
average <- function(x, i){
  mean(x[i])
}

ui <- fluidPage(
  titlePanel("Map Classification Error Calculator"),
  fluidRow(
    column(4,
           fileInput("file1", "Upload two-column CSV file, where first column is estimate and second is MOE:",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           ),
           tags$hr(),
           checkboxInput("header", "File has a header", TRUE),
           checkboxInput("zeroes", "Include estimates of 0 in error calculations", FALSE),
           htmlOutput("county_selector"),
           textInput("pct", "Acceptable error percentage (Optional):", "10"),
           textInput("vec1", "Custom comma-delimited breaks (Optional):","0.75,3.32"),
           helpText("Maps are considered reliable if: 1) No single class has an expected error above 20%; and 2) The overall expected error is less than 10%."),
           textOutput("explainSummary"),
           verbatimTextOutput("printSummary"),
           textOutput("explainUB"),
           verbatimTextOutput("printBreaksU"),
           textOutput("explainEB"),
           verbatimTextOutput("printBreaksE"),
           textOutput("explainJB"),
           verbatimTextOutput("printBreaksJ"),
           textOutput("explainQB"),
           verbatimTextOutput("printBreaksQ"),
           textOutput("explainSB"),
           verbatimTextOutput("printBreaksS")
    ),
    column(width = 8,
           h3("Summary of Expected Errors"),
           fluidRow(
             column(6,
                    h4("Equal Interval Breaks"),
                    fluidRow(
                      column(4,
                             tags$b("Classes"), br(),
                             "2", br(),
                             "3", br(),
                             "4", br(),
                             "5", br(),
                             "6", br(),
                             "7"),
                      column(4,
                             tags$b("Overall Error"),
                             htmlOutput("verdictE2"),
                             htmlOutput("verdictE3"),
                             htmlOutput("verdictE4"),
                             htmlOutput("verdictE5"),
                             htmlOutput("verdictE6"),
                             htmlOutput("verdictE7")),
                      column(4,
                             tags$b("Error by Class"),
                             htmlOutput("verdictET2"),
                             htmlOutput("verdictET3"),
                             htmlOutput("verdictET4"),
                             htmlOutput("verdictET5"),
                             htmlOutput("verdictET6"),
                             htmlOutput("verdictET7"))
                    ),
                    hr(),
                    h4("Standard Deviation Breaks"),
                    fluidRow(
                      column(4,
                             tags$b("Classes"), br(),
                             "2", br(),
                             "3", br(),
                             "4", br(),
                             "5", br(),
                             "6", br(),
                             "7"),
                      column(4,
                             tags$b("Overall Error"),
                             htmlOutput("verdictS2"),
                             htmlOutput("verdictS3"),
                             htmlOutput("verdictS4"),
                             htmlOutput("verdictS5"),
                             htmlOutput("verdictS6"),
                             htmlOutput("verdictS7")),
                      column(4,
                             tags$b("Error by Class"),
                             htmlOutput("verdictST2"),
                             htmlOutput("verdictST3"),
                             htmlOutput("verdictST4"),
                             htmlOutput("verdictST5"),
                             htmlOutput("verdictST6"),
                             htmlOutput("verdictST7"))
                    )
             ),
             column(6,
                    h4("Jenks Breaks"),
                    fluidRow(
                      column(4,
                             tags$b("Classes"), br(),
                             "2", br(),
                             "3", br(),
                             "4", br(),
                             "5", br(),
                             "6", br(),
                             "7"),
                      column(4,
                             tags$b("Overall Error"),
                             htmlOutput("verdictJ2"),
                             htmlOutput("verdictJ3"),
                             htmlOutput("verdictJ4"),
                             htmlOutput("verdictJ5"),
                             htmlOutput("verdictJ6"),
                             htmlOutput("verdictJ7")),
                      column(4,
                             tags$b("Error by Class"),
                             htmlOutput("verdictJT2"),
                             htmlOutput("verdictJT3"),
                             htmlOutput("verdictJT4"),
                             htmlOutput("verdictJT5"),
                             htmlOutput("verdictJT6"),
                             htmlOutput("verdictJT7"))
                    ),
                    hr(),
                    h4("Quantile Breaks"),
                    fluidRow(
                      column(4,
                             tags$b("Classes"), br(),
                             "2", br(),
                             "3", br(),
                             "4", br(),
                             "5", br(),
                             "6", br(),
                             "7"),
                      column(4,
                             tags$b("Overall Error"),
                             htmlOutput("verdictQ2"),
                             htmlOutput("verdictQ3"),
                             htmlOutput("verdictQ4"),
                             htmlOutput("verdictQ5"),
                             htmlOutput("verdictQ6"),
                             htmlOutput("verdictQ7")),
                      column(4,
                             tags$b("Error by Class"),
                             htmlOutput("verdictQT2"),
                             htmlOutput("verdictQT3"),
                             htmlOutput("verdictQT4"),
                             htmlOutput("verdictQT5"),
                             htmlOutput("verdictQT6"),
                             htmlOutput("verdictQT7"))
                    ))
           ),
           hr(),
           h3("Error by Number of Classes"),
           helpText("Select a number of classes to see detailed error reporting."),
           h4("Equal Interval Breaks"),
           textOutput("explainE"),
           htmlOutput("verdictE"),
           verbatimTextOutput("errorsE"),
           textOutput("explainET"),
           htmlOutput("verdictET"),
           verbatimTextOutput("errorsET"),
           h4("Jenks Breaks"),
           textOutput("explainJ"),
           htmlOutput("verdictJ"),
           verbatimTextOutput("errorsJ"),
           textOutput("explainJT"),
           htmlOutput("verdictJT"),
           verbatimTextOutput("errorsJT"),
           h4("Quantile Breaks"),
           textOutput("explainQ"),
           htmlOutput("verdictQ"),
           verbatimTextOutput("errorsQ"),
           textOutput("explainQT"),
           htmlOutput("verdictQT"),
           verbatimTextOutput("errorsQT"),
           h4("Standard Deviation Breaks"),
           textOutput("explainS"),
           htmlOutput("verdictS"),
           verbatimTextOutput("errorsS"),
           textOutput("explainST"),
           htmlOutput("verdictST"),
           verbatimTextOutput("errorsST"),
           h4("User-Defined Breaks"),
           textOutput("explainU"),
           htmlOutput("verdictU"),
           verbatimTextOutput("errorsU"),
           textOutput("explainUT"),
           htmlOutput("verdictUT"),
           verbatimTextOutput("errorsUT")
    )
  )
)

server <- function(input, output, session) {
  output$contents <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = input$header)
  })
  output$county_selector = renderUI({
    selectInput(inputId = "county", #name of input
                label = "Select number of classes: ", #label displayed in ui
                choices = c(2:7), # NEW: can do 2-9
                selected = 5)
  })
  
  output$verdictE2 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 2
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    overallErrorsE <- weighted.mean(totalErrorsE[[4]], totalErrorsE[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsE > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsE, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsE, 2), "%"))
    }
  })
  
  output$verdictET2 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 2
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsE[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictJ2 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 2
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    overallErrorsJ <- weighted.mean(totalErrorsJ[[4]], totalErrorsJ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsJ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsJ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsJ, 2), "%"))
    }
  })
  
  output$verdictJT2 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 2
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsJ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictQ2 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 2
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    overallErrorsQ <- weighted.mean(totalErrorsQ[[4]], totalErrorsQ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsQ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsQ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsQ, 2), "%"))
    }
  })
  
  output$verdictQT2 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 2
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsQ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  output$verdictS2 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((2 %% 2) == 0){
      return("NA")
    }
  })
  
  output$verdictST2 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((2 %% 2) == 0){
      return("NA")
    }
  })
  
  # *****************************************************************************
  
  output$verdictE3 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 3
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    overallErrorsE <- weighted.mean(totalErrorsE[[4]], totalErrorsE[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsE > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsE, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsE, 2), "%"))
    }
  })
  
  output$verdictET3 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 3
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsE[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictJ3 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 3
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    overallErrorsJ <- weighted.mean(totalErrorsJ[[4]], totalErrorsJ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsJ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsJ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsJ, 2), "%"))
    }
  })
  
  output$verdictJT3 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 3
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsJ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictQ3 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 3
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    overallErrorsQ <- weighted.mean(totalErrorsQ[[4]], totalErrorsQ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsQ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsQ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsQ, 2), "%"))
    }
  })
  
  output$verdictQT3 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 3
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsQ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  output$verdictS3 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((3 %% 2) == 0){
      return("NA")
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 3
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
    colnames(totalErrorsS)[4] <- "Total Class Error"
    overallErrorsS <- weighted.mean(totalErrorsS[[4]], totalErrorsS[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsS > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsS, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsS, 2), "%"))
    }
  })
  
  output$verdictST3 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((3 %% 2) == 0){
      return("NA")
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 3
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
    colnames(totalErrorsS)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsS[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  # *****************************************************************************
  
  output$verdictE4 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 4
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    overallErrorsE <- weighted.mean(totalErrorsE[[4]], totalErrorsE[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsE > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsE, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsE, 2), "%"))
    }
  })
  
  output$verdictET4 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 4
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsE[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictJ4 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 4
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    overallErrorsJ <- weighted.mean(totalErrorsJ[[4]], totalErrorsJ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsJ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsJ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsJ, 2), "%"))
    }
  })
  
  output$verdictJT4 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 4
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsJ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictQ4 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 4
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    overallErrorsQ <- weighted.mean(totalErrorsQ[[4]], totalErrorsQ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsQ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsQ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsQ, 2), "%"))
    }
  })
  
  output$verdictQT4 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 4
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsQ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  output$verdictS4 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((4 %% 2) == 0){
      return("NA")
    }
  })
  
  output$verdictST4 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((4 %% 2) == 0){
      return("NA")
    }
  })
  
  # *****************************************************************************
  
  output$verdictE5 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 5
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    overallErrorsE <- weighted.mean(totalErrorsE[[4]], totalErrorsE[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsE > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsE, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsE, 2), "%"))
    }
  })
  
  output$verdictET5 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 5
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsE[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictJ5 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 5
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    overallErrorsJ <- weighted.mean(totalErrorsJ[[4]], totalErrorsJ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsJ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsJ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsJ, 2), "%"))
    }
  })
  
  output$verdictJT5 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 5
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsJ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictQ5 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 5
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    overallErrorsQ <- weighted.mean(totalErrorsQ[[4]], totalErrorsQ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsQ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsQ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsQ, 2), "%"))
    }
  })
  
  output$verdictQT5 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 5
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsQ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  output$verdictS5 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((5 %% 2) == 0){
      return("NA")
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 5
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
    colnames(totalErrorsS)[4] <- "Total Class Error"
    overallErrorsS <- weighted.mean(totalErrorsS[[4]], totalErrorsS[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsS > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsS, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsS, 2), "%"))
    }
  })
  
  output$verdictST5 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((5 %% 2) == 0){
      return("NA")
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 5
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
    colnames(totalErrorsS)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsS[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  # *****************************************************************************
  
  output$verdictE6 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 6
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    overallErrorsE <- weighted.mean(totalErrorsE[[4]], totalErrorsE[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsE > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsE, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsE, 2), "%"))
    }
  })
  
  output$verdictET6 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 6
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsE[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictJ6 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 6
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    overallErrorsJ <- weighted.mean(totalErrorsJ[[4]], totalErrorsJ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsJ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsJ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsJ, 2), "%"))
    }
  })
  
  output$verdictJT6 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 6
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsJ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictQ6 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 6
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    overallErrorsQ <- weighted.mean(totalErrorsQ[[4]], totalErrorsQ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsQ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsQ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsQ, 2), "%"))
    }
  })
  
  output$verdictQT6 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 6
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsQ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  output$verdictS6 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((6 %% 2) == 0){
      return("NA")
    }
  })
  
  output$verdictST6 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((6 %% 2) == 0){
      return("NA")
    }
  })
  
  # *****************************************************************************
  
  output$verdictE7 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 7
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    overallErrorsE <- weighted.mean(totalErrorsE[[4]], totalErrorsE[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsE > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsE, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsE, 2), "%"))
    }
  })
  
  output$verdictET7 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 7
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsE[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictJ7 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 7
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    overallErrorsJ <- weighted.mean(totalErrorsJ[[4]], totalErrorsJ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsJ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsJ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsJ, 2), "%"))
    }
  })
  
  output$verdictJT7 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 7
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsJ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$verdictQ7 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 7
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    overallErrorsQ <- weighted.mean(totalErrorsQ[[4]], totalErrorsQ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsQ > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsQ, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsQ, 2), "%"))
    }
  })
  
  output$verdictQT7 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 7
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("ERROR: Breaks not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsQ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  output$verdictS7 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((7 %% 2) == 0){
      return("NA")
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 7
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
    colnames(totalErrorsS)[4] <- "Total Class Error"
    overallErrorsS <- weighted.mean(totalErrorsS[[4]], totalErrorsS[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsS > cutoff ){
      return(paste0("<span style=\"color:red\">FLAG</span>", ": ", round(overallErrorsS, 2), "%"))
    }else{
      return(paste0("<span style=\"color:green\">OK</span>", ": ", round(overallErrorsS, 2), "%"))
    }
  })
  
  output$verdictST7 <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((7 %% 2) == 0){
      return("NA")
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- 7
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
    colnames(totalErrorsS)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsS[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  # *****************************************************************************
  # *****************************************************************************
  # *****************************************************************************
  
  output$explainSummary <- renderText({"Here's the data you uploaded:"})
  
  output$printSummary <- renderPrint({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    mySummary <- summary(dat[[1]])
    round(mySummary, digits = 3)
  })
  
  output$explainEB <- renderText({"Equal Interval Break Values"})
  
  output$printBreaksE <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    newbreaks <- newbreaks[1:(length(newbreaks)-1)]
    newbreaks <- newbreaks[-1]
    round(newbreaks, digits = 3)
  })
  
  output$explainE <- renderText({"Overall Expected Classification Error"})
  
  output$verdictE <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    overallErrorsE <- weighted.mean(totalErrorsE[[4]], totalErrorsE[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsE > cutoff ){
      return(paste("<span style=\"color:red\">FLAG: High Overall Error</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsE <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    overallErrorsE <- round(weighted.mean(totalErrorsE[[4]], totalErrorsE[[3]]), digits = 3)
    overallErrorsE
  })
  
  output$explainET <- renderText({"Expected Error By Class"})
  
  output$verdictET <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- totalErrorsE[1] + totalErrorsE[2]
    colnames(totalErrorsE)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsE[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG: High Error in One or More Classes</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsET <- renderPrint({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- seq(from = min(dat[[1]]),
                     to = max(dat[[1]]),
                     by = (max(dat[[1]]) - min(dat[[1]])) / myclasses)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsE <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsE)[1] <- "Lower Bound Error"
    totalErrorsE$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsE$`Total Obs.` <- totalObs
    totalErrorsE[4] <- round((totalErrorsE[1] + totalErrorsE[2]), digits = 3)
    totalErrorsE[1] <- round(totalErrorsE[1], digits = 3)
    totalErrorsE[2] <- round(totalErrorsE[2], digits = 3)
    colnames(totalErrorsE)[4] <- "Total Class Error"
    totalErrorsE
  })
  
  output$explainJB <- renderText({"Jenks Break Values"})
  
  output$printBreaksJ <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    newbreaks <- newbreaks[1:(length(newbreaks)-1)]
    newbreaks <- newbreaks[-1]
    round(newbreaks, digits = 3)
  })
  
  output$explainJ <- renderText({"Overall Expected Classification Error"})
  
  output$verdictJ <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    overallErrorsJ <- weighted.mean(totalErrorsJ[[4]], totalErrorsJ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsJ > cutoff ){
      return(paste("<span style=\"color:red\">FLAG: High Overall Error</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsJ <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    overallErrorsJ <- round(weighted.mean(totalErrorsJ[[4]], totalErrorsJ[[3]], na.rm = TRUE), digits = 3)
    overallErrorsJ
  })
  
  output$explainJT <- renderText({"Expected Error by Class"})
  
  output$verdictJT <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- totalErrorsJ[1] + totalErrorsJ[2]
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsJ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG: High Error in One or More Classes</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsJT <- renderPrint({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- getJenksBreaks(dat[[1]], myclasses + 1)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsJ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsJ)[1] <- "Lower Bound Error"
    totalErrorsJ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsJ$`Total Obs.` <- totalObs
    totalErrorsJ[4] <- round((totalErrorsJ[1] + totalErrorsJ[2]), digits = 3)
    totalErrorsJ[1] <- round(totalErrorsJ[1], digits = 3)
    totalErrorsJ[2] <- round(totalErrorsJ[2], digits = 3)
    colnames(totalErrorsJ)[4] <- "Total Class Error"
    totalErrorsJ
  })
  
  output$explainQB <- renderText({"Quantile Break Values"})
  
  output$printBreaksQ <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("Breaks are not unique")
    newbreaks <- newbreaks[1:(length(newbreaks)-1)]
    newbreaks <- newbreaks[-1]
    round(newbreaks, digits = 3)
  })
  
  output$explainQ <- renderText({"Overall Expected Classification Error"})
  
  output$verdictQ <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return(NULL)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    overallErrorsQ <- weighted.mean(totalErrorsQ[[4]], totalErrorsQ[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsQ > cutoff ){
      return(paste("<span style=\"color:red\">FLAG: High Overall Error</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsQ <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("Breaks are not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    overallErrorsQ <- round(weighted.mean(totalErrorsQ[[4]], totalErrorsQ[[3]], na.rm = TRUE), digits = 3)
    overallErrorsQ
  })
  
  output$explainQT <- renderText({"Expected Error by Class"})
  
  output$verdictQT <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return(NULL)
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- totalErrorsQ[1] + totalErrorsQ[2]
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsQ[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG: High Error in One or More Classes</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsQT <- renderPrint({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    newbreaks <- quantile(dat[[1]], probs = seq(0, 1, length = myclasses + 1))
    if (length(unique(newbreaks)) != length(newbreaks))
      return("Breaks are not unique")
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsQ <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsQ)[1] <- "Lower Bound Error"
    totalErrorsQ$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsQ$`Total Obs.` <- totalObs
    totalErrorsQ[4] <- round((totalErrorsQ[1] + totalErrorsQ[2]), digits = 3)
    totalErrorsQ[1] <- round(totalErrorsQ[1], digits = 3)
    totalErrorsQ[2] <- round(totalErrorsQ[2], digits = 3)
    colnames(totalErrorsQ)[4] <- "Total Class Error"
    totalErrorsQ
  })
  
  output$explainSB <- renderText({"Standard Deviation Break Values"})
  
  output$printBreaksS <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    if((as.numeric(input$county) %% 2) == 0){
      return("Must select odd number of classes")
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    halfStDevBreaks <- halfStDevBreaks[1:(length(halfStDevBreaks)-1)]
    halfStDevBreaks <- halfStDevBreaks[-1]
    round(halfStDevBreaks, digits = 3)
  })
  
  output$explainS <- renderText({"Overall Expected Classification Error"})
  
  output$verdictS <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((as.numeric(input$county) %% 2) == 0){
      return(NULL)
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
    colnames(totalErrorsS)[4] <- "Total Class Error"
    overallErrorsS <- weighted.mean(totalErrorsS[[4]], totalErrorsS[[3]], na.rm = TRUE)
    cutoff <- as.numeric(input$pct)
    if( overallErrorsS > cutoff ){
      return(paste("<span style=\"color:red\">FLAG: High Overall Error</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsS <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    if((as.numeric(input$county) %% 2) == 0){
      return("Use odd number of classes")
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
    colnames(totalErrorsS)[4] <- "Total Class Error"
    overallErrorsS <- round(weighted.mean(totalErrorsS[[4]], totalErrorsS[[3]], na.rm = TRUE), digits = 3)
    overallErrorsS
  })
  
  output$explainST <- renderText({"Expected Error by Class"})
  
  output$verdictST <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((as.numeric(input$county) %% 2) == 0){
      return(NULL)
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- totalErrorsS[1] + totalErrorsS[2]
    colnames(totalErrorsS)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsS[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG: High Error in One or More Classes</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsST <- renderPrint({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    if((as.numeric(input$county) %% 2) == 0){
      return("Use odd number of classes")
    }
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    myclasses <- as.numeric(input$county)
    # Half-Standard Deviation (+- 0.5, +- 1.5, etc.)
    halfStDevCount <- c(-1 * rev(seq(1, myclasses, by = 2)),
                        seq(1, myclasses, by = 2))
    if((myclasses %% 2) == 1) {
      halfStDevBreaks <- unlist(lapply(halfStDevCount,
                                       function(i) (0.5 * i * sd(dat[[1]])) + mean(dat[[1]])))
      halfStDevBreaks[[1]] <- ifelse(min(dat[[1]]) < halfStDevBreaks[[1]],
                                     min(dat[[1]]), halfStDevBreaks[[1]])
      halfStDevBreaks[[myclasses + 1]] <- ifelse(max(dat[[1]]) > halfStDevBreaks[[myclasses + 1]],
                                                 max(dat[[1]]), halfStDevBreaks[[myclasses + 1]])
    } else {
      halfStDevBreaks <- NA
    }
    newbreaks <- halfStDevBreaks
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsS <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsS)[1] <- "Lower Bound Error"
    totalErrorsS$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsS$`Total Obs.` <- totalObs
    totalErrorsS[4] <- round((totalErrorsS[1] + totalErrorsS[2]), digits = 3)
    totalErrorsS[1] <- round(totalErrorsS[1], digits = 3)
    totalErrorsS[2] <- round(totalErrorsS[2], digits = 3)
    colnames(totalErrorsS)[4] <- "Total Class Error"
    totalErrorsS
  })
  
  # *****************************************************************************
  # *****************************************************************************
  # *****************************************************************************
  
  output$explainUB <- renderText({"User-Defined Break Values"})
  
  output$printBreaksU <- renderText({
    x <- as.numeric(unlist(strsplit(input$vec1,",")))
    # cat("User-Defined Breaks:\n")
    print(x)
  })
  
  output$explainU <- renderText({"Overall Expected Classification Error"})
  
  output$verdictU <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645
    uBreaks <- as.numeric(unlist(strsplit(input$vec1,",")))
    if (any(is.na(uBreaks)))
      return(NULL)
    myclasses <- length(uBreaks) + 1
    newbreaks <- c(min(dat[[1]]), uBreaks, max(dat[[1]]))
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsU <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsU)[1] <- "Lower Bound Error"
    totalErrorsU$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsU$`Total Obs.` <- totalObs
    totalErrorsU[4] <- totalErrorsU[1] + totalErrorsU[2]
    colnames(totalErrorsU)[4] <- "Total Class Error"
    overallErrorsU <- weighted.mean(totalErrorsU[[4]], totalErrorsU[[3]], na.rm = TRUE)
    if( overallErrorsU > 10 ){
      return(paste("<span style=\"color:red\">FLAG: High</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsU <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return("Upload data to see results")
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645
    uBreaks <- as.numeric(unlist(strsplit(input$vec1,",")))
    if (any(is.na(uBreaks)))
      return("Invalid breaks input. Only numeric values and commas valid")
    myclasses <- length(uBreaks) + 1
    newbreaks <- c(min(dat[[1]]), uBreaks, max(dat[[1]]))
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsU <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsU)[1] <- "Lower Bound Error"
    totalErrorsU$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsU$`Total Obs.` <- totalObs
    totalErrorsU[4] <- totalErrorsU[1] + totalErrorsU[2]
    colnames(totalErrorsU)[4] <- "Total Class Error"
    overallerrorsU <- round(weighted.mean(totalErrorsU[[4]], totalErrorsU[[3]]), digits = 3)
    overallerrorsU
  })
  
  output$explainUT <- renderText({"Expected Error By Class"})
  
  output$verdictUT <- renderText({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645
    uBreaks <- as.numeric(unlist(strsplit(input$vec1,",")))
    if (any(is.na(uBreaks)))
      return(NULL)
    myclasses <- length(uBreaks) + 1
    newbreaks <- c(min(dat[[1]]), uBreaks, max(dat[[1]]))
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsU <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsU)[1] <- "Lower Bound Error"
    totalErrorsU$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsU$`Total Obs.` <- totalObs
    totalErrorsU[4] <- totalErrorsU[1] + totalErrorsU[2]
    colnames(totalErrorsU)[4] <- "Total Class Error"
    cutoff <- as.numeric(input$pct)
    criteria <- ifelse(totalErrorsU[4] > cutoff * 2, 1, 0)
    if(1 %in% criteria){
      return(paste("<span style=\"color:red\">FLAG: High Error in One or More Classes</span>"))
    }else{
      return(paste("<span style=\"color:green\">OK</span>"))
    }
  })
  
  output$errorsUT <- renderPrint({
    inFile2 <- input$file1
    if (is.null(inFile2))
      return(NULL)
    dat <- read.csv(inFile2$datapath, header = input$header)
    dat[dat == 0] <- NA
    dat <- dat[complete.cases(dat),]
    if (input$zeroes == TRUE){
      dat <- read.csv(inFile2$datapath, header = input$header)
    }
    dat$stdev <- dat[[2]] / 1.645 # derive standard deviations from moes
    uBreaks <- as.numeric(unlist(strsplit(input$vec1,",")))
    if (any(is.na(uBreaks)))
      return("Invalid breaks input. Only numeric values and commas valid")
    myclasses <- length(uBreaks) + 1
    newbreaks <- c(min(dat[[1]]), uBreaks, max(dat[[1]]))
    dat$classCode <- cut(dat[[1]], labels = FALSE, breaks = newbreaks, # [[4]]
                         include.lowest = TRUE, right = TRUE)
    dat$lowerBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[1:length(newbreaks) - 1]), # [[5]]
                          include.lowest = TRUE, right = TRUE)
    dat$upperBound <- cut(dat[[1]],
                          breaks = newbreaks,
                          labels = c(newbreaks[2:length(newbreaks)]), # [[6]]
                          include.lowest = TRUE, right = TRUE)
    dat$classCode <- paste0("Class ", dat[[4]])
    dat2 <- split(dat, dat[[4]])
    dat2[[1]]$lowerBound <- -Inf
    dat2[[length(dat2)]]$upperBound <- Inf
    lowerBoundErrors <- lapply(dat2, lower)
    upperBoundErrors <- lapply(dat2, upper)
    totalObs <- lapply(dat2, function(i) nrow(i))
    totalErrorsU <- as.data.frame(sapply(lowerBoundErrors, average))
    colnames(totalErrorsU)[1] <- "Lower Bound Error"
    totalErrorsU$`Upper Bound Error` <- sapply(upperBoundErrors, average)
    totalErrorsU$`Total Obs.` <- totalObs
    totalErrorsU[4] <- round((totalErrorsU[1] + totalErrorsU[2]), digits = 3)
    totalErrorsU[1] <- round(totalErrorsU[1], digits = 3)
    totalErrorsU[2] <- round(totalErrorsU[2], digits = 3)
    colnames(totalErrorsU)[4] <- "Total Class Error"
    totalErrorsU
  })
}

shinyApp(ui = ui, server = server)
