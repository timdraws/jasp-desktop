#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Main function ----
MLRegressionRandomForest <- function(jaspResults, dataset, options, ...) {

  # Set title
  jaspResults$title <- "Random Forest Regression"
  
  # Init options: add variables to options to be used in the remainder of the analysis
  options <- .mlRegRandomForestInitOptions(jaspResults, options)
  
  # Sead dataset
  dataset <- .mlRegRandomForestReadData(dataset, options)
  
  # Error checking
  errors <- .mlRegRandomForestErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  mlRegRandomForestResults <- .mlRegRandomForestComputeResults(jaspResults, dataset, options, errors)
  
  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .mlRegRandomForestContainerMain( jaspResults, options, mlRegRandomForestResults)
  .mlRegRandomForestTableSummary(jaspResults, options, mlRegRandomForestResults)
  
  return()
}

# Init functions ----
.mlRegRandomForestInitOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis
  # options: e.g., nr of predictors should not be larger than nr of predictors given, percentage should betw. 0 and 1
  options
}

# Preprocessing functions ----
.mlRegRandomForestReadData <- function(dataset, options) {
  
  # Read in the dataset using the built-in functions
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    return(.readDataSetToEnd(columns.as.numeric = options$target, columns = options$predictors))
  }
}

.mlRegRandomForestErrorHandling <- function(dataset, options) {
  
  # Check if results can be computed
  if (length(options$target) == 0 || length(options$predictors) == 0) return("No variables")

  # Error Check 1: 0 observations for the target variable
  errors <- .hasErrors(
    dataset = dataset, 
    perform = "run", 
    type = c('observations', 'variance', 'infinity'),
    all.target = options$target,
    observations.amount = '< 1',
    exitAnalysisIfErrors = TRUE)

  errors
}

# Results functions ----
.mlRegRandomForestComputeResults <- function(jaspResults, dataset, options, errors) {

  if (!is.null(errors) && errors == "No variables") return()
  
  if (is.null(jaspResults[["stateMlRegRandomForestResults"]])) {
    mlRegRandomForestResults <- .mlRegRandomForestResultsHelper(dataset, options)
    
    jaspResults[["stateMlRegRandomForestResults"]] <- createJaspState(mlRegRandomForestResults)
    jaspResults[["stateMlRegRandomForestResults"]]$dependOnOptions("predictors")
    
  } else {
    mlRegRandomForestResults <- jaspResults[["stateMlRegRandomForestResults"]]$object
  }
  mlRegRandomForestResults
}

.mlRegRandomForestResultsHelper <- function(dataset, options) {
  
  results <- list() # return object
  
  # Defaults for everything set to "auto"
  if (options$noOfTrees == "auto") {
    options$noOfTrees <- 500
  } else if (options$noOfTrees == "manual") {
    options$noOfTrees <- as.integer(options$noOfTrees)
  }
  
  if (options$noOfPredictors == "auto") {
    options$noOfPredictors <- max(c(floor(length(unlist(options$noOfPredictors)) / 3), 1))
  } else if (options$noOfPredictors == "manual") {
    options$noOfPredictors <- as.integer(options$noOfPredictors)
  }
  
  if (options$dataBootstrapModel == "auto") { # does this give an error if set to a number higher than 1 or below 0?
    options$dataBootstrapModel <- 1
  } else if (options$dataBootstrapModel == "manual") {
    options$dataBootstrapModel <- options$dataBootstrapModel
  }
  
  if (options$dataTrainingModel == "auto") { # aanpassen! check CI whether [0, 1] or [0, 100]
    options$dataTrainingModel <- .8
  } else if (options$dataTrainingModel == "manual") {
    options$dataTrainingModel <- options$dataTrainingModel
  }		
  
  if (options$maximumTerminalNodeSize == "auto") {
    options$maximumTerminalNodeSize <- NULL
  } else if (options$maximumTerminalNodeSize == "manual") {
    options$maximumTerminalNodeSize <- as.integer(options$maximumTerminalNodeSize)
  }
  
  if (options$minimumTerminalNodeSize == "auto") {
    options$minimumTerminalNodeSize <- 5
  } else if (options$minimumTerminalNodeSize == "manual") {
    options$minimumTerminalNodeSize <- as.integer(options$minimumTerminalNodeSize)
  }
  
  # Set seed	
  if (options$seedBox == "manual") {
    set.seed(options$seed)
  } else {
    set.seed(1) # this was set to set.seed(Sys.time()) before, but then results are not reproducible(?)
  }		
  
  preds <- which(colnames(dataset) == .v(options$predictors)) # predictors
  target <- which(colnames(dataset) == .v(options$target)) # target
  
  # Splitting the data into a training and a test set
  n <- nrow(dataset)

  idxTrain <- sample(1:n, floor(options$dataTrainingModel*n))
  idxTest <- (1:n)[-idxTrain]
  
  predsTrain <- dataset[idxTrain, preds, drop = FALSE]
  targetTrain <- dataset[idxTrain, target]
  predsTest <- dataset[idxTest, preds, drop = FALSE]
  targetTest <- dataset[idxTest, target]
  
  # Run Random Forest
  res <- randomForest::randomForest(
    x = predsTrain,
    y = targetTrain,
    xtest = predsTest,
    ytest = targetTest,
    ntree = options$noOfTrees,
    mtry = options$noOfPredictors,
    nodesize = options$minimumTerminalNodeSize,
    maxnodes = options$maximumTerminalNodeSize,
    importance = TRUE, # options[["importance"]], # calc importance between rows. Always calc it, only show on user click.
    proximity = FALSE, # options[["proximity"]], # calc proximity between rows. Always calc it, only show on user click.
    keep.forest = TRUE, # should probably always be TRUE (otherwise partialPlot can't be called)
    na.action = randomForest::na.roughfix
  )
  
  # Compile results object
  results <- list(res = res, 
                  mse = res$mse,
                  data = list(predsTrain = predsTrain, targetTrain = targetTrain, 
                              predsTest = predsTest, targetTest = targetTest))

  return(results)
}

# Output functions ----
.mlRegRandomForestContainerMain <- function(jaspResults, options, mlRegRandomForestResults) {
  if (!is.null(jaspResults[["mlRegRandomForestMainContainer"]])) return()
  
  mainContainer <- createJaspContainer("Random Forest Model")
  mainContainer$dependOnOptions(c("noOfTrees", "noOfPredictors"))
  
  jaspResults[["mlRegRandomForestMainContainer"]] <- mainContainer
}

.mlRegRandomForestTableSummary <- function(jaspResults, options, mlRegRandomForestResults) {
  if (!is.null(jaspResults[["mlRegRandomForestMainContainer"]][["mlRegRandomForestTable"]])) return()

  # Below is one way of creating a table
  mlRegRandomForestTable <- createJaspTable(title = "Random Forest Model")
  
  # Bind table to jaspResults
  jaspResults[["mlRegRandomForestMainContainer"]][["mlRegRandomForestTable"]] <- mlRegRandomForestTable
  
  # Add column info
  mlRegRandomForestTable$addColumnInfo(name = "MSE",  title = "MSE", type = "number", format = "sf:4")
  
  # Add data per column
  mlRegRandomForestTable[["MSE"]]  <- mlRegRandomForestResults$mse
}
