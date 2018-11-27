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

MLRegressionRandomForest <- function(jaspResults, dataset, options, ...) {

  # Set title
  jaspResults$title <- "Random Forest Regression"
  
  # Init options: add variables to options to be used in the remainder of the analysis
  options <- .regranforInitOptions(jaspResults, options)
  
  # Sead dataset
  dataset <- .regranforReadData(dataset, options)
  
  # Error checking
  errors <- .regranforErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  regranforResults <- .regranforComputeResults(jaspResults, dataset, options, errors)
  
  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .regranforContainerMain(     jaspResults, options, regranforResults)
  .regranforTableSummary(      jaspResults, options, regranforResults)
  .regranforTableVarImportance(jaspResults, options, regranforResults)
  .regranforContainerPlots(    jaspResults, options, regranforResults, errors)
  .regranforPlotVarImportance( jaspResults, options, regranforResults, errors)

  return()
}

# Init functions ----
.regranforInitOptions <- function(jaspResults, options) {
  # Determine if analysis can be run with user input
  # Calculate any options common to multiple parts of the analysis
  # options: e.g., nr of predictors should not be larger than nr of predictors given, percentage should betw. 0 and 1
  options
}

# Read data
.regranforReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  } else {
    return(.readDataSetToEnd(columns.as.numeric = options$target, columns = options$predictors))
  }
}

# Check for errors
.regranforErrorHandling <- function(dataset, options) {
  
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

# Compute results
.regranforComputeResults <- function(jaspResults, dataset, options, errors) {

  if (!is.null(errors) && errors == "No variables") return()
  
  if (is.null(jaspResults[["stateregranforResults"]])) {
    regranforResults <- .regranforResultsHelper(dataset, options)
    
    jaspResults[["stateregranforResults"]] <- createJaspState(regranforResults)
    jaspResults[["stateregranforResults"]]$dependOnOptions("predictors")
    
  } else {
    regranforResults <- jaspResults[["stateregranforResults"]]$object
  }
  regranforResults
}

.regranforResultsHelper <- function(dataset, options) {

  results <- list() # return object
  
  # Defining the options
  if (options$noOfTrees == "auto") {
    options$noOfTrees <- 500
  } else if (options$noOfTrees == "manual") {
    options$noOfTrees <- as.integer(options$numberOfTrees)
  }
  
  if (options$noOfPredictors == "auto") {
    options$noOfPredictors <- max(c(floor(sqrt(length(.v(options$predictors)))), 1))
  } else if (options$noOfPredictors == "manual") {
    options$noOfPredictors <- as.integer(options$numberOfPredictors)
  }
  
  if (options$dataBootstrapModel == "auto") { # does this give an error if set to a number higher than 1 or below 0?
    options$dataBootstrapModel <- 1
  } else if (options$dataBootstrapModel == "manual") {
    options$dataBootstrapModel <- options$percentageDataBootstrap
  }
  
  if (options$dataTrainingModel == "auto") { # aanpassen! check CI whether [0, 1] or [0, 100]
    options$dataTrainingModel <- .8
  } else if (options$dataTrainingModel == "manual") {
    options$dataTrainingModel <- options$percentageDataTraining
  }		
  
  if (options$maximumTerminalNodeSize == "auto") {
    options$maximumTerminalNodeSize <- NULL
  } else if (options$maximumTerminalNodeSize == "manual") {
    options$maximumTerminalNodeSize <- as.integer(options$modelMaximumTerminalNode)
  }
  
  if (options$minimumTerminalNodeSize == "auto") {
    options$minimumTerminalNodeSize <- 5
  } else if (options$minimumTerminalNodeSize == "manual") {
    options$minimumTerminalNodeSize <- as.integer(options$modelMinimumTerminalNode)
  }
  
  # Set seed	
  if (options$seedBox == "manual") {
    set.seed(options$seed)
  } else {
    set.seed(1) # this was set to set.seed(Sys.time()) before, but then results are not reproducible(?)
  }		

  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) %in% .v(options$target)) # target
  
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
    importance = TRUE,
    proximity = options$calculateProximityMatrix,
    keep.forest = TRUE,
    na.action = randomForest::na.roughfix
  )
  
  # Compile results object
  results <- list(res = res, data = list(predsTrain = predsTrain, targetTrain = targetTrain, predsTest = predsTest, 
                                         targetTest = targetTest))

  return(results)
}

# Output functions
.regranforContainerMain <- function(jaspResults, options, regranforResults) {
  if (!is.null(jaspResults[["regranforMainContainer"]])) return()
  
  mainContainer <- createJaspContainer("Random Forest Model")
  mainContainer$dependOnOptions(c("noOfTrees", "noOfPredictors"))
  
  jaspResults[["regranforMainContainer"]] <- mainContainer
}

.regranforTableSummary <- function(jaspResults, options, regranforResults) {
  if (!is.null(jaspResults[["regranforMainContainer"]][["regranforTable"]])) return()

  # Below is one way of creating a table
  regranforTableSumm <- createJaspTable(title = "Summary")
  
  # Bind table to jaspResults
  jaspResults[["regranforMainContainer"]][["regranforTableSumm"]] <- regranforTableSumm
  
  # Add column info
  if(options$dataTrainingModel == "auto" || options$percentageDataTraining < 1){
    regranforTableSumm$addColumnInfo(name = "testMSE",  title = "Test Set MSE", type = "number", 
                                             format = "sf:4")
  }
  regranforTableSumm$addColumnInfo(name = "ntrees",  title = "Trees", type = "integer")
  regranforTableSumm$addColumnInfo(name = "mtry",  title = "Variables tried", type = "integer")
  
  # Add data per column
  regranforTableSumm[["testMSE"]]  <- mean((regranforResults$res$test$predicted - 
                                                      regranforResults$data$targetTest)^2)
  regranforTableSumm[["ntrees"]]  <- regranforResults$res$ntree
  regranforTableSumm[["mtry"]]  <- regranforResults$res$mtry
}

.regranforTableVarImportance <- function(jaspResults, options, regranforResults) {
  
  if (!options$tableVariableImportance) return()
  
  # Below is one way of creating a table
  regranforTableVI <- createJaspTable(title = "Variable Importance")
  
  # Bind table to jaspResults
  jaspResults[["regranforMainContainer"]][["regranforTableVI"]] <- regranforTableVI
  
  # Add column info
  regranforTableVI$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regranforTableVI$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number", format = "sf:4")
  regranforTableVI$addColumnInfo(name = "MDiNI",  title = "Mean decrease in node impurity", type = "number", 
                                 format = "sf:4")

  # Ordering the variables according to their mean decrease in accuracy
  varImportanceOrder <- sort(regranforResults$res$importance[,1], decr=T, index.return=T)$ix
  
  # Add data per column
  regranforTableVI[["predictor"]]  <- .unv(names(regranforResults$res$importance[varImportanceOrder, 1]))
  regranforTableVI[["MDiA"]]  <- regranforResults$res$importance[varImportanceOrder, 1]
  regranforTableVI[["MDiNI"]]  <- regranforResults$res$importance[varImportanceOrder, 2]
  
}

.regranforContainerPlots <- function(jaspResults, options, regranforResults, errors) {
  if (!any(options$plotVariableImportance, options$plotTreesVsModelError, options$plotPredictivePerformance)) return()
  if (!is.null(errors) && errors == "No variables") return()
  
  if (is.null(jaspResults[["containerPlots"]])) {
    jaspResults[["containerPlots"]] <- createJaspContainer("Random Forest Plots")
    jaspResults[["containerPlots"]]$dependOnOptions(c("plotVariableImportance", "plotTreesVsModelError", 
                                                      "plotPredictivePerformance"))
  }
}

.regranforPlotVarImportance <- function(jaspResults, options, regranforResults, errors) {
  if (options$plotVariableImportance == FALSE) return()
  if (!is.null(errors) && errors == "No variables") return()
  
  # Create pointer towards main container
  pct <- jaspResults[["containerPlots"]]
  
  varImportancePlot <- .regranforPlotVarImportanceHelper(options$plotVariableImportanceShowValues, regranforResults)
  #obj <- ggplot2::qplot(varImportancePlot[, 1], varImportancePlot[, 2])
  pct[['varImportancePlot']] <- createJaspPlot(plot = varImportancePlot, title = "Variable Importance Plot", 
                                               width = 160, height = 320)
}

.regranforPlotVarImportanceHelper <- function(options, regranforResults) {

  varImportanceOrder <- sort(regranforResults$res$importance[,1], decr=T, index.return=T)$ix
  
  varImportance <- dplyr::tibble(
    Variable = names(regranforResults$res$importance[varImportanceOrder, 1]),
    IncMeanAcc = regranforResults$res$importance[varImportanceOrder, 1],
    IncNodePurity = regranforResults$res$importance[varImportanceOrder, 2]
  )
  
  varImportancePlot <- ggplot2::ggplot(data = varImportance, mapping = ggplot2::aes(y = Variable, x = IncMeanAcc)) +
    ggplot2::geom_point()
  
  return(varImportancePlot)
}
