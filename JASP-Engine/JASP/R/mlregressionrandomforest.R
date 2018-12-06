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

  # Read dataset
  if(options$target == "") options$target <- NULL
  dataset <- .readDataSetToEnd(columns.as.numeric = options$target, columns = options$predictors)

  # Check if results can be computed
  ready <- (!is.null(options$target) && length(options$predictors) > 0)
  
  # Error checking
  if(ready) errors <- .regranforErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if(ready) regranforResults <- .regranforComputeResults(jaspResults, dataset, options)

  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .regranforContainerMain(        jaspResults, options, regranforResults)
  .regranforTable(                jaspResults, options, regranforResults, ready)
  .regranforTableVarImportance(   jaspResults, options, regranforResults, ready)
  .regranforContainerPlots(       jaspResults, options, regranforResults, ready)
  .regranforPlotVarImportance(    jaspResults, options, regranforResults, ready)
  .regranforPlotTreesVsModelError(jaspResults, options, regranforResults, ready)

  return()
}

# Check for errors
.regranforErrorHandling <- function(dataset, options) {

  # Error Check 1: 0 observations for the target variable
  .hasErrors(
    dataset = dataset, 
    perform = "run", 
    type = c('observations', 'variance', 'infinity'),
    all.target = options$target,
    observations.amount = '< 1',
    exitAnalysisIfErrors = TRUE)

}

# Compute results
.regranforComputeResults <- function(jaspResults, dataset, options) {
  
  if (is.null(jaspResults[["stateregranforResults"]])) {
    regranforResults <- .regranforResultsHelper(jaspResults, dataset, options)
    
    jaspResults[["stateregranforResults"]] <- createJaspState(regranforResults)
    jaspResults[["stateregranforResults"]]$dependOnOptions("predictors")
    
  } else {
    regranforResults <- jaspResults[["stateregranforResults"]]$object
  }
  regranforResults
}

.regranforResultsHelper <- function(jaspResults, dataset, options) {

  results <- list()
  
  results[["spec"]] <- .regranforCalcSpecs(dataset, options)
  results[["res"]] <- list()
  
  # Set seed	
  if (options$seedBox == "manual") set.seed(options$seed) else set.seed(Sys.time())

  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) %in% .v(options$target)) # target
  
  # Splitting the data into a training and a test set
  n <- nrow(dataset)

  idxTrain <- sample(1:n, floor(results$spec$dataTrainingModel*n))
  idxTest <- (1:n)[-idxTrain]
  
  xTrain <- dataset[idxTrain, preds, drop = FALSE]
  yTrain <- dataset[idxTrain, target]
  xTest <- dataset[idxTest, preds, drop = FALSE]
  yTest <- dataset[idxTest, target]
  
  # Run Random Forest
  results[["res"]] <- randomForest::randomForest(
    x = xTrain,
    y = yTrain,
    xtest = xTest,
    ytest = yTest,
    ntree = results$spec$noOfTrees,
    mtry = results$spec$noOfPredictors,
    sampsize = results$spec$dataBootstrapModel,
    nodesize = results$spec$minimumTerminalNodeSize,
    maxnodes = results$spec$maximumTerminalNodeSize,
    importance = TRUE,
    # proximity = options$calculateProximityMatrix, # let's leave that out for now
    keep.forest = TRUE,
    na.action = randomForest::na.roughfix
  )
  
  results[["data"]] <- list(xTrain = xTrain, yTrain = yTrain, xTest = xTest, yTest = yTest)
  
  # Save results to state
  jaspResults[["stateregranforResults"]] <- createJaspState(results)
  jaspResults[["stateregranforResults"]]$dependOnOptions(
    c("target", "predictors", "indicator", "plotVariableImportance", "plotTreesVsModelError", 
      "plotVariableImportanceShowValues", "plotPredictivePerformance", "noOfTrees", "noOfPredictors", 
      "dataTrainingModel", "dataBootstrapModel", "maximumTerminalNodeSize", "minimumTerminalNodeSize", "seedBox")
  )

  return(results)
}

.regranforCalcSpecs <- function(dataset, options) {
  specs <- list()

  # Setting the number of trees used
  if (options$noOfTrees == "manual") {
    specs$noOfTrees <- as.integer(options$numberOfTrees)
  } else {
    specs$noOfTrees <- 500
  }
  
  # Setting the number of variables considered at each split
  if (options$noOfPredictors == "manual") {
    specs$noOfPredictors <- as.integer(options$numberOfPredictors)
  } else {
    specs$noOfPredictors <- if (!is.null(options$target) && !is.factor(options$target)) 
      max(floor(length(.v(options$predictors))/3), 1) else floor(sqrt(length(.v(options$predictors))))
  }
  
  # Specifying what percentage of the data should be used for training
  if (options$dataTrainingModel == "manual") {
    specs$dataTrainingModel <- options$percentageDataTraining
  } else {
    specs$dataTrainingModel <- .8
  }		

  # Specifying what percentage of the data should be used for training
  if (options$dataBootstrapModel == "manual") {
    specs$dataBootstrapModel <- options$percentageDataBootstrap
    } else {
      specs$dataBootstrapModel <- ceiling(.632*nrow(dataset)*specs$dataTrainingModel)
  }

  # Setting the maximum number of terminal nodes
  if (options$maximumTerminalNodeSize == "manual") {
    specs$maximumTerminalNodeSize <- as.integer(options$modelMaximumTerminalNode)
  } else {
    specs$maximumTerminalNodeSize <- NULL
  }
  
  # Setting the minimum size of terminal nodes
  if (options$minimumTerminalNodeSize == "manual") {
    specs$minimumTerminalNodeSize <- as.integer(options$modelMinimumTerminalNode)
  } else {
    specs$minimumTerminalNodeSize <- if (!is.null(options$target) && !is.factor(options$target)) 5 else 1
  }
  
  return(specs)
}

# Output functions
.regranforContainerMain <- function(jaspResults, options, regranforResults) {
  if (!is.null(jaspResults[["regranforMainContainer"]])) return()
  
  mainContainer <- createJaspContainer("Random Forest Model")
  mainContainer$dependOnOptions(c("noOfTrees", "noOfPredictors", "numberOfPredictors"))
  # mainContainer$setOptionMustContainDependency("noOfPredictors")
  
  jaspResults[["regranforMainContainer"]] <- mainContainer
}

.regranforTable <- function(jaspResults, options, regranforResults, ready) {
  if (!is.null(jaspResults[["regranforMainContainer"]][["tableSummary"]])) return()
  
  # Create table and bind to jaspResults
  regranforTable <- createJaspTable(title = "Summary")
  jaspResults[["regranforMainContainer"]][["tableSummary"]] <- regranforTable
  
  # Add column info
  if(options$dataTrainingModel == "auto" || options$percentageDataTraining < 1){
    regranforTable$addColumnInfo(name = "testMSE",  title = "Test Set MSE", type = "number", 
                                             format = "sf:4")
  }
  regranforTable$addColumnInfo(name = "ntrees",  title = "Trees", type = "integer")
  regranforTable$addColumnInfo(name = "mtry",  title = "Variables tried", type = "integer")
  
  # Add data per column
  regranforTable[["testMSE"]]  <- if (ready) 
    mean((regranforResults$res$test$predicted - regranforResults$data$yTest)^2) else "."
  regranforTable[["ntrees"]]  <- if (ready) regranforResults$res$ntree else "."
  regranforTable[["mtry"]]  <- if (ready) regranforResults$res$mtry else "."
  
}

.regranforTableVarImportance <- function(jaspResults, options, regranforResults, ready) {
  
  if (!is.null(jaspResults[["regranforMainContainer"]][["tableVariableImportance"]])) return()
  if (!options$tableVariableImportance) return()
  
  regranforTableVI <- createJaspTable(title = "Variable Importance")
  jaspResults[["regranforMainContainer"]][["tableVariableImportance"]] <- regranforTableVI
  
  # Add column info
  regranforTableVI$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regranforTableVI$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number", format = "sf:4")
  regranforTableVI$addColumnInfo(name = "MDiNI",  title = "Total decrease in node impurity", type = "number", 
                                 format = "sf:4")

  # Ordering the variables according to their mean decrease in accuracy
  if(ready) varImportanceOrder <- sort(regranforResults$res$importance[,1], decr = FALSE, index.return = T)$ix
  
  # Add data per column
  regranforTableVI[["predictor"]]  <- if(ready) 
    .unv(names(regranforResults$res$importance[varImportanceOrder, 1])) else "."
  regranforTableVI[["MDiA"]]  <- if(ready) regranforResults$res$importance[varImportanceOrder, 1] else "."
  regranforTableVI[["MDiNI"]]  <- if(ready) regranforResults$res$importance[varImportanceOrder, 2] else "."
  
}

.regranforContainerPlots <- function(jaspResults, options, regranforResults, ready) {
  if (!any(options$plotVariableImportance, options$plotTreesVsModelError, options$plotPredictivePerformance)) return()
  if (!ready) return()
  
  if (is.null(jaspResults[["containerPlots"]])) {
    jaspResults[["containerPlots"]] <- createJaspContainer("Random Forest Plots")
    jaspResults[["containerPlots"]]$dependOnOptions(c("plotVariableImportance", "plotTreesVsModelError", 
                                                      "plotPredictivePerformance"))
  }
}

.regranforPlotVarImportance <- function(jaspResults, options, regranforResults, ready) {
  if (!options$plotVariableImportance) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  varImportanceOrder <- sort(regranforResults$res$importance[,1], decr = T, index.return = T)$ix
  
  varImportance <- dplyr::tibble(
    Variable = .unv(names(regranforResults$res$importance[varImportanceOrder, 1])),
    IncMeanAcc = regranforResults$res$importance[varImportanceOrder, 1],
    IncNodePurity = regranforResults$res$importance[varImportanceOrder, 2]
  )
  
  varImpPlot1 <- .regranforVarImpPlot1Helper(options, regranforResults)
  varImpPlot2 <- .regranforVarImpPlot2Helper(options, regranforResults)
  pct[['varImportancePlot1']] <- createJaspPlot(plot = varImpPlot1, title = "Variable Importance Plot", 
                                               width = 400, height = 300)
  pct[['varImportancePlot2']] <- createJaspPlot(plot = varImpPlot2, title = "Variable Importance Plot", 
                                                width = 400, height = 300)
}

.regranforVarImpPlot1Helper <- function(options, regranforResults) {
  
  plotPosition <- ggplot2::position_dodge(0.2)
  
  varImpPlot1 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImportance, ggplot2::aes(x = reorder(Variable, -IncMeanAcc), y = IncMeanAcc)) +
    ggplot2::geom_bar(stat = "identity", position = plotPosition) +
    ggplot2::ylab("Mean Decrease in Accuracy") +
    ggplot2::xlab(NULL) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::coord_flip()
    )
  
  return(varImpPlot1)
}

.regranforVarImpPlot2Helper <- function(options, regranforResults) {
  
  plotPosition <- ggplot2::position_dodge(0.2)
  varImpPlot2 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImportance, ggplot2::aes(x = reorder(Variable, -IncMeanAcc), y = IncNodePurity)) +
      ggplot2::geom_bar(stat = "identity", position = plotPosition) +
      ggplot2::ylab("Total Decrease in Node Impurity") +
      ggplot2::xlab(NULL) +
      ggplot2::coord_flip()
  )
  
  return(varImpPlot2)
}

.regranforPlotTreesVsModelError <- function(jaspResults, options, regranforResults, ready) {
  if (!options$plotTreesVsModelError) return()
  if (!ready) return()
  
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  plotTreesVsModelError <- .regranforPlotTreesVsModelErrorHelper(options, regranforResults)
  pct[['plotTreesVsModelError']] <- createJaspPlot(plot = plotTreesVsModelError, title = "Trees vs MSE", 
                                                width = 400, height = 300)
}

.regranforPlotTreesVsModelErrorHelper <- function(options, regranforResults) {
  
  plotPosition <- ggplot2::position_dodge(0.2)
  
  plotTreesVsModelError <- randomForest:::plot.randomForest(regranforResults$res)
  
  return(plotTreesVsModelError)
}
