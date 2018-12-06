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

MLClassificationRandomForest <- function(jaspResults, dataset, options, ...) {
  
  # Set title
  jaspResults$title <- "Random Forest Classification"
  
  # Read dataset
  if(options$target == "") options$target <- NULL
  dataset <- .readDataSetToEnd(columns.as.factor = options$target, columns = options$predictors)
  
  # Check if results can be computed
  ready <- (!is.null(options$target) && length(options$predictors) > 0)
  
  # Error checking
  if(ready) errors <- .classranforErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if(ready) classranforResults <- .classranforComputeResults(jaspResults, dataset, options)
  
  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .classranforContainerMain(     jaspResults, options, classranforResults)
  .classranforTable(             jaspResults, options, classranforResults, ready)
  .classranforTableVarImportance(jaspResults, options, classranforResults, ready)
  .classranforContainerPlots(    jaspResults, options, classranforResults, ready)
  .classranforPlotVarImportance( jaspResults, options, classranforResults, ready)
  
  return()
}

# Check for errors
.classranforErrorHandling <- function(dataset, options) {
  
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
.classranforComputeResults <- function(jaspResults, dataset, options) {
  
  if (is.null(jaspResults[["stateclassranforResults"]])) {
    classranforResults <- .classranforResultsHelper(jaspResults, dataset, options)
    
    jaspResults[["stateclassranforResults"]] <- createJaspState(classranforResults)
    jaspResults[["stateclassranforResults"]]$dependOnOptions("predictors")
    
  } else {
    classranforResults <- jaspResults[["stateclassranforResults"]]$object
  }
  classranforResults
}

.classranforResultsHelper <- function(jaspResults, dataset, options) {
  
  results <- list()
  
  results[["spec"]] <- .classranforCalcSpecs(dataset, options)
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
  jaspResults[["stateclassranforResults"]] <- createJaspState(results)
  jaspResults[["stateclassranforResults"]]$dependOnOptions(
    c("target", "predictors", "indicator", "plotVariableImportance", "plotTreesVsModelError", 
      "plotVariableImportanceShowValues", "plotPredictivePerformance", "noOfTrees", "noOfPredictors", 
      "dataTrainingModel", "dataBootstrapModel", "maximumTerminalNodeSize", "minimumTerminalNodeSize", "seedBox")
  )
  
  return(results)
}

.classranforCalcSpecs <- function(dataset, options) {
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
.classranforContainerMain <- function(jaspResults, options, classranforResults) {
  if (!is.null(jaspResults[["classranforMainContainer"]])) return()
  
  mainContainer <- createJaspContainer("Random Forest Model")
  mainContainer$dependOnOptions(c("noOfTrees", "noOfPredictors", "numberOfPredictors"))
  # mainContainer$setOptionMustContainDependency("noOfPredictors")
  
  jaspResults[["classranforMainContainer"]] <- mainContainer
}

.classranforTable <- function(jaspResults, options, classranforResults, ready) {
  if (!is.null(jaspResults[["classranforMainContainer"]][["tableSummary"]])) return()
  
  # Create table and bind to jaspResults
  classranforTable <- createJaspTable(title = "Summary")
  jaspResults[["classranforMainContainer"]][["tableSummary"]] <- classranforTable
  
  # Add column info
  if(options$dataTrainingModel == "auto" || options$percentageDataTraining < 1){
    classranforTable$addColumnInfo(name = "testMSE",  title = "Test Set MSE", type = "number", 
                                 format = "sf:4")
  }
  classranforTable$addColumnInfo(name = "ntrees",  title = "Trees", type = "integer")
  classranforTable$addColumnInfo(name = "mtry",  title = "Variables tried", type = "integer")
  
  # Add data per column
  classranforTable[["testMSE"]]  <- if (ready) 
    mean((classranforResults$res$test$predicted - classranforResults$data$yTest)^2) else "."
  classranforTable[["ntrees"]]  <- if (ready) classranforResults$res$ntree else "."
  classranforTable[["mtry"]]  <- if (ready) classranforResults$res$mtry else "."
  
}

.classranforTableVarImportance <- function(jaspResults, options, classranforResults, ready) {
  
  if (!is.null(jaspResults[["classranforMainContainer"]][["tableVariableImportance"]])) return()
  if (!options$tableVariableImportance) return()
  
  classranforTableVI <- createJaspTable(title = "Variable Importance")
  jaspResults[["classranforMainContainer"]][["tableVariableImportance"]] <- classranforTableVI
  
  # Add column info
  classranforTableVI$addColumnInfo(name = "predictor",  title = " ", type = "string")
  classranforTableVI$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number", format = "sf:4")
  classranforTableVI$addColumnInfo(name = "MDiNI",  title = "Total decrease in node impurity", type = "number", 
                                 format = "sf:4")
  
  # Ordering the variables according to their mean decrease in accuracy
  if(ready) varImportanceOrder <- sort(classranforResults$res$importance[,1], decr = FALSE, index.return = T)$ix
  
  # Add data per column
  classranforTableVI[["predictor"]]  <- if(ready) 
    .unv(names(classranforResults$res$importance[varImportanceOrder, 1])) else "."
  classranforTableVI[["MDiA"]]  <- if(ready) classranforResults$res$importance[varImportanceOrder, 1] else "."
  classranforTableVI[["MDiNI"]]  <- if(ready) classranforResults$res$importance[varImportanceOrder, 2] else "."
  
}

.classranforContainerPlots <- function(jaspResults, options, classranforResults, ready) {
  if (!any(options$plotVariableImportance, options$plotTreesVsModelError, options$plotPredictivePerformance)) return()
  if (!ready) return()
  
  if (is.null(jaspResults[["containerPlots"]])) {
    jaspResults[["containerPlots"]] <- createJaspContainer("Random Forest Plots")
    jaspResults[["containerPlots"]]$dependOnOptions(c("plotVariableImportance", "plotTreesVsModelError", 
                                                      "plotPredictivePerformance"))
  }
}

.classranforPlotVarImportance <- function(jaspResults, options, classranforResults, ready) {
  if (!options$plotVariableImportance) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  varImportancePlot <- .classranforPlotVarImportanceHelper(options, classranforResults)
  pct[['varImportancePlot']] <- createJaspPlot(plot = varImportancePlot, title = "Variable Importance Plot", 
                                               width = 400, height = 300)
}

.classranforPlotVarImportanceHelper <- function(options, classranforResults) {
  
  varImportanceOrder <- sort(classranforResults$res$importance[,1], decr = T, index.return = T)$ix
  
  varImportance <- dplyr::tibble(
    Variable = .unv(names(classranforResults$res$importance[varImportanceOrder, 1])),
    IncMeanAcc = classranforResults$res$importance[varImportanceOrder, 1],
    IncNodePurity = classranforResults$res$importance[varImportanceOrder, 2]
  )
  
  plotPosition <- ggplot2::position_dodge(0.2)
  
  varImpPlot1 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImportance, ggplot2::aes(x = reorder(Variable, -IncMeanAcc), y = IncMeanAcc)) +
      ggplot2::geom_bar(stat = "identity", position = plotPosition) +
      ggplot2::ylab("Mean Decrease in Accuracy") +
      ggplot2::xlab(NULL) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::coord_flip()
  )
  
  varImpPlot2 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImportance, ggplot2::aes(x = reorder(Variable, -IncMeanAcc), y = IncNodePurity)) +
      ggplot2::geom_bar(stat = "identity", position = plotPosition) +
      ggplot2::ylab("Total Decrease in Node Impurity") +
      ggplot2::xlab(NULL) +
      ggplot2::coord_flip()
  )
  
  varImpPlot <- gridExtra::grid.arrange(varImpPlot1, varImpPlot2, ncol = 2)
  
  return(varImpPlot)
}
