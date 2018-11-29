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
    regranforResults <- .regranforResultsHelper(jaspResults, dataset, options)
    
    jaspResults[["stateregranforResults"]] <- createJaspState(regranforResults)
    jaspResults[["stateregranforResults"]]$dependOnOptions("predictors")
    
  } else {
    regranforResults <- jaspResults[["stateregranforResults"]]$object
  }
  regranforResults
}

.regranforResultsHelper <- function(jaspResults, dataset, options) {

  # This will be the object that we fill with results
  results <- list()
  
  # First, we perform precalculation of variables we use throughout the analysis
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
  
  predsTrain <- dataset[idxTrain, preds, drop = FALSE]
  targetTrain <- dataset[idxTrain, target]
  predsTest <- dataset[idxTest, preds, drop = FALSE]
  targetTest <- dataset[idxTest, target]
  
  # Run Random Forest
  results[["res"]] <- randomForest::randomForest(
    x = predsTrain,
    y = targetTrain,
    xtest = predsTest,
    ytest = targetTest,
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
  mainContainer$setOptionMustContainDependency("noOfPredictors", options$noOfPredictors)
  
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
  #if (regranforResults$spec$ready)
  regranforTableSumm[["testMSE"]]  <- mean((regranforResults$res$test$predicted - regranforResults$data$targetTest)^2)
  regranforTableSumm[["ntrees"]]  <- regranforResults$res$ntree
  regranforTableSumm[["mtry"]]  <- regranforResults$res$mtry
}

.regranforTableVarImportance <- function(jaspResults, options, regranforResults) {
  
  if (!options$tableVariableImportance) return()
  
  regranforTableVI <- createJaspTable(title = "Variable Importance")
  jaspResults[["regranforMainContainer"]][["regranforTableVI"]] <- regranforTableVI
  
  # Add column info
  regranforTableVI$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regranforTableVI$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number", format = "sf:4")
  regranforTableVI$addColumnInfo(name = "MDiNI",  title = "Total decrease in node impurity", type = "number", 
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
  if (!options$plotVariableImportance) return()
  if (!is.null(errors) && errors == "No variables") return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  varImportancePlot <- .regranforPlotVarImportanceHelper(options$plotVariableImportanceShowValues, regranforResults)
  pct[['varImportancePlot']] <- createJaspPlot(plot = varImportancePlot, title = "Variable Importance Plot", 
                                               width = 400, height = 300)
}

.regranforPlotVarImportanceHelper <- function(options, regranforResults) {

  varImportanceOrder <- sort(regranforResults$res$importance[,1], decr = T, index.return = T)$ix
  
  varImportance <- dplyr::tibble(
    Variable = .unv(names(regranforResults$res$importance[varImportanceOrder, 1])),
    IncMeanAcc = regranforResults$res$importance[varImportanceOrder, 1],
    IncNodePurity = regranforResults$res$importance[varImportanceOrder, 2]
  )
  
  plotPosition <- ggplot2::position_dodge(0.2)
  
  varImpPlot1 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImportance, ggplot2::aes(x = reorder(Variable, -IncMeanAcc), y = IncMeanAcc)) +
    ggplot2::geom_bar(stat = "identity", position = plotPosition, size = 4) +
    ggplot2::ylab("Mean Decrease in Accuracy") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::xlab(NULL)) +
    ggplot2::coord_flip()# +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      plot.title        = ggplot2::element_text(size = 18),
      panel.grid.major  = ggplot2::element_blank(),
      axis.title.x      = ggplot2::element_text(size = 18, color = "black"),
      axis.text.x       = ggplot2::element_text(size = 15, color = "black"),
      axis.text.y       = ggplot2::element_text(size = 15, color = "black"),
      panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
      axis.ticks        = ggplot2::element_line(size = 0.5),
      axis.ticks.margin = grid::unit(1, "mm"),
      axis.ticks.length = grid::unit(3, "mm"),
      plot.margin       = grid::unit(c(0.5, 0, 0.5, 0.5), "cm")
    )
  
  varImpPlot2 <-
    ggplot2::ggplot(varImportance, ggplot2::aes(x = reorder(Variable, -IncMeanAcc), y = IncNodePurity)) +
    ggplot2::geom_bar(stat = "identity", position = plotPosition, size = 4) +
    ggplot2::ylab("Total Decrease in Node Impurity") +
    ggplot2::xlab(NULL) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      plot.title        = ggplot2::element_text(size = 18),
      panel.grid.major  = ggplot2::element_blank(),
      axis.title.x      = ggplot2::element_text(size = 18, color = "black"),
      axis.text.x       = ggplot2::element_text(size = 15, color = "black"),
      axis.text.y       = ggplot2::element_text(size = 15, color = "black"),
      panel.background  = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.background   = ggplot2::element_rect(fill = "transparent", colour = NA),
      axis.ticks        = ggplot2::element_line(size = 0.5),
      axis.ticks.margin = grid::unit(1, "mm"),
      axis.ticks.length = grid::unit(3, "mm"),
      plot.margin       = grid::unit(c(0.5, 0, 0.5, 0.5), "cm")
    )
  
  varImpPlot <- gridExtra::grid.arrange(varImpPlot1, varImpPlot2, ncol = 2)
  
  return(varImpPlot)
}
