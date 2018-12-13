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
  ready <- (!is.null(options$target) && length(.v(options$predictors)) > 0)
  
  # Error checking
  if(ready) errors <- .regRanForErrorHandling(dataset, options)
  
  # Compute (a list of) results from which tables and plots can be created
  if(ready) regRanForResults <- .regRanForComputeResults(jaspResults, dataset, options)

  # Output containers, tables, and plots based on the results. These functions should not return anything!
  .regRanForTable(                jaspResults, options, regRanForResults, ready)
  .regRanForTableVarImportance(   jaspResults, options, regRanForResults, ready)
  .regRanForContainerPlots(       jaspResults, options, regRanForResults, ready)
  .regRanForPlotVarImp(           jaspResults, options, regRanForResults, ready)
  .regRanForPlotTreesVsModelError(jaspResults, options, regRanForResults, ready)
  .regRanForPlotPredPerformance(  jaspResults, options, regRanForResults, ready)

  return()
}

# Check for errors
.regRanForErrorHandling <- function(dataset, options) {

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
.regRanForComputeResults <- function(jaspResults, dataset, options) {
  
  if (!is.null(jaspResults[["stateRegRanForResults"]])) return (jaspResults[["stateRegRanForResults"]]$object)
    
  # Create results object
  results <- list()
  
  results[["spec"]] <- .regRanForCalcSpecs(dataset, options)
  results[["res"]] <- list()
  
  # Set seed	
  if (options$seedBox == "manual") set.seed(options$seed) else set.seed(Sys.time())

  # Prepare data
  preds <- which(colnames(dataset) %in% .v(options$predictors)) # predictors
  target <- which(colnames(dataset) %in% .v(options$target)) # target
  
  # Deal with NAs ("pairwise", "listwise", "impute" => "omit", "rough fix", "impute")
  if (sum(is.na(dataset)) > 0) {
    
    # If a predictor column consists of only NAs, exclude that column entirely
    for (predictor in preds) {
      if(sum(is.na(dataset[,preds])) == nrow(dataset)) {
        preds <- preds[-predictor]
      } 
    }
    
    # Option: apply na.roughfix (see ?na.roughfix)
    if (options$missingValues == "roughfix") {
      dataset <- randomForest::na.roughfix(dataset)
      
    # Otherwise appy the default: remove all rows that contain NA values
    } else {
      dataset <- na.omit(dataset)
    } 
    
  }
  
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
    na.action = results$spec$missingValues
  )
  
  results[["data"]] <- list(xTrain = xTrain, yTrain = yTrain, xTest = xTest, yTest = yTest)
  
  # Save results to state
  jaspResults[["stateregRanForResults"]] <- createJaspState(results)
  jaspResults[["stateregRanForResults"]]$dependOnOptions(
    c("target", "predictors", "indicator", "noOfTrees", "noOfPredictors", "dataTrainingModel", "dataBootstrapModel", 
      "maximumTerminalNodeSize", "minimumTerminalNodeSize", "seedBox")
  )

  return(results)
}

.regRanForCalcSpecs <- function(dataset, options) {
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
.regRanForTable <- function(jaspResults, options, regRanForResults, ready) {
  if (!is.null(jaspResults[["regRanForTable"]])) return()
  
  # Create table and bind to jaspResults
  regRanForTable <- createJaspTable(title = "Random Forest Regression Model Summary")
  jaspResults[["regRanForTable"]] <- regRanForTable
  jaspResults[["regRanForTable"]]$dependOnOptions(
    c("target", "predictors", "indicator", "noOfTrees", "noOfPredictors", "dataTrainingModel", 
      "dataBootstrapModel", "maximumTerminalNodeSize", "minimumTerminalNodeSize", "seedBox")
  )
  
  # Add column info
  if(options$dataTrainingModel == "auto" || options$percentageDataTraining < 1){
    regRanForTable$addColumnInfo(name = "testMSE",  title = "Test Set MSE", type = "number", 
                                             format = "sf:4")
  }
  regRanForTable$addColumnInfo(name = "ntrees",  title = "Trees", type = "integer")
  regRanForTable$addColumnInfo(name = "mtry",  title = "Variables tried", type = "integer")
  
  # Add data per column
  regRanForTable[["testMSE"]]  <- if (ready) 
    mean((regRanForResults$res$test$predicted - regRanForResults$data$yTest)^2) else "."
  regRanForTable[["ntrees"]]  <- if (ready) regRanForResults$res$ntree else "."
  regRanForTable[["mtry"]]  <- if (ready) regRanForResults$res$mtry else "."
  
}

.regRanForTableVarImportance <- function(jaspResults, options, regRanForResults, ready) {
  
  if (!is.null(jaspResults[["tableVarImp"]])) return()
  if (!options$tableVariableImportance) return()
  
  # Create table
  regRanForTableVarImp <- createJaspTable(title = "Variable Importance")
  jaspResults[["tableVarImp"]] <- regRanForTableVarImp
  jaspResults[["tableVarImp"]]$dependOnOptions(
    c("target", "predictors", "indicator", "noOfTrees", "noOfPredictors", "dataTrainingModel", "dataBootstrapModel", 
      "maximumTerminalNodeSize", "minimumTerminalNodeSize", "seedBox")
  )
  
  # Add column info
  regRanForTableVarImp$addColumnInfo(name = "predictor",  title = " ", type = "string")
  regRanForTableVarImp$addColumnInfo(name = "MDiA",  title = "Mean decrease in accuracy", type = "number", format = "sf:4")
  regRanForTableVarImp$addColumnInfo(name = "MDiNI",  title = "Total decrease in node impurity", type = "number", 
                                 format = "sf:4")

  # Ordering the variables according to their mean decrease in accuracy
  if(ready) varImpOrder <- sort(regRanForResults$res$importance[,1], decr = FALSE, index.return = T)$ix
  
  # Add data per column
  regRanForTableVarImp[["predictor"]]  <- if(ready) 
    .unv(names(regRanForResults$res$importance[varImpOrder, 1])) else "."
  regRanForTableVarImp[["MDiA"]]  <- if(ready) regRanForResults$res$importance[varImpOrder, 1] else "."
  regRanForTableVarImp[["MDiNI"]]  <- if(ready) regRanForResults$res$importance[varImpOrder, 2] else "."
  
}

.regRanForContainerPlots <- function(jaspResults, options, regRanForResults, ready) {
  if (!any(options$plotVariableImportance, options$plotTreesVsModelError, options$plotPredictivePerformance)) return()
  if (!ready) return()
  
  if (is.null(jaspResults[["containerPlots"]])) {
    jaspResults[["containerPlots"]] <- createJaspContainer("Random Forest Plots")
    jaspResults[["containerPlots"]]$dependOnOptions(c("plotVariableImportance", "plotTreesVsModelError", 
                                                      "plotPredictivePerformance"))
  }
}

.regRanForPlotVarImp <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotVariableImportance) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  varImpOrder <- sort(regRanForResults$res$importance[,1], decr = T, index.return = T)$ix
  
  varImp <- dplyr::tibble(
    Variable = .unv(names(regRanForResults$res$importance[varImpOrder, 1])),
    MeanIncrMSE = regRanForResults$res$importance[varImpOrder, 1],
    TotalDecrNodeImp = regRanForResults$res$importance[varImpOrder, 2]
  )
  
  varImpPlot1 <- .regRanForVarImpPlot1Helper(options, regRanForResults, varImp)
  varImpPlot2 <- .regRanForVarImpPlot2Helper(options, regRanForResults, varImp)
  pct[['varImpPlot1']] <- createJaspPlot(plot = varImpPlot1, title = "Mean Increase in MSE", 
                                         width = options$plotWidth, height = options$plotHeight)
  pct[['varImpPlot2']] <- createJaspPlot(plot = varImpPlot2, title = "Total Decrease in Node Impurity", 
                                         width = options$plotWidth, height = options$plotHeight)
}

.regRanForVarImpPlot1Helper <- function(options, regRanForResults, varImp) {
  
  varImpPlot1 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImp, ggplot2::aes(x = reorder(Variable, -MeanIncrMSE), y = MeanIncrMSE)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::ylab("Mean Increase in MSE") +
      ggplot2::xlab(NULL) +
      ggplot2::ylim(min(pretty(min(varImp$MeanIncrMSE))), max(pretty(max(varImp$MeanIncrMSE)))) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::coord_flip()
  )
  
  return(varImpPlot1)
}

.regRanForVarImpPlot2Helper <- function(options, regRanForResults, varImp) {
  
  varImpPlot2 <- JASPgraphs::themeJasp(
    ggplot2::ggplot(varImp, ggplot2::aes(x = reorder(Variable, -MeanIncrMSE), y = TotalDecrNodeImp)) +
      ggplot2::geom_bar(stat = "identity", fill = "grey", col = "black", size = .3) +
      ggplot2::ylab("Total Decrease in Node Impurity") +
      ggplot2::xlab(NULL) +
      ggplot2::coord_flip()
  )
  
  return(varImpPlot2)
}

.regRanForPlotTreesVsModelError <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotTreesVsModelError) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  treesMSE <- dplyr::tibble(
    trees = 1:length(regRanForResults$res$mse),
    MSE = regRanForResults$res$mse
  )
  
  plotTreesVsModelError <- .regRanForPlotTreesVsModelErrorHelper(options, regRanForResults, treesMSE)
  pct[['plotTreesVsModelError']] <- createJaspPlot(plot = plotTreesVsModelError, title = "Trees vs MSE", 
                                                width = options$plotWidth, height = options$plotHeight)
}

.regRanForPlotTreesVsModelErrorHelper <- function(options, regRanForResults, treesMSE) {
  
  # Format x ticks
  xlow <- 1
  xhigh <- max(treesMSE$trees)
  xticks <- pretty(c(xlow, xhigh))
  
  # format x labels
  xLabs <- vector("character", length(xticks))
  for (i in seq_along(xticks)) {
    if (xticks[i] < 10^6) {
      xLabs[i] <- format(xticks[i], digits= 3, scientific = FALSE)
    } else {
      xLabs[i] <- format(xticks[i], digits= 3, scientific = TRUE)
    }
  }
  
  # Format y ticks
  ylow <- 0
  yhigh <- max(pretty(treesMSE$MSE))        
  yticks <- pretty(c(ylow, yhigh))
  
  # format y labels
  yLabs <- vector("character", length(yticks))
  for (i in seq_along(yticks)) {
    if (yticks[i] < 10^6) {
      yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
    } else {
      yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
    }
  }
  
  plotTreesVsModelError <- JASPgraphs::themeJasp(
    JASPgraphs::drawAxis(xName = "Number of Trees", yName = "MSE", xBreaks = xticks, yBreaks = yticks, 
                         xLabels = xLabs, yLabels = yLabs, force = TRUE) +
      ggplot2::geom_line(treesMSE, mapping = ggplot2::aes(x = trees, y = MSE), size = 1)
  )
  
  return(plotTreesVsModelError)
}

.regRanForPlotPredPerformance <- function(jaspResults, options, regRanForResults, ready) {
  if (!options$plotPredictivePerformance) return()
  if (!ready) return()
  
  pct <- jaspResults[["containerPlots"]] # create pointer towards main container
  
  if(regRanForResults$spec$dataTrainingModel == 1){
    
    predPerformance <- dplyr::tibble(
      true = regRanForResults$res$predicted,
      predicted = regRanForResults$data$targetTrain) 
    
  } else {
    
    predPerformance <- dplyr::tibble(
      true = regRanForResults$res$test$predicted,
      predicted = regRanForResults$data$yTest) 
    
  }
  
  plotPredPerformance <- .regRanForPlotPredPerformanceHelper(options, regRanForResults, predPerformance)
  pct[['plotPredPerformance']] <- createJaspPlot(plot = plotPredPerformance, title = "Predictive Performance",
                                                 width = options$plotWidth, height = options$plotHeight)
}

.regRanForPlotPredPerformanceHelper <- function(options, regRanForResults, predPerformance) {
  
  # Format x ticks
  xlow <- min(pretty(predPerformance$true))
  xhigh <- max(pretty(predPerformance$true))
  xticks <- pretty(c(xlow, xhigh))
  
  # format x labels
  xLabs <- vector("character", length(xticks))
  for (i in seq_along(xticks)) {
    if (xticks[i] < 10^6) {
      xLabs[i] <- format(xticks[i], digits= 3, scientific = FALSE)
    } else {
      xLabs[i] <- format(xticks[i], digits= 3, scientific = TRUE)
    }
  }
  
  # Format y ticks
  ylow <- min(pretty(predPerformance$predicted))
  yhigh <- max(pretty(predPerformance$predicted))        
  yticks <- pretty(c(ylow, yhigh))
  
  # format y labels
  yLabs <- vector("character", length(yticks))
  for (i in seq_along(yticks)) {
    if (yticks[i] < 10^6) {
      yLabs[i] <- format(yticks[i], digits= 3, scientific = FALSE)
    } else {
      yLabs[i] <- format(yticks[i], digits= 3, scientific = TRUE)
    }
  }
  
  # Taking the broader range for both axes so that all data points will be shown and the plot stays pretty
  if (diff(range(xticks)) > diff(range(yticks))) ticks <- xticks else ticks <- yticks
  if (diff(range(xticks)) > diff(range(yticks))) labs <- xLabs else labs <- yLabs
  
  plotPredPerformance <- JASPgraphs::themeJasp(
    JASPgraphs::drawAxis(xName = "True", yName = "Predicted", xBreaks = ticks, yBreaks = ticks, 
                         xLabels = labs, yLabels = labs, force = TRUE) +
      ggplot2::geom_line(data = dplyr::tibble(x = c(min(ticks), max(ticks)), y = c(min(ticks), max(ticks))), 
                         mapping = ggplot2::aes(x = x, y = y), col = "darkred", size = 1) +
      ggplot2::geom_point(predPerformance, mapping = ggplot2::aes(x = true, y = predicted), size = 3)
  )
  
  return(plotPredPerformance)
}
