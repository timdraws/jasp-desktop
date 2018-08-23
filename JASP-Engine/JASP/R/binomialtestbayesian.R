#
# Copyright (C) 2015 University of Amsterdam
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

BinomialTestBayesian <- function(jaspResults, dataset, options, state = NULL) {
	
  # Update options
  
  if (options$bayesFactorType == "BF01") {
    
    options[["BFH1H0"]] <- FALSE
    
    if (options$hypothesis == "notEqualToTestValue") {
      options[["bf.title"]] <- "BF\u2080\u2081"
    } else if (options$hypothesis == "greaterThanTestValue") {
      options[["bf.title"]] <- "BF\u2080\u208A"
    } else if (options$hypothesis == "lessThanTestValue") {
      options[["bf.title"]] <-  "BF\u2080\u208B"
    }
    
  } else if (options$bayesFactorType == "BF10") {
    
    options[["BFH1H0"]] <- TRUE
    
    if (options$hypothesis == "notEqualToTestValue"){
      options[["bf.title"]] <- "BF\u2081\u2080"
    } else if (options$hypothesis == "greaterThanTestValue"){
      options[["bf.title"]] <- "BF\u208A\u2080"
    } else if (options$hypothesis == "lessThanTestValue"){
      options[["bf.title"]] <- "BF\u208B\u2080"
    }
    
  } else if (options$bayesFactorType == "LogBF10") {
    
    options[["BFH1H0"]] <- TRUE
    
    if (options$hypothesis == "notEqualToTestValue"){
      options[["bf.title"]] <- "Log(\u0042\u0046\u2081\u2080)"
    } else if (options$hypothesis == "greaterThanTestValue"){
      options[["bf.title"]] <-"Log(\u0042\u0046\u208A\u2080)"
    } else if (options$hypothesis == "lessThanTestValue"){
      options[["bf.title"]] <- "Log(\u0042\u0046\u208B\u2080)"
    }
  }
  
  # Read dataset
	if (is.null(dataset)) {
	  dataset <- .readDataSetToEnd(columns.as.numeric = NULL, columns.as.factor = options$variables, exclude.na.listwise = NULL)
	} else {
	  dataset <- .vdf(dataset, columns.as.numeric = NULL, columns.as.factor = options$variables)
	}

  # Set title
	jaspResults$title <- "Bayesian Binomial Test"
	
	# Check if results can be computed
	ready <- (length(options$variables) > 0)
	
	# Check for errors
	if (ready) {
	  
	  # Error check 1: 0 observations for a level of a variable
	  for (variable in options$variables) {
	    
	    column <- dataset[[ .v(variable) ]]
	    data <- column[!is.na(column)]
	    levels <- levels(data)
	    
	    for (level in levels) {
	      .hasErrors(data[data == level], perform = "run", type = 'observations',
	                 observations.amount = c('< 1'), exitAnalysisIfErrors = TRUE)
	    }
	  }
	  
	  # Error check 2: Test value should match hypotheses
	  if (options$testValue == 1 && options$hypothesis == "greaterThanTestValue") {
	    .quitAnalysis(message = "The hypothesis that the test value is greater than 1 cannot be tested.")
	  } else if (options$testValue == 0 && options$hypothesis == "lessThanTestValue") {
	    .quitAnalysis(message = "The hypothesis that the test value is less than 0 cannot be tested.")
	  }
	}
	
	# Create Bayesian Binomial Table
	.createBayesianBinomialTable(jaspResults = jaspResults, dataset = dataset, options = options, ready = ready)
	
	# Create Bayesian Binomial Plots Container (if wanted and if results can be computed)
	if ((options$plotPriorAndPosterior == TRUE || options$plotSequentialAnalysis == TRUE) && ready == TRUE) {
	  .createBayesianBinomialPlotsContainerTotal(jaspResults = jaspResults, dataset = dataset, options = options)
	}
	
	# Bring state up-to-date
	state[["options"]] <- options
	
	return(state = state)
}

.createBayesianBinomialTable <- function(jaspResults, dataset, options, ready) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["bayesianBinomialTable"]])) {
    return(NULL)
  }
  
  # Create table
  bayesianBinomialTable <- createJaspTable(title = "Bayesian Binomial Test")
  jaspResults[["bayesianBinomialTable"]] <- bayesianBinomialTable
  bayesianBinomialTable$showSpecifiedColumnsOnly <- TRUE
  bayesianBinomialTable$dependOnOptions(c("variables", "testValue", "priorA", "priorB", "hypothesis", "bayesFactorType"))
  
  # Add columns to table
  bayesianBinomialTable$addColumnInfo(name = "variable",   title = "",           type = "string", combine = TRUE)
  bayesianBinomialTable$addColumnInfo(name = "level",      title = "Level",      type = "string")
  bayesianBinomialTable$addColumnInfo(name = "counts",     title = "Counts",     type = "integer")
  bayesianBinomialTable$addColumnInfo(name = "total",      title = "Total",      type = "integer")
  bayesianBinomialTable$addColumnInfo(name = "proportion", title = "Proportion", type = "number", format = "sf:4;dp:3")
  bayesianBinomialTable$addColumnInfo(name = "bf",         title = bf.title,     type = "number", format = "sf:4;dp:3")
  
  # Fill up table with results
  .fillUpBayesianBinomialTable(bayesianBinomialTable = bayesianBinomialTable, dataset = dataset, options = options, ready = ready)
  
  return(NULL)
}

.fillUpBayesianBinomialTable <- function(bayesianBinomialTable, dataset, options, ready) {
  
  # If results can be computed, compute them and add row for each level of each variable
  if (ready == TRUE) {
    
    for (variable in options$variables) {
      
      # Prepare for running the binomial test
      column <- dataset[[ .v(variable) ]]
      data <- column[!is.na(column)]
      levels <- levels(data)
      
      for (level in levels) {
        .addRowForBayesianBinomialTable(bayesianBinomialTable = bayesianBinomialTable, data = data, options = options,
                                        variable = variable, level = level)
      }
    }
    
    # Add footnote: Alternative hypothesis
    if (options$hypothesis == "notEqualToTestValue") {
      message <- paste0("Proportions tested against value: ", options$testValue, ".")
    } else if (options$hypothesisRec == "greaterThanTestValue") {
      message <- paste0("For all tests, the alternative hypothesis specifies that the proportion is greater than ", options$testValue, ".")
    } else if (options$hypothesisRec == "lessThanTestValue") {
      message <- paste0("For all tests, the alternative hypothesis specifies that the proportion is less than ", options$testValue, ".")
    }
    bayesianBinomialTable$addFootnote(message = message, symbol = "<em>Note.</em>")
    
    # If results cannot be computed, add an empty row
  } else {
    row <- list(variable = ".", level = ".", counts = ".", total = ".", proportion = ".", bf = ".")
    bayesianBinomialTable$addRows(row)
  }

  return(NULL)
}

.addRowForBayesianBinomialTable <- function(bayesianBinomialTable, data, options, variable = variable, level = level) {
  
  # Compute results except for Bayes Factor
  counts     <- sum(data == level)
  total      <- length(data)
  proportion <- counts/total
  
  # Compute Bayes Factor
  BF10 <- .bayesBinomialTest(counts = counts, total = total, theta0 = options$testValue, hypothesis = options$hypothesis, a = options$priorA, b = options$priorB)
  bf <- BF10
  if (options$bayesFactorType == "BF01") {
    bf <- 1/BF10
  } else if (options$bayesFactorType == "LogBF10") {
    bf <- log(BF10)
  }
  
  # Add row to table
  row <- list(variable = .clean(variable), level = .clean(level), counts = .clean(counts), total = .clean(total), 
              proportion = .clean(proportion), bf = .clean(bf))
  bayesianBinomialTable$addRows(row, rowNames = paste0(variable, " - ", level))
  
  return(NULL)
}
  



#.bayesBinomialTest.twoSided.jeffreys <- function(counts, total, theta0) {
#	# assuming a = b = 1, i.e., uniform prior
#	
#	logBF01 <- lgamma(total + 2) - lgamma(counts + 1) - lgamma(total - counts + 1) + counts*log(theta0) + (total - counts)*log(1 - theta0)
#	BF10 <- 1 / exp(logBF01)
#	
#	return(BF10)
#	
#}

.bayesBinomialTest.twoSided <- function(counts, total, theta0, a, b) {
	
	if (theta0 == 0 && counts == 0) {
	
		# in this case, counts*log(theta0) should be zero, omit to avoid numerical issue with log(0)
		logBF10 <- lbeta(counts + a, total - counts + b) -  lbeta(a, b) - (total - counts)*log(1 - theta0)
		
	} else if (theta0 == 1 && counts == total) {
	
		# in this case, (total - counts)*log(1 - theta0) should be zero, omit to avoid numerical issue with log(0)
		logBF10 <- lbeta(counts + a, total - counts + b) -  lbeta(a, b) - counts*log(theta0) 
		
	} else {
	
		logBF10 <- lbeta(counts + a, total - counts + b) -  lbeta(a, b) - counts*log(theta0) - (total - counts)*log(1 - theta0)
	}
	
	BF10 <- exp(logBF10)
	
	return(BF10)
	
}

.bayesBinomialTest.oneSided <- function(counts, total, theta0, a, b, hypothesis) {
  
  if (hypothesis == "lessThanTestValue") {
    
    lowerTail <- TRUE
    
  } else if (hypothesis == "greaterThanTestValue") {
    
    lowerTail <- FALSE
    
  }
  
  if (theta0 == 0 && counts == 0) {
    
    # in this case, counts*log(theta0) should be zero, omit to avoid numerical issue with log(0)
    logMLikelihoodH0 <- (total - counts)*log(1 - theta0)
    
  } else if (theta0 == 1 && counts == total) {
    
    # in this case, (total - counts)*log(1 - theta0) should be zero, omit to avoid numerical issue with log(0)
    logMLikelihoodH0 <- counts*log(theta0)

  } else {
  
  logMLikelihoodH0 <- counts*log(theta0) + (total - counts)*log(1 - theta0)
  
  }
  
  term1 <- pbeta(theta0, a + counts, b + total - counts, lower.tail = lowerTail, log.p = TRUE) +
    lbeta(a + counts, b + total - counts)
  term2 <- lbeta(a,b) + pbeta(theta0, a, b, lower.tail = lowerTail, log.p = TRUE)
  logMLikelihoodH1 <- term1 - term2
  BF10 <- exp(logMLikelihoodH1 - logMLikelihoodH0)
  
  return(BF10)
  
}

.bayesBinomialTest <- function(counts, total, theta0, hypothesis, a, b) {
	
	if (hypothesis == "notEqualToTestValue") {
		
		BF10 <- try(.bayesBinomialTest.twoSided(counts, total, theta0, a, b), silent = TRUE)
		
	} else {
		
			BF10 <- try(.bayesBinomialTest.oneSided(counts, total, theta0, a, b, hypothesis), silent = TRUE)

	}
	
	if (class(BF10) == "try-error")
		BF10 <- NA

	return(BF10)
	
}

#################### Plots ####################

.createBayesianBinomialPlotsContainerTotal <- function(jaspResults, dataset, options){
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(jaspResults[["bayesianBinomialPlotsContainerTotal"]])) {
    return(NULL)
  }
  
  # Create container for all variables
  bayesianBinomialPlotsContainerTotal <- createJaspContainer(title = "Plots")
  jaspResults[["bayesianBinomialPlotsContainerTotal"]] <- bayesianBinomialPlotsContainerTotal
  bayesianBinomialPlotsContainerTotal$dependOnOptions(c("variables", "testValue", "priorA", "priorB",
                                                        "hypothesis", "bayesFactorType", "plotPriorAndPosterior",
                                                        "plotPriorAndPosteriorAdditionalInfo", "plotSequentialAnalysis"))
  
  # Create subcontainer for each variable
  for (variable in options$variables) {
    .createBayesianBinomialPlotsContainerVariable(bayesianBinomialPlotsContainerTotal = bayesianBinomialPlotsContainerTotal,
                                                  dataset = dataset, options = options, variable = variable)
  }
  
  return(NULL)
  
}

.createBayesianBinomialPlotsContainerVariable <- function(bayesianBinomialPlotsContainerTotal, dataset, 
                                                          options, variable) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(bayesianBinomialPlotsContainerTotal[[variable]])) {
    return(NULL)
  }
  
  # Create subcontainer for variable
  .createBayesianBinomialPlotsContainerVariable <- createJaspContainer(title = variable)
  bayesianBinomialPlotsContainerTotal[[variable]] <- .createBayesianBinomialPlotsContainerVariable
  .createBayesianBinomialPlotsContainerVariable$dependOnOptions(c("variables", "testValue", "priorA", "priorB",
                                                                  "hypothesis", "bayesFactorType", "plotPriorAndPosterior",
                                                                  "plotPriorAndPosteriorAdditionalInfo", "plotSequentialAnalysis"))
  
  # Get levels and data for variable
  column <- dataset[[ .v(variable) ]]
  data <- column[!is.na(column)]
  levels <- levels(data)
  
  # For each level, add plot
  for (level in levels) {
    .addBayesianBinomialPlot(bayesianBinomialPlotsContainerVariable = bayesianBinomialPlotsContainerVariable,
                             data = data, options = options, variable = variable, level = level)
  }
  
  return(NULL)
}

.addBayesianBinomialPlot <- function(bayesianBinomialPlotsContainerVariable, data, options,
                                     variable, level) {
  
  # Check if object can be reused (in case relevant options did not change)
  if (!is.null(bayesianBinomialPlotsContainerVariable[[level]])) {
    return(NULL)
  }
  
  # Make plot
  Plot <- .makeBayesianBinomialPlot(data = data, options = options, 
                                    variable = variable, level = level)
  
  # Add plot to container
  bayesianBinomialPriorPosteriorPlotsLevel <- createJaspPlot(plot = descriptivesPlot, title = level)
  bayesianBinomialPlotsContainerVariable[[level]] <- bayesianBinomialPriorPosteriorPlotsLevel
  bayesianBinomialPriorPosteriorPlotsLevel$dependOnOptions(c("variables", "testValue", "priorA", "priorB",
                                                           "hypothesis", "bayesFactorType", "plotPriorAndPosterior",
                                                           "plotPriorAndPosteriorAdditionalInfo", "plotSequentialAnalysis"))
  
  sink("~/Desktop/testcommon.txt", append = T)
  print(bayesianBinomialPriorPosteriorPlotsLevel)
  sink()
  
  return(NULL)
}

.makeBinomialDescriptivesPlot <- function(data, options, variable, level) {
  
  # Define base breaks function for y
  base_breaks_y <- function(x, testValue) {
    d <- data.frame(x = -Inf, xend = -Inf, y = 0, yend = 1)
    list(ggplot2::geom_segment(data = d, ggplot2::aes(x = x, y = y, xend = xend,
                                                      yend = yend),
                               inherit.aes = FALSE, size = 1),
         ggplot2::scale_y_continuous(breaks = c(0,  round(testValue,3), 1)))
  }
  
  # Define plot position
  plotPosition <- ggplot2::position_dodge(0.2)
  
  # Compute data for plot
  nObs <- length(data)
  counts <- sum(data == level)
  proportion <- counts/nObs
  results <- stats::binom.test(x = counts, n = nObs, p = options$testValue, alternative = "notEqualToTestValue", 
                               conf.level = options$descriptivesPlotsConfidenceInterval)
  lowerCI <- results$conf.int[1]
  upperCI <- results$conf.int[2]
  
  summaryStat <- data.frame(label = level, proportion = proportion, lowerCI = lowerCI, upperCI = upperCI)
  dfTestValue <- data.frame(testValue = options$testValue)
  
  # Make plot
  descriptivesPlot <- ggplot2::ggplot(summaryStat, ggplot2::aes(x = label, y = proportion, group = 1)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lowerCI, ymax = upperCI), colour = "black", width = 0.2, 
                           position = plotPosition) +
    ggplot2::geom_point(position = plotPosition, size = 4) +
    ggplot2::geom_hline(data = dfTestValue, ggplot2::aes(yintercept = options$testValue), linetype = "dashed") +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL) +
    ggplot2::theme_bw() +
    ggplot2::ylim(min = 0, max = 1) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 18),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_text(size = 18, vjust = -1),
                   axis.text.x = ggplot2::element_text(size = 15),
                   axis.text.y = ggplot2::element_text(size = 15),
                   panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   legend.title = ggplot2::element_text(size = 12),
                   legend.text = ggplot2::element_text(size = 12),
                   axis.ticks = ggplot2::element_line(size = 0.5),
                   axis.ticks.margin = grid::unit(1, "mm"),
                   axis.ticks.length = grid::unit(3, "mm"),
                   plot.margin = grid::unit(c(0.5, 0, 0.5, 0.5), "cm")) +
    base_breaks_y(summaryStat, dfTestValue$testValue)
  
  # Return plot
  return(descriptivesPlot)
}

.dpriorTheta <- function(x, a = 1, b = 1, hypothesis = "notEqualToTestValue", theta0 = .5) {
	
	if (hypothesis == "notEqualToTestValue") {
		
		dbeta(x, a, b)
		
	} else if (hypothesis == "greaterThanTestValue") {
		
		ifelse (x >= theta0,
				dbeta(x, a, b) / pbeta(theta0, a, b, lower.tail = FALSE),
				0)
		
	} else if (hypothesis == "lessThanTestValue") {
		
		ifelse (x <= theta0,
				dbeta(x, a, b) / pbeta(theta0, a, b),
				0)
	
	}
	
}

.dposteriorTheta <- function(x, a = 1, b = 1, counts = 10, total = 20, hypothesis = "notEqualToTestValue", theta0 = .5) {
	
	if (hypothesis == "notEqualToTestValue") {
		
		dbeta(x, a + counts, b + total - counts)
		
	} else if (hypothesis == "greaterThanTestValue") {
		
		ifelse (x >= theta0,
				dbeta(x, a + counts, b + total - counts) / pbeta(theta0, a + counts, b + total - counts, lower.tail = FALSE),
				0)
		
	} else if (hypothesis == "lessThanTestValue") {
		
		ifelse (x <= theta0,
				dbeta(x, a + counts, b + total - counts) / pbeta(theta0, a + counts, b + total - counts),
				0)
	
	}
	
}

.credibleIntervalPlusMedian <- function(credibleIntervalInterval = .95, a = 1, b = 1, counts = 10, total = 20, hypothesis = "notEqualToTestValue", theta0 = .5) {
	
	lower <- (1 - credibleIntervalInterval) / 2
	upper <- 1 - lower
	
	if (hypothesis == "notEqualToTestValue") {
		
		quantiles <- qbeta(c(lower, .5, upper), a + counts , b + total - counts)
		
	} else if (hypothesis == "greaterThanTestValue") {
		
		rightArea <- pbeta(theta0, a + counts , b + total - counts, lower.tail = FALSE)
		leftArea <- 1 - rightArea
		quantiles <- qbeta(leftArea + rightArea * c(lower, .5, upper), a + counts , b + total - counts)
		
	} else if (hypothesis == "lessThanTestValue") {
		
		leftArea <- pbeta(theta0, a + counts , b + total - counts)
		quantiles <- qbeta(leftArea * c(lower, .5, upper), a + counts , b + total - counts)
		
	}
	
	return(list(ci.lower = quantiles[1], ci.median = quantiles[2], ci.upper = quantiles[3]))
	
}

.plotPosterior.binomTest <- function(counts, total, theta0, a = 1, b = 1, BF10, hypothesis = "notEqualToTestValue",
									 addInformation = TRUE, dontPlotData = FALSE,
									 lwd = 2, cexPoints = 1.5, cexAxis = 1.2, cexYlab = 1.5,
									 cexXlab = 1.5, cexTextBF = 1.4, cexCI = 1.1, cexLegend = 1.2,
									 lwdAxis = 1.2, credibleIntervalInterval = .95) {
	
	
	if (addInformation) {
	
		par(mar = c(5.6, 5, 7, 4) + 0.1, las = 1)
		drawCI <- TRUE
	
	} else {
	
		par(mar = c(5.6, 5, 4, 4) + 0.1, las = 1)
		drawCI <- FALSE
	}
	
	
	if (dontPlotData) {
	
		plot(1, type = "n", xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, xlab = "", ylab = "")
		
		axis(1, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, xlab = "")
		axis(2, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, ylab = "")
		
		mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 3.25)
		mtext(expression("Population proportion" ~ theta), side = 1, cex = cexXlab, line = 2.6)
	
		return()
	}
	
	if (is.infinite(BF10))
		stop("Bayes factor is infinite")
	
	if (is.infinite(1/BF10))
		stop("Bayes factor is too small")
	
	# set limits plot
	
	xlim <- c(0, 1)
	
	if (hypothesis == "notEqualToTestValue") {
	
		stretch <- 1.35
		
	} else if (hypothesis == "greaterThanTestValue") {
	
		stretch <- 1.45
		
	} else if (hypothesis == "lessThanTestValue") {
	
		stretch <- 1.45
	}
	
	# calculate position of "nice" tick marks and create labels
	
	xticks <- seq(0, 1, 0.2)
	xlabels <- c("0", "0.2", "0.4", "0.6", "0.8", "1")
	
	# compute 95% credible interval & median:
	
	quantiles <- try(.credibleIntervalPlusMedian(credibleIntervalInterval = credibleIntervalInterval,
					 a = a, b = b, counts = counts, total = total, hypothesis, theta0 = theta0),
					 silent = TRUE)
	
	if (class(quantiles) == "try-error") {
		
		drawCI <- FALSE
		
	} else {
		
		CIlow <- quantiles$ci.lower
		medianPosterior <- quantiles$ci.median
		CIhigh <- quantiles$ci.upper
	}
	
	if (a == 1 && b == 1) {
		
		theta <- seq(0, 1, length.out = 1000)
		
	} else {
		
		theta <- seq(0.001, 0.999, length.out = 1000)
	}
	
	priorLine <- .dpriorTheta(theta, a, b, hypothesis, theta0)
	posteriorLine <- .dposteriorTheta(theta, a, b, counts, total, hypothesis, theta0)
	
	dmax <- max(c(posteriorLine[is.finite(posteriorLine)], priorLine[is.finite(priorLine)]))
	
	ylim <- vector("numeric", 2)
	ylim[1] <- 0
	ylim[2] <- stretch * dmax
	
	yticks <- pretty(ylim)
	
	ylim <- range(yticks)
	ylabels <- formatC(yticks, 1, format = "f")
	
	
	plot(1, xlim = xlim, ylim = range(yticks), ylab = "", xlab = "", type = "n", axes = FALSE)
	
	lines(theta, posteriorLine, lwd = lwd)
	lines(theta, priorLine, lwd = lwd, lty = 3)
	
	axis(1, at = xticks, labels = xlabels, cex.axis = cexAxis, lwd = lwdAxis)
	axis(2, at = yticks, labels = ylabels, cex.axis = cexAxis, lwd = lwdAxis)
	
	
	if (nchar(ylabels[length(ylabels)]) > 4) {
		
		mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 4)
		
	} else if (nchar(ylabels[length(ylabels)]) == 4) {
		
		mtext(text = "Density", side = 2, las=0, cex = cexYlab, line = 3.25)
		
	} else if (nchar(ylabels[length(ylabels)]) < 4) {
		
		mtext(text = "Density", side = 2, las = 0, cex = cexYlab, line = 2.85)
	}
	
	mtext(expression("Population proportion" ~ theta), side = 1, cex = cexXlab, line = 2.6)
	
	
	evalPosterior <- posteriorLine[posteriorLine > 0]
	
	if (theta0 == 0) {
		
		heightPriorTheta0 <- priorLine[1]
		heightPosteriorTheta0 <- posteriorLine[1]
		
		points(theta[1], heightPriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
		points(theta[1], heightPosteriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
	
	} else if (theta0 == 1) {
		
		heightPriorTheta0 <- priorLine[length(priorLine)]
		heightPosteriorTheta0 <- posteriorLine[length(posteriorLine)]
		
		points(theta[length(theta)], heightPriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
		points(theta[length(theta)], heightPosteriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
	
	} else {
	
		heightPriorTheta0 <- .dpriorTheta(theta0, a, b, hypothesis, theta0)
		heightPosteriorTheta0 <- .dposteriorTheta(theta0, a, b, counts, total, hypothesis, theta0)
		
		points(theta0, heightPriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
		points(theta0, heightPosteriorTheta0, col = "black", pch = 21, bg = "grey", cex = cexPoints)
	}
	
	par(xpd = TRUE) # enable plotting in margin
	
	yCI <- grconvertY(dmax, "user", "ndc") + 0.04
	yCI <- grconvertY(yCI, "ndc", "user")
	
	if (drawCI) {
		
		arrows(CIlow, yCI, CIhigh, yCI, angle = 90, code = 3, length = 0.1, lwd = lwd)
		medianText <- formatC(medianPosterior, digits = 3, format = "f")
	}
	
	if (addInformation) {
		
		BF01 <- 1 / BF10
		
		# display BF10 value
		
		offsetTopPart <- 0.06
		
		yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
		
		xx <- min(xticks)
		
		if (BF10 >= 1000000 || BF01 >= 1000000) {
		
			BF10t <- format(BF10, digits = 4, scientific = TRUE)
			BF01t <- format(BF01, digits = 4, scientific = TRUE)
		}
		
		if (BF10 < 1000000 && BF01 < 1000000) {
		
			BF10t <- formatC(BF10,3, format = "f")
			BF01t <- formatC(BF01,3, format = "f")
		}
		
		if (hypothesis == "notEqualToTestValue") {
			
			text(xx, yy2, bquote(BF[10] == .(BF10t)), cex = cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0][1] == .(BF01t)), cex = cexTextBF, pos = 4)
		
		} else if (hypothesis == "greaterThanTestValue") {
			
			text(xx, yy2, bquote(BF["+"][0] == .(BF10t)), cex = cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["+"] == .(BF01t)), cex = cexTextBF, pos = 4)
		
		} else if (hypothesis == "lessThanTestValue") {
			
			text(xx, yy2, bquote(BF["-"][0] == .(BF10t)), cex= cexTextBF, pos = 4)
			text(xx, yy, bquote(BF[0]["-"] == .(BF01t)), cex= cexTextBF, pos = 4)
		}
		
		yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
		
		if (drawCI) {
			
			CIText <- paste("95% CI: [",  bquote(.(formatC(CIlow, 3, format = "f"))), ", ",  bquote(.(formatC(CIhigh, 3, format = "f"))), "]", sep = "")
			medianLegendText <- paste("median =", medianText)
			
			text(max(xticks), yy2, medianLegendText, cex = 1.1, pos = 2)
			text(max(xticks), yy, CIText, cex = 1.1, pos = 2)
		}
		
		### probability wheel ###
		
		if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
			xx <- grconvertX(0.44, "ndc", "user")
		} else if (max(nchar(BF10t), nchar(BF01t)) == 5) {
			xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
		} else if (max(nchar(BF10t), nchar(BF01t)) == 6) {
			xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
		} else if (max(nchar(BF10t), nchar(BF01t)) == 7) {
			xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		} else if (max(nchar(BF10t), nchar(BF01t)) == 8) {
			xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		} else if (max(nchar(BF10t), nchar(BF01t)) > 8) {
			xx <- grconvertX(0.44 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
		}
		
		yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
		
		# make sure that colored area is centered
		
		radius <- 0.06 * diff(range(xticks))
		A <- radius^2 * pi
		alpha <- 2 / (BF01 + 1) * A / radius^2
		startpos <- pi/2 - alpha/2
		
		# draw probability wheel
		
		plotrix::floating.pie(xx, yy,c(BF10, 1),radius = radius, col = c("darkred", "white"), lwd = 2,startpos = startpos)
		
		yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
		yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
		
		if (hypothesis == "notEqualToTestValue") {
			
			text(xx, yy, "data|H1", cex = cexCI)
			text(xx, yy2, "data|H0", cex = cexCI)
			
		} else if (hypothesis == "greaterThanTestValue") {
			
			text(xx, yy, "data|H+", cex = cexCI)
			text(xx, yy2, "data|H0", cex = cexCI)
			
		} else if (hypothesis == "lessThanTestValue") {
			
			text(xx, yy, "data|H-", cex = cexCI)
			text(xx, yy2, "data|H0", cex = cexCI)
		}
	}
	
	if (medianPosterior >= .5) {
		
		legendPosition <- min(xticks)
 		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty = c(1,3), bty = "n", lwd = c(lwd,lwd), cex = cexLegend, xjust = 0, yjust = 1)
		
	} else {
		
		legendPosition <- max(xticks)
 		legend(legendPosition, max(yticks), legend = c("Posterior", "Prior"), lty = c(1,3), bty = "n", lwd = c(lwd,lwd), cex = cexLegend, xjust = 1, yjust = 1)
	}
	
}

.plotSequentialBF.binomTest <- function(d, level, theta0 = .5, a = 1, b = 1, BF10table, hypothesis = "notEqualToTestValue",
										callback = function(...) 0, lwd = 2, cexPoints = 1.4, cexAxis = 1.2,
										cexYlab = 1.5, cexXlab = 1.6, cexTextBF = 1.4, cexText = 1.2,
										cexLegend = 1.2, cexEvidence = 1.6, lwdAxis = 1.2, plotDifferentPriors = FALSE,
										dontPlotData = FALSE, BFH1H0 = TRUE) {
	
	
	#### settings ####
	
	if ( ! plotDifferentPriors) {
		
		evidenceText <-  TRUE
		
	} else {
		
		evidenceText <-  FALSE
	}
	
	
	par(mar = c(5.6, 6, 7, 7) + 0.1, las = 1)
	
	if (dontPlotData) {
		
		plot(1, type = 'n', xlim = 0:1, ylim = 0:1, bty = "n", axes = FALSE, xlab = "", ylab = "")
		
		axis(1, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, xlab = "")
		axis(2, at = 0:1, labels = FALSE, cex.axis = cexAxis, lwd = lwdAxis, ylab = "")
		
		mtext("n", side = 1, cex = cexXlab, line= 2.5)
		
		if (hypothesis == "notEqualToTestValue") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las = 0, cex = cexYlab, line = 3.1)
			}
		}
		
		if (hypothesis == "greaterThanTestValue") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las=0, cex = cexYlab, line= 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las=0, cex = cexYlab, line= 3.1)
			}
		}
		
		if (hypothesis == "lessThanTestValue") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las = 0, cex = cexYlab, line = 3.1)
			}
		}
		
		return()
	}
	
	if (is.infinite(BF10table))
		stop("Bayes factor is infinite")
	
	if (is.infinite(1/BF10table))
		stop("Bayes factor is too small")
	
	# convert to zero's and one's (ones denote "successes") with respect to the level of interest
	
	x <- ifelse (d == level, 1, 0)
	
	BF10 <- vector("numeric", length(x))
	
	for (i in seq_along(x)) {
		
		counts <- sum(x[1:i] == 1)
		total <- length(x[1:i])
		BF10[i] <- .bayesBinomialTest(counts, total, theta0, hypothesis, a = a, b = b)
		
		if (is.na(BF10[i]))
			stop("One or more Bayes factors cannot be computed")
		
		if (is.infinite(BF10[i]))
			stop("One or more Bayes factors are infinity")
	}
	
	if (BFH1H0) {
	
		BF <- BF10
		
	} else {
	
		BF <- 1 / BF10
	}
	
	bfAxis <- .scaleBFaxis(BF)
	
	yAt <- bfAxis$yAt
	yLab <- bfAxis$yLab
	omit3s <- bfAxis$omit3s
	
# creating plots
	
	xLab <- pretty(c(0, length(BF10)+2))
	xlim <- range(xLab)
	ylow <- log(eval(parse(text= yLab[1])))
	yhigh <- log(eval(parse(text= yLab[length(yLab)])))
	
	if (is.infinite(yhigh))
		yhigh <- 1e+308
	
	ylim <- c(ylow, yhigh)
	
	plot(1, 1, xlim = xlim, ylim = ylim, ylab = "", xlab = "", type = "n", axes = FALSE)
	
	
	for (i in seq_along(yAt))
		lines(x = xlim, y = rep(yAt[i], 2), col = "darkgrey", lwd = 1.3, lty = 2)
	
	lines(xlim, rep(0, 2), lwd = lwd)
	
	axis(1, at = xLab, labels = xLab, cex.axis = cexAxis, lwd = lwdAxis)
	axis(2, at = yAt, labels = yLab, cex.axis = cexAxis, lwd = lwdAxis)
	
	par(xpd = TRUE) # enable plotting in margin
	
	xx <- grconvertX(0.79, "ndc", "user")
	
	yAthigh <- yAt[yAt >= 0]
	
	if (!omit3s & eval(parse(text = yLab[1])) >= 1/300 & eval(parse(text = yLab[length(yLab)])) <= 300) {
		
		for (i in 1:(length(yAthigh)-1)) {
			
			yy <- mean(c(yAthigh[i], yAthigh[i+1]))
			
			if (yAthigh[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos = 4, cex = cexText)
			}
			if (yAthigh[i] == log(3)) {
				text(x = xx, yy,"Moderate", pos = 4, cex = cexText)
			}
			if (yAthigh[i] == log(10)) {
				text(x = xx, yy,"Strong", pos = 4, cex = cexText)
			}
			if (yAthigh[i] == log(30)) {
				text(x = xx, yy,"Very strong", pos = 4, cex = cexText)
			}
			if (yAthigh[i] == log(100)) {
				text(x = xx, yy,"Extreme", pos = 4, cex = cexText)
			}
		}
		
		yAtlow <- rev(yAt[yAt <= 0])
		
		for (i in 1:(length(yAtlow)-1)) {
			
			yy <- mean(c(yAtlow[i], yAtlow[i+1]))
			
			if (yAtlow[i] == log(1)) {
				text(x = xx, yy,"Anecdotal", pos = 4, cex = cexText)
			}
			if (yAtlow[i] == log(1/3)) {
				text(x = xx, yy,"Moderate", pos = 4, cex = cexText)
			}
			if (yAtlow[i] == log(1/10)) {
				text(x = xx, yy,"Strong", pos = 4, cex = cexText)
			}
			if (yAtlow[i] == log(1/30)) {
				text(x = xx, yy,"Very strong", pos = 4, cex = cexText)
			}
			if (yAtlow[i] == log(1/100)) {
				text(x = xx, yy,"Extreme", pos = 4, cex = cexText)
			}
		}
		
		if ( ! .shouldContinue(callback()))
			return()
		
		axis(side = 4, at = yAt, tick = TRUE, las = 2, cex.axis = cexAxis, lwd = lwdAxis, labels = FALSE, line = -0.6)
		
		xx <- grconvertX(0.96, "ndc", "user")
		yy <- grconvertY(0.5, "npc", "user")
		
		text(xx, yy, "Evidence", srt = -90, cex = cexEvidence)
	}
	
	if (omit3s) {
		
		if (hypothesis == "notEqualToTestValue") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las = 0, cex = cexYlab, line = 4.3)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las = 0, cex = cexYlab, line = 4.3)
			}
		}
		
		if (hypothesis == "greaterThanTestValue") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las = 0, cex = cexYlab, line = 4.3)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las = 0, cex = cexYlab, line = 4.3)
			}
		}
		
		if (hypothesis == "lessThanTestValue") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las = 0, cex = cexYlab, line = 4.3)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las = 0, cex = cexYlab, line = 4.3)
			}
		}
	}
	
	if (omit3s == FALSE) {
		
		if (hypothesis == "notEqualToTestValue") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF[1][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0][1]), side = 2, las = 0, cex = cexYlab, line = 3.1)
			}
		}
		
		if (hypothesis == "greaterThanTestValue") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["+"][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["+"]), side = 2, las = 0, cex = cexYlab, line = 3.1)
			}
		}
		
		if (hypothesis == "lessThanTestValue") {
			
			if (BFH1H0) {
				
				mtext(text = expression(BF["-"][0]), side = 2, las = 0, cex = cexYlab, line = 3.1)
				
			} else {
				
				mtext(text = expression(BF[0]["-"]), side = 2, las=0, cex = cexYlab, line = 3.1)
			}
		}
	}
	
	mtext("n", side = 1, cex = cexXlab, line= 2.5)
	
	xx <- grconvertX(0.1, "npc", "user")
	yy1 <- yAt[length(yAt)-1]
	yy2 <- yAt[length(yAt)]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4* diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd = lwd)
	
	xxt <- grconvertX(0.28, "npc", "user")
	
	if (hypothesis == "notEqualToTestValue") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		}
	}
	
	if (hypothesis == "greaterThanTestValue") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		}
	}
	
	if (hypothesis == "lessThanTestValue") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
		}
	}
	
	
	yy1 <- yAt[2]
	yy2 <- yAt[1]
	yya1 <- yy1 + 1/4 * diff(c(yy1, yy2))
	yya2 <- yy1 + 3/4 * diff(c(yy1, yy2))
	
	arrows(xx, yya1, xx, yya2, length = 0.1, code = 2, lwd = lwd)
	
	if (hypothesis == "notEqualToTestValue") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H1", cex = cexText)
		}
	}
	
	if (hypothesis == "greaterThanTestValue"){
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H+", cex = cexText)
		}
	}
	
	if (hypothesis == "lessThanTestValue") {
		
		if (BFH1H0) {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H0", cex = cexText)
			
		} else {
			
			text(xxt, mean(c(yya1, yya2)), labels = "Evidence for H-", cex = cexText)
		}
	}
	
	
	BF10e <- BF10table
	BF01e <- 1 / BF10e
	
	# display BF10 value
	
	offsetTopPart <- 0.06
	
	xx <- min(xLab)
	yy <- grconvertY(0.75 + offsetTopPart, "ndc", "user")
	yy2 <- grconvertY(0.806 + offsetTopPart, "ndc", "user")
	
	if (BF10e >= 1000000 | BF01e >= 1000000) {
		
		BF10t <- formatC(BF10e,3, format = "e")
		BF01t <- formatC(BF01e,3, format = "e")
	}
	
	if (BF10e < 1000000 & BF01e < 1000000) {
		
		BF10t <- formatC(BF10e, 3, format = "f")
		BF01t <- formatC(BF01e, 3, format = "f")
	}
	
	if (hypothesis == "notEqualToTestValue") {
		
		text(xx, yy2, bquote(BF[10] == .(BF10t)), cex = cexTextBF, pos = 4, offset = -.2)
		text(xx, yy, bquote(BF[0][1] == .(BF01t)), cex = cexTextBF, pos = 4, offset = -.2)
	}
	
	if (hypothesis == "greaterThanTestValue") {
		
		text(xx, yy2, bquote(BF["+"][0] == .(BF10t)), cex = cexTextBF, pos = 4, offset = -.2)
		text(xx, yy, bquote(BF[0]["+"] == .(BF01t)), cex = cexTextBF, pos = 4, offset = -.2)
	}
	
	if (hypothesis == "lessThanTestValue") {
		
		text(xx, yy2, bquote(BF["-"][0] == .(BF10t)), cex = cexTextBF, pos = 4, offset = -.2)
		text(xx, yy, bquote(BF[0]["-"] == .(BF01t)), cex = cexTextBF, pos = 4, offset = -.2)
	}
	
	
	# probability wheel
	
	if (max(nchar(BF10t), nchar(BF01t)) <= 4) {
		xx <- grconvertX(0.44, "ndc", "user")
	} else if (max(nchar(BF10t), nchar(BF01t)) == 5) {
		xx <- grconvertX(0.44 +  0.001* 5, "ndc", "user")
	} else if (max(nchar(BF10t), nchar(BF01t)) == 6) {
		xx <- grconvertX(0.44 + 0.001* 6, "ndc", "user") 
	} else if (max(nchar(BF10t), nchar(BF01t)) == 7) {
		xx <- grconvertX(0.44 + 0.002* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
	} else if (max(nchar(BF10t), nchar(BF01t)) == 8) {
		xx <- grconvertX(0.44 + 0.003* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
	} else if (max(nchar(BF10t), nchar(BF01t)) > 8) {
		xx <- grconvertX(0.445 + 0.005* max(nchar(BF10t), nchar(BF01t)), "ndc", "user") 
	}
	
	yy <- grconvertY(0.788 + offsetTopPart, "ndc", "user")
	
	
	# make sure that colored area is centered
	
	radius <- grconvertX(0.2, "ndc", "user") - grconvertX(0.16, "ndc", "user")
	A <- radius^2*pi
	alpha <- 2 / (BF01e + 1) * A / radius^2
	startpos <- pi/2 - alpha/2
	
	if ( ! .shouldContinue(callback()))
		return()
	
	# draw probability wheel
	
	plotrix::floating.pie(xx, yy,c(BF10e, 1),radius = radius, col = c("darkred", "white"), lwd = 2, startpos = startpos)
	
	yy <- grconvertY(0.865 + offsetTopPart, "ndc", "user")
	yy2 <- grconvertY(0.708 + offsetTopPart, "ndc", "user")
	
	if (hypothesis == "notEqualToTestValue") {
		
		text(xx, yy, "data|H1", cex = 1.1)
		text(xx, yy2, "data|H0", cex =  1.1)
	}
	
	if (hypothesis == "greaterThanTestValue") {
		
		text(xx, yy, "data|H+", cex =  1.1)
		text(xx, yy2, "data|H0", cex =  1.1)
	}
	
	if (hypothesis == "lessThanTestValue") {
		
		text(xx, yy, "data|H-", cex =  1.1)
		text(xx, yy2, "data|H0", cex =  1.1)
	}
	
	if (length(BF10) <= 60) {
		
		points(log(BF), pch = 21, bg = "grey", cex = cexPoints, lwd = 1.3)
		
	} else {
		
		lines(log(BF), col = "black", lwd = 2.7)
	}
	
	# Determine the strength of the evidence that the Bayes Factor conveys (currently not implemented as an option)
	
	BFevidence <- BF10e
	
	if (evidenceText) {
		
		if (BF10e < 1)
			BFevidence <- 1 / BF10e
		
		if (BFevidence >= 1 & BFevidence <= 3) {
			lab <- "Anecdotal"
		} else if (BFevidence > 3 & BFevidence <= 10) {
			lab <- "Moderate"
		} else if (BFevidence > 10 & BFevidence <= 30) {
			lab <- "Strong"
		} else if (BFevidence > 30 & BFevidence <= 100) {
			lab <- "Very strong"
		} else if (BFevidence > 100) {
			lab <- "Extreme"
		}
		
		xxT <- max(xLab)
		yyT <- grconvertY(0.775 + offsetTopPart, "ndc", "user")
		
		if (BF10e >= 1) {
			
			if (hypothesis == "notEqualToTestValue") {
				text(xxT, yyT, paste("Evidence for H1:\n", lab), cex = 1.4, pos = 2, offset = -.2)
			} else if (hypothesis == "greaterThanTestValue") {
				text(xxT, yyT, paste("Evidence for H+:\n", lab), cex = 1.4, pos = 2, offset = -.2)
			} else if (hypothesis == "lessThanTestValue") {
				text(xxT, yyT, paste("Evidence for H-:\n", lab), cex = 1.4, pos = 2, offset = -.2)
			}
		}
		
		if (BF10e < 1)
			text(xxT, yyT, paste("Evidence for H0:\n", lab), cex = 1.4, pos = 2, offset = -.2)
	}
}

.scaleBFaxis <- function(BF, callback = function(...) 0) {
	
	# y-axis labels larger than 1
	
	y1h <- "1"
	
	i <- 1
	
	while (eval(parse(text = y1h[i])) < max(BF)) {
		
		if (grepl(pattern = "e",y1h[i])) {
			
			newy <- paste(strsplit(y1h[i], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(y1h[i], split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
			
		} else {
			
			newy <- paste(y1h[i], "0", sep = "")
		}
		
		if (eval(parse(text=newy)) >= 10^6) {
			
			newy <- format(as.numeric(newy), digits = 3, scientific = TRUE)
		}
		
		y1h <- c(y1h, newy)
		i <- i + 1
	}
	
	y3h <- "3"
	
	i <- 1
	
	while (eval(parse(text = y3h[i])) < max(BF)) {
		
		if (grepl(pattern = "e",y3h[i])) {
			
			newy <- paste(strsplit(y3h[i], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(y3h[i], split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
		} else {
			
			newy <- paste(y3h[i], "0", sep = "")
		}
		
		if (as.numeric(newy) >= 10^6) {
			
			newy <- format(as.numeric(newy), digits = 3, scientific = TRUE)
		}
		
		y3h <- c(y3h, newy)
		
		i <- i + 1
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	yhigh <- vector("numeric", length(y1h) + length(y3h))
	
	o <- 1
	e <- 1
	
	for (i in seq_along(yhigh)) {
		
		if (i %% 2 == 1) {
			
			yhigh[i] <- y1h[o]
			o <- o + 1
		}
		
		if (i %% 2 == 0) {
			
			yhigh[i] <- y3h[e]
			e <- e + 1
		}
	} 
	
	yhighLab <- as.character(yhigh)
	
	
	# y-axis labels smaller than 1
	
	y1l <- "1/1"
	
	i <- 1
	
	while (eval(parse(text = y1l[i])) > min(BF)) {
		
		if (grepl(pattern = "e",y1l[i])) {
			
			newy <- paste(strsplit(y1l[i], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(y1l[i], split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
		} else {
			
			newy <- paste(y1l[i], "0", sep = "")
		}
		
		if (eval(parse(text= newy)) <= 10^(-6)) {
			
			newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}
		
		y1l <- c(y1l, newy)
		i <- i + 1
	}
	
	y3l <- "1/3"
	
	i <- 1
	
	while (eval(parse(text= y3l[i])) > min(BF)) {
		
		if (grepl(pattern = "e",y3l[i])) {
			
			newy <- paste(strsplit(y3l[i], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(y3l[i], split = "+", fixed = TRUE)[[1]][2])+1, sep = "")
		} else {
			
			newy <- paste(y3l[i], "0", sep = "")
		}
		
		if (newy == "1/3e+9") {
			
			newy <- "1/3e+09"
		}
		
		if (eval(parse(text = newy)) <= 10^(-6) & eval(parse(text = newy)) > 10^(-9)) {
			
			newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
			newy <- paste(substring(newy, 1, nchar(newy)-1), as.numeric(substring(newy, nchar(newy), nchar(newy))) - 1, sep = "")
			newy <- sub(".33", "", newy)
			newy <-  sub("-", "+", x = newy)
			newy <- paste0("1/", newy)
		}
		
		y3l <- c(y3l, newy)
		i <- i + 1
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	ylow <- vector("numeric", length(y1l) + length(y3l))
	o <- 1
	e <- 1
	
	for (i in seq_along(ylow)) {
		
		if (i %% 2 == 1) {
			
			ylow[i] <- y1l[o]
			o <- o + 1
		}
		
		if (i %% 2 == 0) {
			
			ylow[i] <- y3l[e]
			e <- e + 1
		}
	}
	
	yLab <- c(rev(ylow[-1]), yhighLab)
	
	
	# remove 3's if yLab vector is too long
	omit3s <- FALSE
	
	if (length(yLab) > 9) {
		
		omit3s <- TRUE
		
		ind <- which(yLab == "3")
		
		yLabsHigh <- yLab[ind:length(yLab)]
		
		if (length(yLabsHigh) > 1) {
			
			yLabsHigh <- yLabsHigh[seq(2, length(yLabsHigh), 2)]
			
		} else {
			
			yLabsHigh <- character(0)
		}
		
		yLabsLow <- yLab[1:(ind-1)]
		yLabsLow <- yLabsLow[-grep(pattern = "/3", x = yLab)]
		
		yLab1s <- c(yLabsLow, yLabsHigh)
		
		
		if (max(BF) > eval(parse(text = yLab1s[length(yLab1s)-1]))) {
			
			#for (i in 1) {
				
				if(grepl(pattern = "e", yLab1s[length(yLab1s)])){
					
					newy <-  paste(strsplit(yLab1s[length(yLab1s)], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[length(yLab1s)], split = "+", fixed = TRUE)[[1]][2])+1, sep = "")
					
				} else {
					
					newy <- paste(yLab1s[length(yLab1s)], "0", sep = "")
				}
				
				if (eval(parse(text = newy)) >= 10^6) {
					
					newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
				}
				
				yLab1s <- c(yLab1s, newy)
			#}
		}
		
		if (yLab1s[1] == "1") {
			
			yLab1s <- c(paste0(yLab1s[1], "/", "10"), yLab1s)
		}
		
		if (yLab1s[length(yLab1s)] == "1") {
			
			yLab1s <- c(yLab1s, "10")
		}
		
		if (min(BF) < eval(parse(text = yLab1s[2]))) {
			
			#for (i in 1:2) {
				
				if (grepl(pattern = "e",yLab1s[1])) {
					
					newy <- paste(strsplit(yLab1s[1], split = "+", fixed = TRUE)[[1]][1], "+", as.numeric(strsplit(yLab1s[1],split = "+", fixed = TRUE)[[1]][2]) + 1, sep = "")
					
				} else {
					
					newy <- paste(yLab1s[1], "0", sep = "")
				}
				
				if (eval(parse(text = newy)) <= 10^(-6)) {
					
					newy <- format(eval(parse(text = newy)), digits = 3, scientific = TRUE)
					newy <-  sub("-", "+", x = newy)
					newy <- substring(newy, nchar(newy) - 4, nchar(newy))
					newy <- paste0("1/", newy)
				}
			#}
			
			yLab1s <- c(newy, yLab1s)
		}
		
		yLab <- yLab1s
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	while (length(yLab) > 9) {
		
		ind <- which(yLab == "1")
		
		if (ind == 1) {
			
			yLabLow <- character(0)
			
		} else {
			
			yLabLow <- yLab[1:(ind-1)]
		}
		
		if (ind == length(yLab)) {
			
			yLabHigh <- character(0)
			
		} else {
			
			yLabHigh <- yLab[(ind+1):length(yLab)]
		}
		
		if (length(yLabLow) > 1) {
			
			yLabLow <- yLabLow[seq(length(yLabLow)-1, 1, -2)]
			
		} else {
			
			yLabLow <- yLabLow
		}
		
		if (length(yLabHigh) > 1) {
			
			yLabHigh <- yLabHigh[seq(2, length(yLabHigh), 2)]
			
		} else {
			
			yLabHigh <- yLabHigh
		}
		
		if (length(yLabLow) == 1) {
			
			yLabLow <- paste("1/", yLabHigh[1], sep = "")
		}
		
		if (length(yLabHigh) == 1) {
			
			yLabHigh <- strsplit(x = yLabLow[1], "/", fixed = TRUE)[[1]][2]
		}
		
		yLab <- c(rev(yLabLow), "1", yLabHigh)
	}
	
	
	while (eval(parse(text=yLab[1])) > min(BF)) {
		
		for (i in 1:2) {
			
			interval <- as.numeric(strsplit(yLab[1], "+", fixed = TRUE)[[1]][2]) - as.numeric(strsplit(yLab[2], "+", fixed = TRUE)[[1]][2])
			pot <- as.numeric(strsplit(yLab[1], "+", fixed = TRUE)[[1]][2]) + interval
			
			newy <- paste(strsplit(yLab[1], "+", fixed = TRUE)[[1]][1], "+", pot, sep = "")
			yLab <- c(newy, yLab)
		}
	}
	
	while (eval(parse(text = yLab[length(yLab)])) < max(BF)) {
		
		for (i in 1:2) {
			
			interval <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed = TRUE)[[1]][2]) - as.numeric(strsplit(yLab[length(yLab)-1], "+", fixed = TRUE)[[1]][2])
			pot <- as.numeric(strsplit(yLab[length(yLab)], "+", fixed = TRUE)[[1]][2]) + interval
			newy <- paste(strsplit(yLab[length(yLab)], "+", fixed = TRUE)[[1]][1], "+", pot, sep ="")
			yLab <- c( yLab, newy)
		}
	}
	
	if ( ! .shouldContinue(callback()))
		return()
	
	yAt <- vector("numeric", length(yLab))
	
	for (i in seq_along(yLab)) {
		
		yAt[i] <- log(eval(parse(text = yLab[i])))
	}
	
	return(list(yAt = yAt, yLab = yLab, omit3s = omit3s))
}
