context("Bayesian Independent Samples T-Test")

# does not test
# - bftype (01, 10)
# - missing value exclusion
# - informed prior Normal/t distributions
# - error handling of plots

getTtestTable <- function(x) x[["results"]][["ttestContainer"]][["collection"]][["ttestContainer_ttestTable"]]
getDescriptivesTable <- function(x) x[["results"]][["descriptivesContainer"]][["collection"]][["descriptivesContainer_table"]]

test_that("Main table results match for Student test", {
  set.seed(0)
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$hypothesis <- "groupOneGreater"
  options$effectSizeStandardized <- "informative"
  options$informativeCauchyLocation <- 1
  options$informativeCauchyScale <- 0.5
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  expect_equal_tables(table, list(0.123677493243643, 0.0217437890351034, "contNormal"))
})

test_that("Main table results match for Wilcoxon test", {
  set.seed(0)
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$testStatistic <- "Wilcoxon"
  options$wilcoxonSamplesNumber <- 100
  options$hypothesis <- "groupOneGreater"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  table <- getTtestTable(results)[["data"]]
  expect_equal_tables(table, list(0.354513046015919, 1146, 1.00753043165554, "contNormal"))
})

test_that("Inferential and descriptives plots match", {
  set.seed(0)
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contNormal"
  options$groupingVariable <- "contBinom"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- FALSE
  
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- FALSE
  
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- FALSE
  
  options$descriptives <- TRUE
  options$descriptivesPlots <- TRUE
  options$descriptivesPlotsCredibleInterval <- 0.90
  
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "descriptives", dir="TTestBayesianIndependentSamples")
  
  table <- getDescriptivesTable(results)[["data"]]
  expect_equal_tables(table,
    list(58, 0, -0.362903138386961, -0.120135614827586, 1.10575982846952,
         0.145193378675912, 0.122631908731789, "contNormal", 42, 1, -0.541774532654284,
         -0.283499835571428, 0.994612407217046, 0.15347202634745, -0.0252251384885728,
         "contNormal"),
    label = "Descriptives table"
  )

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior", dir="TTestBayesianIndependentSamples")
  
  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  expect_equal_plots(testPlot, "robustness-check", dir="TTestBayesianIndependentSamples")

  testPlot <- results[["state"]][["figures"]][[4]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis", dir="TTestBayesianIndependentSamples")
  
})

test_that("Inferential plots with additional info match", {
  set.seed(0)
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "contcor1"
  options$groupingVariable <- "facGender"
  options$plotPriorAndPosterior <- TRUE
  options$plotPriorAndPosteriorAdditionalInfo <- TRUE
  
  options$plotBayesFactorRobustness <- TRUE
  options$plotBayesFactorRobustnessAdditionalInfo <- TRUE
  
  options$plotSequentialAnalysis <- TRUE
  options$plotSequentialAnalysisRobustness <- TRUE
  
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  expect_equal_plots(testPlot, "prior-posterior-additional", dir="TTestBayesianIndependentSamples")
  
  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  expect_equal_plots(testPlot, "robustness-check-additional", dir="TTestBayesianIndependentSamples")

  testPlot <- results[["state"]][["figures"]][[3]][["obj"]]
  expect_equal_plots(testPlot, "sequential-analysis-additional", dir="TTestBayesianIndependentSamples")
  
  
})

test_that("Analysis handles errors", {
  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")

  options$variables <- "debInf"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("infinity", notes, ignore.case=TRUE)), label = "Inf check")

  options$variables <- "debSame"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  expect_equal(object = results[["status"]], expected = "validationError",
  	label = "Variance check"
  )
  expect_false(startsWith(results[["results"]][["errorMessage"]], "This analysis terminated unexpectedly."))

  options$variables <- "debMiss99"
  options$groupingVariable <- "contBinom"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  notes <- unlist(getTtestTable(results)[["footnotes"]])
  expect_true(any(grepl("observations", notes, ignore.case=TRUE)), label = "Too few obs check")

  options$dependent <- "contNormal"
  options$groupingVariable <- "debSame"
  results <- jasptools::run("TTestBayesianIndependentSamples", "test.csv", options)
  msg <- results[["results"]][["errorMessage"]]
  expect_true(any(grepl("levels", msg, ignore.case=TRUE)), label = "1-level factor check")
})

test_that("Analysis handles integer overflow", {

  set.seed(4491)
  dat <- data.frame(dependent_var = rnorm(2e5),
                    grouping      = rep(c(1, 2), each = 1e5))

  options <- jasptools::analysisOptions("TTestBayesianIndependentSamples")
  options$variables <- "dependent_var"
  options$groupingVariable <- "grouping"
  results <- jasptools::run("TTestBayesianIndependentSamples", dat, options)

  table <- getTtestTable(results)[["data"]]
  expect_equal_tables(table, list(0.00511047754408505, 0.311958523148014, "dependent_var"))
})
