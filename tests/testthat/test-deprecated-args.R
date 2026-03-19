context("Deprecated argument warnings")

test_that("vsize triggers deprecation warning for node.size", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  expect_warning(
    qgraph(mat, vsize = 10, DoNotPlot = TRUE),
    "deprecated"
  )
})

test_that("node.size works without warning", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  expect_silent({
    suppressWarnings(qgraph(mat, node.size = 10, do.not.plot = TRUE))
  })
})

test_that("esize triggers deprecation warning", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  expect_warning(
    qgraph(mat, esize = 5, DoNotPlot = TRUE),
    "deprecated"
  )
})

test_that("posCol triggers deprecation warning", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  expect_warning(
    qgraph(mat, posCol = "blue", DoNotPlot = TRUE),
    "deprecated"
  )
})

test_that("negCol triggers deprecation warning", {
  mat <- matrix(c(0, 0.5, -0.5, 0), 2, 2)
  expect_warning(
    qgraph(mat, negCol = "purple", DoNotPlot = TRUE),
    "deprecated"
  )
})

test_that("DoNotPlot triggers deprecation warning", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  expect_warning(
    qgraph(mat, DoNotPlot = TRUE),
    "deprecated"
  )
})

test_that("sampleSize triggers deprecation warning", {
  set.seed(1)
  mat <- cor(matrix(rnorm(100), 10, 10))
  expect_warning(
    qgraph(mat, graph = "glasso", sampleSize = 10, DoNotPlot = TRUE),
    "deprecated"
  )
})

test_that("nodeNames triggers deprecation warning", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  expect_warning(
    qgraph(mat, nodeNames = c("A", "B"), DoNotPlot = TRUE),
    "deprecated"
  )
})

test_that("asize triggers deprecation warning", {
  el <- matrix(c(1, 2), ncol = 2)
  expect_warning(
    qgraph(el, asize = 5, DoNotPlot = TRUE),
    "deprecated"
  )
})

test_that("new name takes precedence when both old and new are specified", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  expect_warning(
    Q <- qgraph(mat, node.size = 15, vsize = 10, do.not.plot = TRUE),
    "Both"
  )
  # New name should win
  expect_equal(Q$graphAttributes$Nodes$width[1], 15)
})

test_that("renamed arguments produce same results as old names", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)

  Q_old <- suppressWarnings(qgraph(mat, vsize = 12, DoNotPlot = TRUE))
  Q_new <- qgraph(mat, node.size = 12, do.not.plot = TRUE)

  expect_equal(Q_old$graphAttributes$Nodes$width, Q_new$graphAttributes$Nodes$width)
})

test_that("GLratio triggers deprecation warning", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  groups <- list(A = 1, B = 2)
  expect_warning(
    qgraph(mat, groups = groups, GLratio = 3, DoNotPlot = TRUE),
    "deprecated"
  )
})
