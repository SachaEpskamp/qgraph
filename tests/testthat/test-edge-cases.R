context("Edge cases")

test_that("qgraph handles single node", {
  mat <- matrix(0, 1, 1)
  Q <- qgraph(mat, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
  expect_equal(Q$graphAttributes$Graph$nNodes, 1)
})

test_that("qgraph handles empty graph (no edges)", {
  mat <- matrix(0, 3, 3)
  Q <- qgraph(mat, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
  expect_equal(Q$graphAttributes$Graph$nNodes, 3)
})

test_that("qgraph handles negative correlations", {
  mat <- matrix(c(1, -0.8, -0.8, 1), 2, 2)
  Q <- qgraph(mat, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph handles directed graph", {
  mat <- matrix(c(0, 0.5, 0, 0), 2, 2)
  Q <- qgraph(mat, directed = TRUE, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph handles various themes", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  themes <- c("classic", "colorblind", "gray", "Hollywood")
  for (th in themes) {
    Q <- qgraph(mat, theme = th, do.not.plot = TRUE)
    expect_s3_class(Q, "qgraph")
  }
})

test_that("qgraph handles cut argument", {
  set.seed(1)
  mat <- cor(matrix(rnorm(400), 20, 20))
  Q <- qgraph(mat, cut = 0.3, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph handles character labels", {
  mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.4, 0.3, 0.4, 0), 3, 3)
  Q <- qgraph(mat, labels = c("X", "Y", "Z"), do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph handles node colors", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  Q <- qgraph(mat, color = c("red", "blue"), do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph handles custom layout matrix", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  L <- matrix(c(0, 0, 1, 1), 2, 2)
  Q <- qgraph(mat, layout = L, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})
