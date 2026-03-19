context("Basic qgraph functionality")

test_that("qgraph creates object from correlation matrix", {
  set.seed(1)
  mat <- cor(matrix(rnorm(100), 10, 10))
  Q <- qgraph(mat, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
  expect_true(!is.null(Q$layout))
  expect_true(!is.null(Q$Edgelist))
})

test_that("qgraph creates object from adjacency matrix", {
  set.seed(1)
  adj <- matrix(sample(0:1, 25, TRUE, c(0.7, 0.3)), 5, 5)
  diag(adj) <- 0
  Q <- qgraph(adj, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph creates object from edgelist", {
  el <- matrix(c(1,2, 1,3, 2,3), ncol = 2, byrow = TRUE)
  Q <- qgraph(el, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph creates object from weighted edgelist", {
  el <- cbind(
    c(1, 1, 2),
    c(2, 3, 3),
    c(0.5, -0.3, 0.8)
  )
  Q <- qgraph(el, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
  expect_equal(length(Q$Edgelist$weight), 3)
})

test_that("qgraph respects layout argument", {
  mat <- diag(3)
  mat[1,2] <- mat[2,1] <- 0.5
  Q1 <- qgraph(mat, layout = "spring", do.not.plot = TRUE)
  Q2 <- qgraph(mat, layout = "circle", do.not.plot = TRUE)
  expect_s3_class(Q1, "qgraph")
  expect_s3_class(Q2, "qgraph")
})

test_that("qgraph respects groups argument", {
  set.seed(1)
  mat <- cor(matrix(rnorm(100), 10, 10))
  groups <- list(A = 1:5, B = 6:10)
  Q <- qgraph(mat, groups = groups, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph handles minimum and maximum", {
  set.seed(1)
  mat <- cor(matrix(rnorm(100), 10, 10))
  Q <- qgraph(mat, minimum = 0.2, maximum = 1, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph handles edge labels", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  Q <- qgraph(mat, edge.labels = TRUE, do.not.plot = TRUE)
  expect_s3_class(Q, "qgraph")
})

test_that("qgraph returns correct number of nodes", {
  mat <- matrix(0, 5, 5)
  mat[1,2] <- mat[2,1] <- 0.5
  Q <- qgraph(mat, do.not.plot = TRUE)
  expect_equal(Q$graphAttributes$Graph$nNodes, 5)
})

test_that("qgraph replotting with qgraph object as input works", {
  mat <- matrix(c(0, 0.5, 0.5, 0), 2, 2)
  Q1 <- qgraph(mat, do.not.plot = TRUE)
  Q2 <- qgraph(Q1, do.not.plot = TRUE)
  expect_s3_class(Q2, "qgraph")
})
