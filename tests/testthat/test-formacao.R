html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")

test_that("get_formacao_graduacao returns expected columns and data", {
  res <- get_formacao_graduacao(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("periodo", "curso", "instituicao", "id_lattes") %in% names(res)))
  expect_true(nrow(res) >= 1L)
})

test_that("get_formacao_mestrado returns expected columns and data", {
  res <- get_formacao_mestrado(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("periodo", "curso", "titulo", "ano", "id_lattes") %in% names(res)))
  expect_true(nrow(res) >= 1L)
})

test_that("get_formacao_doutorado returns expected columns and data", {
  res <- get_formacao_doutorado(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("periodo", "titulo", "ano", "id_lattes") %in% names(res)))
  expect_true(nrow(res) >= 1L)
  expect_equal(res$ano[1], "2014")
})

test_that("get_formacao_pos_doutorado returns a tibble", {
  res <- get_formacao_pos_doutorado(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_formacao_complementar returns a tibble", {
  res <- get_formacao_complementar(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})
