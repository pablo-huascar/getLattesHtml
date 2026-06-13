html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")

test_that("get_orientacoes_mestrado returns data from example file", {
  res <- get_orientacoes_mestrado(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("aluno", "titulo", "ano", "situacao", "id_lattes") %in% names(res)))
  expect_true(nrow(res) >= 1L)
})

test_that("get_orientacoes_doutorado returns a tibble", {
  res <- get_orientacoes_doutorado(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_orientacoes_pos_doutorado returns a tibble", {
  res <- get_orientacoes_pos_doutorado(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_bancas_mestrado returns data from example file", {
  res <- get_bancas_mestrado(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("candidato", "titulo", "ano", "id_lattes") %in% names(res)))
  expect_true(nrow(res) >= 1L)
})

test_that("get_bancas_doutorado returns a tibble", {
  res <- get_bancas_doutorado(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_bancas_graduacao returns a tibble", {
  res <- get_bancas_graduacao(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})
