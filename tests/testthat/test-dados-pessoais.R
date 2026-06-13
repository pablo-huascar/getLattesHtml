html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")

test_that("get_id returns a tibble with id_lattes", {
  res <- get_id(html)
  expect_s3_class(res, "tbl_df")
  expect_named(res, "id_lattes")
  expect_equal(nrow(res), 1L)
  expect_equal(res$id_lattes, "1234567890123456")
})

test_that("get_dados_gerais returns expected columns", {
  res <- get_dados_gerais(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("nome", "id_lattes", "resumo") %in% names(res)))
  expect_equal(nrow(res), 1L)
  expect_equal(res$nome, "Joana Ferreira")
})

test_that("get_endereco_profissional returns expected columns", {
  res <- get_endereco_profissional(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("cidade", "uf", "id_lattes") %in% names(res)))
  expect_equal(res$cidade, "Fortaleza")
})

test_that("get_idiomas returns one row per language", {
  res <- get_idiomas(html)
  expect_s3_class(res, "tbl_df")
  expect_true(nrow(res) >= 1L)
  expect_true("idioma" %in% names(res))
})

test_that("get_areas_atuacao returns expected columns", {
  res <- get_areas_atuacao(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("grande_area", "area", "id_lattes") %in% names(res)))
})

test_that("get_linha_pesquisa returns a tibble", {
  res <- get_linha_pesquisa(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("linha", "id_lattes") %in% names(res)))
})
