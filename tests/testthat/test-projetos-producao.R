html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")

test_that("get_participacao_projeto returns data from example file", {
  res <- get_participacao_projeto(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("titulo", "descricao", "situacao", "id_lattes") %in% names(res)))
  expect_true(nrow(res) >= 1L)
})

test_that("get_producao_tecnica returns a tibble", {
  res <- get_producao_tecnica(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("autores", "titulo", "ano", "tipo", "id_lattes") %in% names(res)))
})

test_that("get_outras_producoes_tecnicas returns a tibble", {
  res <- get_outras_producoes_tecnicas(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_patentes returns a tibble", {
  res <- get_patentes(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_livros_publicados returns a tibble", {
  res <- get_livros_publicados(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("autores", "titulo", "id_lattes") %in% names(res)))
})

test_that("get_capitulos_livros returns a tibble", {
  res <- get_capitulos_livros(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_trabalhos_em_eventos returns a tibble", {
  res <- get_trabalhos_em_eventos(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_trabalhos_anais_congresso returns a tibble", {
  res <- get_trabalhos_anais_congresso(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_eventos_congressos returns a tibble", {
  res <- get_eventos_congressos(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_organizacao_eventos returns a tibble", {
  res <- get_organizacao_eventos(html)
  expect_s3_class(res, "tbl_df")
  expect_true("id_lattes" %in% names(res))
})

test_that("get_atuacoes_profissionais returns a tibble", {
  res <- get_atuacoes_profissionais(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("instituicao", "vinculo", "id_lattes") %in% names(res)))
})
