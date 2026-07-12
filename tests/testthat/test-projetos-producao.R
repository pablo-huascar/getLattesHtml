html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")

test_that("get_participacao_projeto returns data from example file", {
  res <- get_participacao_projeto(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("titulo", "descricao", "situacao", "id_lattes") %in% names(res)))
  expect_true(nrow(res) >= 1L)
})

test_that("get_participacao_projeto splits the labelled fields", {
  res <- get_participacao_projeto(html)
  expect_equal(res$descricao[1], "Estudo sobre identidade em grupos minorizados.")
  expect_equal(res$situacao[1], "Em andamento")
  expect_equal(res$natureza[1], "Pesquisa")
  expect_equal(res$integrantes[1], "Joana Ferreira - Coordenadora.")
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
  expect_true(all(c(
    "instituicao", "vinculo", "atividade", "enquadramento_funcional",
    "carga_horaria", "regime", "outras_informacoes", "id_lattes"
  ) %in% names(res)))
})

test_that("get_atuacoes_profissionais splits the labelled vinculo fields", {
  res <- get_atuacoes_profissionais(html)
  # The bundled example has a single fully-populated vinculo cell:
  # "Vínculo: Docente, Atividade: Ensino Superior, Enquadramento Funcional:
  #  Professor Adjunto, Carga Horária: 40, Regime: Dedicacao Exclusiva."
  expect_equal(res$vinculo[1], "Docente")
  expect_equal(res$atividade[1], "Ensino Superior")
  expect_equal(res$enquadramento_funcional[1], "Professor Adjunto")
  expect_equal(res$carga_horaria[1], "40")
  expect_equal(res$regime[1], "Dedicacao Exclusiva")
  # No label text should leak into the values
  expect_false(any(grepl("Funcional:|Carga|Regime:",
    res$enquadramento_funcional, ignore.case = TRUE)))
})
