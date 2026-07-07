html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")

test_that("get_orientacoes_mestrado returns data from example file", {
  res <- get_orientacoes_mestrado(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("aluno", "titulo", "ano", "situacao", "id_lattes") %in% names(res)))
  expect_true(nrow(res) >= 1L)
})

test_that("get_orientacoes_mestrado does not leak banca entries", {
  res <- get_orientacoes_mestrado(html)
  expect_equal(nrow(res), 2L)
  expect_false(any(stringr::str_detect(res$titulo, "banca")))
  expect_setequal(res$aluno, c("Carlos Eduardo Lima", "Ana Paula Rodrigues"))
})

test_that("orientacao em andamento has a clean title", {
  res <- get_orientacoes_mestrado(html)
  em_and <- res[res$situacao == "em andamento", ]
  expect_equal(em_and$titulo, "Discriminação racial no trabalho")
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

test_that("get_bancas_mestrado parses fields from example file", {
  res <- get_bancas_mestrado(html)
  expect_equal(res$candidato[1], "Pedro Costa")
  expect_equal(res$ano[1], "2023")
  expect_equal(res$membros_banca[1], "FERREIRA, J.; SILVA, M. A.")
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
