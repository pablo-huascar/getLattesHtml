html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")

test_that("get_artigos_publicados returns expected columns", {
  res <- get_artigos_publicados(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("ano", "titulo", "periodico", "issn", "doi", "id_lattes") %in% names(res)))
  expect_true(nrow(res) >= 1L)
  expect_equal(res$ano[1], "2022")
})

test_that("get_artigos_aceitos always returns NA tibble", {
  res <- get_artigos_aceitos(html)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("ano", "titulo", "id_lattes") %in% names(res)))
  expect_true(is.na(res$titulo[1]))
})
