# Regression tests for the text parsers, using citation formats found in real
# Lattes curricula.

test_that(".split_autores_titulo handles the author-block endings", {
  expect_equal(
    .split_autores_titulo("FURTADO, L.A.R.. Titulo do artigo. Revista, 2021.")[["autores"]],
    "FURTADO, L.A.R."
  )
  expect_equal(
    .split_autores_titulo("LARANJEIRA, PIRES. Ensaios afro-literarios. 2. ed. Lisboa: X, 2001.")[["autores"]],
    "LARANJEIRA, PIRES"
  )
  expect_equal(
    .split_autores_titulo("PRIMI, Ricardo; CASTILHO, A. V. . Software MMPI. 1997.")[["autores"]],
    "PRIMI, Ricardo; CASTILHO, A. V."
  )
})

test_that(".parse_trabalho_evento splits title, event and year", {
  r <- .parse_trabalho_evento(paste(
    "GOMES, R. H. S. F.. Desobediencia e Lei Natural em Hobbes. In: I Encontro",
    "de Pesquisa em Filosofia da UFC, 2009, Fortaleza-Ce. Caderno de Resumos, 2009. p. 206-206."
  ))
  expect_equal(r[["autores"]], "GOMES, R. H. S. F.")
  expect_equal(r[["titulo"]], "Desobediencia e Lei Natural em Hobbes")
  expect_equal(r[["evento"]], "I Encontro de Pesquisa em Filosofia da UFC")
  expect_equal(r[["ano"]], "2009")

  r <- .parse_trabalho_evento(
    "CUNHA, E. S.; PINHEIRO, F. P. H. A. . Monitoria. 2021. (Apresentacao de Trabalho/Outra)."
  )
  expect_equal(r[["titulo"]], "Monitoria")
  expect_equal(r[["tipo"]], "Apresentacao de Trabalho/Outra")
})

test_that(".parse_prod_tecnica keeps a year that is part of the title", {
  r <- .parse_prod_tecnica("GOMES, R. H. S. F.; SABOIA, I. B. . Evento Polifonias 2021. 2021.",
                           "Trabalhos tecnicos")
  expect_equal(r[["titulo"]], "Evento Polifonias 2021")
  expect_equal(r[["ano"]], "2021")
  expect_equal(r[["tipo"]], "Trabalhos tecnicos")
})

test_that(".cauda_periodico reads volume, issue and page range", {
  r <- .cauda_periodico("X. Revista Y, v. 33, n. 2, p. 161-163, 2021.")
  expect_equal(unname(r[c("volume", "numero", "pagina_inicial", "pagina_final", "ano")]),
               c("33", "2", "161", "163", "2021"))
})

test_that(".parse_capitulo treats '-ed.' as a blank edition", {
  r <- .parse_capitulo(paste(
    "MARTINS, P. ; DIAS, M. . Analise. In: Andrea (Org.). Saude sem Fronteiras.",
    "-ed.Curitiba: Editora CRV, 2020, v. , p. 35-63."
  ))
  expect_true(is.na(r[["edicao"]]))
  expect_equal(r[["cidade"]], "Curitiba")
  expect_equal(r[["editora"]], "Editora CRV")
})

test_that(".rotulo tolerates U+FFFD in damaged downloads", {
  expect_true(grepl(.rotulo("Descrição"), "Descri��o: x"))
  expect_true(grepl(.rotulo("Descrição"), "Descricao: x"))
})
