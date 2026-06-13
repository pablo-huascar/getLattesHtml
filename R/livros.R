# Helper: parse a livro/cap\u00edtulo span.transform text
.parse_livro <- function(txt) {
  # Remove leading counter "N. " if present
  txt <- stringr::str_remove(txt, "^\\d+\\.\\s*")

  ano <- .parse_ano(txt)

  # Edition marker: "N. ed." splits autores+titulo from publisher info
  m_ed <- stringr::str_match(txt, "(\\d+[a\u00aa]?)\\.\\s*ed\\.")
  edicao <- if (!is.na(m_ed[, 1])) m_ed[, 2] else NA_character_

  # Split on ". N. ed." to get left (autores . titulo) and right (city: publisher, year)
  partes <- if (!is.na(m_ed[, 1])) {
    stringr::str_split_fixed(txt, paste0(m_ed[, 1], "\\.\\s*ed\\."), 2)
  } else {
    matrix(c(txt, ""), nrow = 1)
  }

  esquerda <- partes[, 1]
  direita   <- partes[, 2]

  # In "esquerda": autores end at last ". " before mixed-case title
  # Heuristic: find the split between UPPERCASE author block and title
  m_split <- stringr::str_match(esquerda,
    "^((?:[A-Z\u00c1\u00c9\u00cd\u00d3\u00da\u00c3\u00d5\u00c2\u00ca\u00d4\u00c0\u00c7][^a-z]{0,3}[A-Z\u00c1\u00c9\u00cd\u00d3\u00da\u00c3\u00d5\u00c2\u00ca\u00d4\u00c0\u00c7][^\\.]+\\.\\s*(?:\\([^)]+\\)\\.\\s*)?)+?)([A-Z\u00c1\u00c9\u00cd\u00d3\u00da\u00c3\u00d5\u00c2\u00ca\u00d4\u00c0\u00c7][a-z\u00e1\u00e9\u00ed\u00f3\u00fa\u00e3\u00f5\u00e2\u00ea\u00f4\u00e0\u00e7].+)$"
  )

  autores <- if (!is.na(m_split[, 2])) stringr::str_squish(m_split[, 2]) else NA_character_
  titulo  <- if (!is.na(m_split[, 3])) stringr::str_squish(m_split[, 3]) else
    stringr::str_squish(esquerda)

  # Parse right side: "CIDADE: EDITORA, ANO."
  m_pub <- stringr::str_match(direita, "^\\s*([^:]+):\\s*([^,]+),\\s*(\\d{4})")
  cidade  <- if (!is.na(m_pub[, 2])) stringr::str_squish(m_pub[, 2]) else NA_character_
  editora <- if (!is.na(m_pub[, 3])) stringr::str_squish(m_pub[, 3]) else NA_character_

  c(autores = autores, titulo = titulo, edicao = edicao,
    editora = editora, cidade = cidade, ano = ano)
}

.parse_capitulo <- function(txt) {
  txt <- stringr::str_remove(txt, "^\\d+\\.\\s*")
  ano <- .parse_ano(txt)

  # Split on "In:" to separate chapter info from book info
  partes <- stringr::str_split_fixed(txt, "\\bIn:\\s*", 2)
  cap_parte  <- partes[, 1]
  livro_parte <- partes[, 2]

  # Cap: autores. titulo_capitulo.
  m_cap <- stringr::str_match(cap_parte,
    "^((?:[A-Z\u00c1\u00c9\u00cd\u00d3\u00da\u00c3\u00d5\u00c2\u00ca\u00d4\u00c0\u00c7][^\\.]+\\.\\s*(?:\\([^)]+\\)\\.\\s*)?)+?)([A-Z\u00c1\u00c9\u00cd\u00d3\u00da\u00c3\u00d5\u00c2\u00ca\u00d4\u00c0\u00c7][a-z\u00e1\u00e9\u00ed\u00f3\u00fa\u00e3\u00f5\u00e2\u00ea\u00f4\u00e0\u00e7].+\\.?)$"
  )
  autores        <- if (!is.na(m_cap[, 2])) stringr::str_squish(m_cap[, 2]) else NA_character_
  titulo_capitulo <- if (!is.na(m_cap[, 3])) stringr::str_squish(m_cap[, 3]) else
    stringr::str_squish(cap_parte)

  # Livro: ORG (Org.). TITULO_LIVRO. N. ed. CIDADE: EDITORA, ANO, p. X-Y.
  m_tl <- stringr::str_match(livro_parte, "\\.\\s*([A-Z\u00c1\u00c9\u00cd\u00d3\u00da\u00c3\u00d5\u00c2\u00ca\u00d4\u00c0\u00c7][^\\(]+)\\.")
  titulo_livro <- if (!is.na(m_tl[, 2])) stringr::str_squish(m_tl[, 2]) else NA_character_

  m_pag <- stringr::str_match(livro_parte, "p\\.\\s*(\\d+)-(\\d+)")
  pagina_inicial <- if (!is.na(m_pag[, 2])) m_pag[, 2] else NA_character_
  pagina_final   <- if (!is.na(m_pag[, 3])) m_pag[, 3] else NA_character_

  m_ed  <- stringr::str_match(livro_parte, "(\\d+[a\u00aa]?)\\.\\s*ed\\.")
  edicao <- if (!is.na(m_ed[, 2])) m_ed[, 2] else NA_character_

  m_pub <- stringr::str_match(livro_parte, "(\\d+[a\u00aa]?)\\.\\s*ed\\.\\s*([^:]+):\\s*([^,]+),\\s*(\\d{4})")
  cidade  <- if (!is.na(m_pub[, 3])) stringr::str_squish(m_pub[, 3]) else NA_character_
  editora <- if (!is.na(m_pub[, 4])) stringr::str_squish(m_pub[, 4]) else NA_character_

  organizadores_m <- stringr::str_match(livro_parte, "^([^.]+\\(Org\\.\\)[^.]*)")
  organizadores <- if (!is.na(organizadores_m[, 2])) stringr::str_squish(organizadores_m[, 2]) else
    NA_character_

  c(autores = autores, titulo_capitulo = titulo_capitulo,
    titulo_livro = titulo_livro, organizadores = organizadores,
    edicao = edicao, editora = editora, cidade = cidade,
    pagina_inicial = pagina_inicial, pagina_final = pagina_final, ano = ano)
}

#' Extract published books
#'
#' @inheritParams get_id
#' @return A tibble with columns: autores, titulo, edicao, editora, cidade, ano, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_livros_publicados(html)
#' @export
get_livros_publicados <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    autores = NA_character_, titulo = NA_character_, edicao = NA_character_,
    editora = NA_character_, cidade = NA_character_, ano = NA_character_,
    id_lattes = id_lattes
  )

  txts <- .transforms_por_cita(doc, "Livros publicados", "Cap[\u00edi]tulos")
  if (length(txts) == 0) {
    # Try getting all transforms after LivrosCapitulos anchor
    txts <- .transforms_entre(doc, "LivrosCapitulos",
      c("TrabalhoEmEventos", "AnaisCongressos", "OutrasProducoesBibliograficas",
        "ProducaoBibliografica", "ProducaoTecnica"))
    if (length(txts) == 0) return(na_ret)
    # Keep only entries that look like books (have "ed." and no "In:")
    txts <- txts[stringr::str_detect(txts, "\\.\\s*ed\\.") &
                   !stringr::str_detect(txts, "\\bIn:\\s*")]
  }

  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_livro)

  tibble::tibble(
    autores   = sapply(parsed, `[[`, "autores"),
    titulo    = sapply(parsed, `[[`, "titulo"),
    edicao    = sapply(parsed, `[[`, "edicao"),
    editora   = sapply(parsed, `[[`, "editora"),
    cidade    = sapply(parsed, `[[`, "cidade"),
    ano       = sapply(parsed, `[[`, "ano"),
    id_lattes = rep(id_lattes, length(parsed))
  )
}

#' Extract book chapters
#'
#' @inheritParams get_id
#' @return A tibble with columns: autores, titulo_capitulo, titulo_livro,
#'   organizadores, edicao, editora, cidade, pagina_inicial, pagina_final, ano,
#'   id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_capitulos_livros(html)
#' @export
get_capitulos_livros <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    autores = NA_character_, titulo_capitulo = NA_character_,
    titulo_livro = NA_character_, organizadores = NA_character_,
    edicao = NA_character_, editora = NA_character_,
    cidade = NA_character_, pagina_inicial = NA_character_,
    pagina_final = NA_character_, ano = NA_character_,
    id_lattes = id_lattes
  )

  # LivrosCapitulos anchor appears twice (once for livros, once for chapters).
  # Get all spans in that subsection range, then filter by "In:" to keep chapters only.
  txts <- .transforms_entre_strict(doc,
    c("LivrosCapitulos"),
    c("TextosJornaisRevistas", "TrabalhosPublicadosAnaisCongresso",
      "ApresentacoesTrabalho", "ProducaoTecnica"))
  if (length(txts) == 0) return(na_ret)

  # Keep only chapter entries (chapters have "In:" reference to the hosting book)
  txts <- txts[stringr::str_detect(txts, "\\bIn:\\s*")]
  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_capitulo)

  tibble::tibble(
    autores         = sapply(parsed, `[[`, "autores"),
    titulo_capitulo = sapply(parsed, `[[`, "titulo_capitulo"),
    titulo_livro    = sapply(parsed, `[[`, "titulo_livro"),
    organizadores   = sapply(parsed, `[[`, "organizadores"),
    edicao          = sapply(parsed, `[[`, "edicao"),
    editora         = sapply(parsed, `[[`, "editora"),
    cidade          = sapply(parsed, `[[`, "cidade"),
    pagina_inicial  = sapply(parsed, `[[`, "pagina_inicial"),
    pagina_final    = sapply(parsed, `[[`, "pagina_final"),
    ano             = sapply(parsed, `[[`, "ano"),
    id_lattes       = rep(id_lattes, length(parsed))
  )
}
