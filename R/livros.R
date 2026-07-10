# Split "TITULO. N ed. CIDADE: EDITORA, ANO ..." into title and tail.
# The title runs up to the edition marker; without one, up to the
# "CIDADE: EDITORA, ANO" block.
.split_titulo_publicacao <- function(txt) {
  loc <- stringr::str_locate(txt, .ed_regex)
  if (!is.na(loc[1, 1])) {
    titulo <- stringr::str_sub(txt, 1, loc[1, 1] - 1)
    cauda  <- stringr::str_sub(txt, loc[1, 2] + 1)
  } else {
    m <- stringr::str_match(txt, "^(.*?)\\s*\\.\\s*([^:,]*:\\s*[^,]*,\\s*(?:1[89]|20)\\d{2}.*)$")
    if (!is.na(m[, 1])) {
      titulo <- m[, 2]
      cauda  <- m[, 3]
    } else {
      titulo <- txt
      cauda  <- ""
    }
  }
  titulo <- stringr::str_squish(stringr::str_remove(titulo, "[.,;\\s]+$"))
  if (!nzchar(titulo)) titulo <- NA_character_
  c(titulo = titulo, cauda = cauda)
}

# Helper: parse a livro span.transform text
# "AUTORES (Org.) . Titulo do livro. N. ed. Cidade: Editora, ANO. v. X. NNNp ."
.parse_livro <- function(txt) {
  txt <- stringr::str_remove(txt, "^\\d+\\.\\s*")

  at      <- .split_autores_titulo(txt)
  autores <- at[["autores"]]

  edicao <- stringr::str_match(at[["resto"]], .ed_regex)[, 2]

  tt     <- .split_titulo_publicacao(at[["resto"]])
  titulo <- tt[["titulo"]]

  pub     <- .parse_pub(tt[["cauda"]])
  cidade  <- pub[["cidade"]]
  editora <- pub[["editora"]]
  ano     <- pub[["ano"]]
  if (is.na(ano)) ano <- .parse_ano(tt[["cauda"]])
  if (is.na(ano)) ano <- .parse_ano(txt)

  c(autores = autores, titulo = titulo, edicao = edicao,
    editora = editora, cidade = cidade, ano = ano)
}

# "AUTORES . Titulo do capitulo. In: ORGANIZADORES. (Org.). Titulo do livro.
#  Ned.Cidade: Editora, ANO, v. X, p. A-B."
.parse_capitulo <- function(txt) {
  txt <- stringr::str_remove(txt, "^\\d+\\.\\s*")

  # Split on "In:" to separate chapter info from book info
  partes <- stringr::str_split_fixed(txt, "\\bIn:\\s*", 2)
  cap_parte   <- partes[, 1]
  livro_parte <- partes[, 2]

  at             <- .split_autores_titulo(cap_parte)
  autores        <- at[["autores"]]
  titulo_capitulo <- stringr::str_squish(stringr::str_remove(at[["resto"]], "[.\\s]+$"))
  if (!nzchar(titulo_capitulo)) titulo_capitulo <- NA_character_

  # Organizadores: everything before "(Org.)"
  m_org <- stringr::str_match(livro_parte, "^(.*?)\\s*[.;,]?\\s*\\(Orgs?\\.?\\)\\s*\\.?")
  if (!is.na(m_org[, 1])) {
    organizadores <- stringr::str_squish(m_org[, 2])
    if (!nzchar(organizadores)) organizadores <- NA_character_
    resto <- stringr::str_sub(livro_parte, nchar(m_org[, 1]) + 1)
  } else {
    organizadores <- NA_character_
    resto <- livro_parte
  }

  edicao <- stringr::str_match(resto, .ed_regex)[, 2]

  tt           <- .split_titulo_publicacao(stringr::str_squish(resto))
  titulo_livro <- tt[["titulo"]]

  pub     <- .parse_pub(tt[["cauda"]])
  cidade  <- pub[["cidade"]]
  editora <- pub[["editora"]]
  ano     <- pub[["ano"]]
  if (is.na(ano)) ano <- .parse_ano(tt[["cauda"]])
  if (is.na(ano)) ano <- .parse_ano(livro_parte)

  m_pag <- stringr::str_match(livro_parte, "p\\.\\s*(\\d+)\\s*-\\s*(\\d+)?")
  pagina_inicial <- if (!is.na(m_pag[, 2])) m_pag[, 2] else NA_character_
  pagina_final   <- if (!is.na(m_pag[, 3])) m_pag[, 3] else NA_character_

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
    txts <- txts[stringr::str_detect(txts, .ed_regex) &
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
