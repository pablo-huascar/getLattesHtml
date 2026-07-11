# Canonical tail of a banca entry: "ANO. TIPO (PROGRAMA) - INSTITUICAO."
# Anchoring on the year avoids matching words like "trabalhadores (as)" inside
# the title. The LAST such match is taken because titles may contain years.
# PROGRAMA allows one level of nested parentheses:
# "(Doutorado em Psicologia (Psicologia Social))".
.banca_segmento <- function(txt) {
  m <- stringr::str_match_all(txt, paste0(
    "\\b((?:1[89]|20)\\d{2})\\.\\s*",          # ano
    "([^().]*?)\\s*",                          # tipo
    "\\(((?:[^()]|\\([^()]*\\))*)\\)\\s*",     # programa
    "(?:[-\u2013\u2014]\\s*([^.;\\n]+))?"      # instituicao
  ))[[1]]
  if (nrow(m) == 0) {
    return(c(ano = NA_character_, tipo = NA_character_,
             programa = NA_character_, instituicao = NA_character_,
             inicio = NA_character_))
  }
  m <- m[nrow(m), ]
  pos <- stringr::str_locate(txt, stringr::fixed(m[1]))[1, 1]
  c(ano = m[2], tipo = .nz(stringr::str_squish(m[3])),
    programa = .nz(stringr::str_squish(m[4])),
    instituicao = .nz(stringr::str_squish(m[5] %||% NA_character_)),
    inicio = as.character(pos))
}

# Helper: parse one banca span.transform text
.parse_banca <- function(txt) {
  txt <- stringr::str_squish(txt)

  # Members: all text before "Participa\u00e7\u00e3o em banca de"
  m_part <- stringr::str_split_fixed(txt,
    "(?i)Participa[\u00e7c][\u00e3a]o em banca de\\s+", 2)
  membros_banca <- stringr::str_squish(m_part[, 1]) |>
    stringr::str_remove("[;,\\s]+$") |>
    stringr::str_replace("\\.{2,}$", ".")
  resto <- m_part[, 2]

  # Candidate: up to the first period after "banca de"
  candidato_m <- stringr::str_match(resto, "^([^\\.]+)\\.\\s*")
  candidato <- if (!is.na(candidato_m[, 2])) stringr::str_squish(candidato_m[, 2]) else NA_character_
  resto2 <- sub("^[^\\.]+\\.\\s*", "", resto)

  seg <- .banca_segmento(txt)
  ano         <- if (!is.na(seg[["ano"]])) seg[["ano"]] else .parse_ano(txt)
  tipo        <- seg[["tipo"]]
  programa    <- seg[["programa"]]
  instituicao <- seg[["instituicao"]]

  # Titulo: between candidato and the "ANO. TIPO (...)" tail
  if (!is.na(seg[["inicio"]])) {
    corte <- as.integer(seg[["inicio"]]) - (nchar(txt) - nchar(resto2)) - 1L
    titulo <- if (corte > 0) stringr::str_sub(resto2, 1, corte) else resto2
  } else {
    titulo <- sub(paste0("\\s*\\.?\\s*", ano %||% "\\d{4}", ".*$"), "", resto2)
  }
  titulo <- stringr::str_squish(stringr::str_remove(titulo, "[.\\s]+$"))

  c(membros_banca = membros_banca, candidato = candidato,
    titulo = titulo, ano = ano, tipo = tipo,
    programa = programa, instituicao = instituicao)
}

# Classify a banca entry by the PROGRAMA of its canonical tail, so that
# "Exame de qualifica\u00e7\u00e3o (Doutorando em ...)" counts as doutorado and
# "P\u00f3s-Gradua\u00e7\u00e3o" inside the program name does not leak into
# gradua\u00e7\u00e3o.
.banca_natureza <- function(txt) {
  seg <- .banca_segmento(txt)
  programa <- seg[["programa"]]
  tipo     <- seg[["tipo"]]
  if (is.na(programa)) programa <- ""
  if (is.na(tipo)) tipo <- ""

  if (stringr::str_detect(programa, stringr::regex("^Doutor", ignore_case = TRUE)) ||
      stringr::str_detect(tipo, stringr::regex("^Tese", ignore_case = TRUE))) {
    return("doutorado")
  }
  if (stringr::str_detect(programa, stringr::regex("^Mestr", ignore_case = TRUE)) ||
      stringr::str_detect(tipo, stringr::regex("^Disserta", ignore_case = TRUE))) {
    return("mestrado")
  }
  if (stringr::str_detect(programa,
        stringr::regex("^(Gradua|Aperfei|Especializa)", ignore_case = TRUE)) ||
      stringr::str_detect(tipo,
        stringr::regex("Monografia|Conclus[\u00e3a]o", ignore_case = TRUE))) {
    return("graduacao")
  }
  NA_character_
}

.bancas_do_tipo <- function(doc, id_lattes, filtro_fn) {
  na_ret <- tibble::tibble(
    membros_banca = NA_character_, candidato = NA_character_,
    titulo = NA_character_, ano = NA_character_,
    tipo = NA_character_, programa = NA_character_,
    instituicao = NA_character_, id_lattes = id_lattes
  )

  anchors_pre <- c("ParticipacaoBancasTrabalho", "BancasTrabalho")
  anchors_pos <- c("EventosCongressos", "OrganizacaoEventos",
                   "Orientacaoemandamento", "OrientacaoEmAndamento",
                   "ParticipacaoBancasComissoes")

  txts <- .transforms_entre(doc, anchors_pre, anchors_pos)
  txts <- filtro_fn(txts)
  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_banca)

  tibble::tibble(
    membros_banca = sapply(parsed, `[[`, "membros_banca"),
    candidato     = sapply(parsed, `[[`, "candidato"),
    titulo        = sapply(parsed, `[[`, "titulo"),
    ano           = sapply(parsed, `[[`, "ano"),
    tipo          = sapply(parsed, `[[`, "tipo"),
    programa      = sapply(parsed, `[[`, "programa"),
    instituicao   = sapply(parsed, `[[`, "instituicao"),
    id_lattes     = rep(id_lattes, length(parsed))
  )
}

#' Extract doctoral examination boards
#'
#' @inheritParams get_id
#' @return A tibble with columns: membros_banca, candidato, titulo, ano, tipo,
#'   programa, instituicao, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_bancas_doutorado(html)
#' @export
get_bancas_doutorado <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  filtro <- function(v) purrr::keep(v, ~ identical(.banca_natureza(.x), "doutorado"))

  .bancas_do_tipo(doc, id_lattes, filtro)
}

#' Extract master's examination boards
#'
#' @inheritParams get_id
#' @return A tibble with columns: membros_banca, candidato, titulo, ano, tipo,
#'   programa, instituicao, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_bancas_mestrado(html)
#' @export
get_bancas_mestrado <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  filtro <- function(v) purrr::keep(v, ~ identical(.banca_natureza(.x), "mestrado"))

  .bancas_do_tipo(doc, id_lattes, filtro)
}

#' Extract undergraduate/specialization examination boards
#'
#' Covers monografias, TCC and similar final-year projects.
#'
#' @inheritParams get_id
#' @return A tibble with columns: membros_banca, candidato, titulo, ano, tipo,
#'   programa, instituicao, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_bancas_graduacao(html)
#' @export
get_bancas_graduacao <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  filtro <- function(v) purrr::keep(v, ~ identical(.banca_natureza(.x), "graduacao"))

  .bancas_do_tipo(doc, id_lattes, filtro)
}
