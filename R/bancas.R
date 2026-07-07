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

  # Year
  ano <- .parse_ano(txt)

  # Titulo: between candidato and year
  titulo <- sub(paste0("\\s*\\.?\\s*", ano %||% "\\d{4}", ".*$"), "", resto2) |>
    stringr::str_squish()

  # Type: content of first parenthetical after year
  tipo_m <- stringr::str_match(txt, "(?i)\\b\\d{4}\\.\\s*(Disserta\\w*|Tese|Monografia|Trabalho[^(]+)\\s*\\(")
  tipo <- if (!is.na(tipo_m[, 2])) stringr::str_squish(tipo_m[, 2]) else {
    stringr::str_match(txt, "(?i)(Disserta\\w*|Tese|Monografia|Trabalho[^(]+)\\s*\\(")[, 2] |>
      stringr::str_squish()
  }

  # Program: content inside parentheses after tipo
  prog_m <- stringr::str_match(txt,
    "(?i)(?:Disserta\\w*|Tese|Monografia|Trabalho[^(]+)\\s*\\(([^)]+)\\)")
  programa <- if (!is.na(prog_m[, 2])) stringr::str_squish(prog_m[, 2]) else NA_character_

  # Institution: after ")" + "-" pattern
  inst_m <- stringr::str_match(txt,
    "(?i)(?:Disserta\\w*|Tese|Monografia|Trabalho[^(]+)\\s*\\([^)]+\\)\\s*[-\u2013\u2014]\\s*([^.;,\\n]+)")
  instituicao <- if (!is.na(inst_m[, 2])) stringr::str_squish(inst_m[, 2]) else NA_character_

  c(membros_banca = membros_banca, candidato = candidato,
    titulo = titulo, ano = ano, tipo = tipo,
    programa = programa, instituicao = instituicao)
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

  filtro <- function(v) purrr::keep(v, ~ stringr::str_detect(.x,
    stringr::regex("Tese\\s*\\(.*?Doutorad|doutorado|doutoral", ignore_case = TRUE)))

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

  filtro <- function(v) purrr::keep(v, ~ stringr::str_detect(.x,
    stringr::regex("Disserta\\w+\\s*\\([^)]*Mestrad", ignore_case = TRUE)))

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

  filtro <- function(v) purrr::keep(v, ~ stringr::str_detect(.x,
    stringr::regex(paste0(
      "Monografia|Trabalho de Conclus[\u00e3a]o|TCC|",
      "Especializa[\u00e7c][\u00e3a]o|Aperfei[\u00e7c]oamento|Gradua[\u00e7c][\u00e3a]o"
    ), ignore_case = TRUE)) &&
    !stringr::str_detect(.x, stringr::regex("Mestrad|Doutorad", ignore_case = TRUE)))

  .bancas_do_tipo(doc, id_lattes, filtro)
}
