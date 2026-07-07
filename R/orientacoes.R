# Shared anchors for orientation sections
.anchors_concluidas  <- c("Orientacoesconcluidas", "OrientacoesConcluidas")
# "Orientacoes" = general section (em andamento); "Orientacaoemandamento" = alternate name
.anchors_emandamento <- c(
  "Orientacoes", "Orientacaoemandamento", "OrientacaoEmAndamento",
  "Orientacoesemandamento", "OrientacoesEmAndamento"
)

# Anchors of sections that may follow the orientations block; used to stop
# the "conclu\u00eddas" scan so bancas/eventos entries do not leak in.
.anchors_pos_orientacoes <- c(
  "ParticipacaoBancasTrabalho", "BancasTrabalho", "ParticipacaoBancasComissoes",
  "Bancas", "EventosCongressos", "ParticipacaoEventos", "OrganizacaoEventos",
  "Eventos", "OutrasInformacoesRelevantes", "PotencialInovacao"
)

# Helper: collect and classify orientation texts
.orientacoes_textos <- function(doc, filtro_fn) {
  # Use strict between to avoid mixing em andamento and conclu\u00eddas
  em_and  <- .transforms_entre_strict(doc, .anchors_emandamento, .anchors_concluidas)
  conclui <- .transforms_entre_strict(doc, .anchors_concluidas, .anchors_pos_orientacoes)

  # Banca texts also mention "Disserta\u00e7\u00e3o (Mestrado ...)" etc.; drop them
  sem_banca <- function(v) purrr::discard(v, ~ stringr::str_detect(.x,
    stringr::regex("Participa[\u00e7c][\u00e3a]o em banca", ignore_case = TRUE)))

  em_and  <- filtro_fn(sem_banca(em_and))
  conclui <- filtro_fn(sem_banca(conclui))

  list(
    textos   = c(conclui, em_and),
    situacao = c(rep("conclu\u00edda", length(conclui)),
                 rep("em andamento", length(em_and)))
  )
}

# Helper: parse one orientation span text into named vector
.parse_orientacao <- function(txt, tipo_pat, tipo_label) {
  aluno <- stringr::str_match(txt, "^\\s*([^\\.]+)\\.")[, 2] |>
    stringr::str_squish()

  txt1 <- sub("^\\s*[^\\.]+\\.\\s*", "", txt)
  pos_year <- regexpr("\\b[12][0-9]{3}\\b", txt1)
  if (pos_year > 0) {
    tit <- substr(txt1, 1, pos_year - 1)
  } else {
    pos_tipo <- regexpr(tipo_pat, txt1, perl = TRUE, ignore.case = TRUE)
    tit <- if (pos_tipo > 0) substr(txt1, 1, pos_tipo - 1) else txt1
  }
  tit <- stringr::str_squish(tit)
  # Drop trailing "In\u00edcio:"/"Ano:" labels left behind when the year was cut off
  tit <- stringr::str_remove(tit,
    stringr::regex("[.;,:\\s]*(?:In[\u00edi]cio|Ano)[.;,:\\s]*$", ignore_case = TRUE))
  tit <- stringr::str_replace(tit, "[.;,:\\s]+$", "")

  ano <- stringr::str_extract(txt, "\\b[12][0-9]{3}\\b")

  curso_m <- stringr::str_match(txt,
    paste0("(?i)", tipo_label, "\\s*\\(([^)]*)\\)"))
  curso <- if (!is.na(curso_m[, 2])) stringr::str_squish(curso_m[, 2]) else NA_character_

  inst_m <- stringr::str_match(txt,
    paste0("(?i)", tipo_label, "\\s*\\([^)]*\\)\\s*[-\u2013\u2014]\\s*([^.,;\\n]+)"))
  instituicao <- if (!is.na(inst_m[, 2])) stringr::str_squish(inst_m[, 2]) else NA_character_

  c(aluno = aluno, titulo = tit, ano = ano, curso = curso, instituicao = instituicao)
}

#' Extract doctoral advisorships
#'
#' @inheritParams get_id
#' @return A tibble with columns: aluno, titulo, ano, curso, instituicao,
#'   situacao, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_orientacoes_doutorado(html)
#' @export
get_orientacoes_doutorado <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    aluno = NA_character_, titulo = NA_character_,
    ano = NA_character_, curso = NA_character_,
    instituicao = NA_character_, situacao = NA_character_,
    id_lattes = id_lattes
  )

  filtro <- function(v) purrr::keep(v, ~ stringr::str_detect(.x,
    stringr::regex("\\bTese\\s*\\(.*?Doutorado", ignore_case = TRUE)))

  res <- .orientacoes_textos(doc, filtro)
  if (length(res$textos) == 0) return(na_ret)

  parsed <- lapply(res$textos, .parse_orientacao, "(?i)Tese\\s*\\(", "Tese")
  n <- length(parsed)

  tibble::tibble(
    aluno       = sapply(parsed, `[[`, "aluno"),
    titulo      = sapply(parsed, `[[`, "titulo"),
    ano         = sapply(parsed, `[[`, "ano"),
    curso       = sapply(parsed, `[[`, "curso"),
    instituicao = sapply(parsed, `[[`, "instituicao"),
    situacao    = res$situacao,
    id_lattes   = rep(id_lattes, n)
  )
}

#' Extract master's advisorships
#'
#' @inheritParams get_id
#' @return A tibble with columns: aluno, titulo, ano, curso, instituicao,
#'   situacao, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_orientacoes_mestrado(html)
#' @export
get_orientacoes_mestrado <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    aluno = NA_character_, titulo = NA_character_,
    ano = NA_character_, curso = NA_character_,
    instituicao = NA_character_, situacao = NA_character_,
    id_lattes = id_lattes
  )

  filtro <- function(v) purrr::keep(v, ~ stringr::str_detect(.x,
    stringr::regex("\\bDisserta\\w*\\s*\\([^)]*Mestrad\\w*", ignore_case = TRUE)))

  res <- .orientacoes_textos(doc, filtro)
  if (length(res$textos) == 0) return(na_ret)

  parsed <- lapply(res$textos, .parse_orientacao,
    "(?i)Disserta\\w*\\s*\\(", "Disserta\\w*")
  n <- length(parsed)

  tibble::tibble(
    aluno       = sapply(parsed, `[[`, "aluno"),
    titulo      = sapply(parsed, `[[`, "titulo"),
    ano         = sapply(parsed, `[[`, "ano"),
    curso       = sapply(parsed, `[[`, "curso"),
    instituicao = sapply(parsed, `[[`, "instituicao"),
    situacao    = res$situacao,
    id_lattes   = rep(id_lattes, n)
  )
}

#' Extract post-doctoral supervisorships
#'
#' @inheritParams get_id
#' @return A tibble with columns: aluno, titulo, ano, instituicao, situacao,
#'   id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_orientacoes_pos_doutorado(html)
#' @export
get_orientacoes_pos_doutorado <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    aluno = NA_character_, titulo = NA_character_,
    ano = NA_character_, instituicao = NA_character_,
    situacao = NA_character_, id_lattes = id_lattes
  )

  filtro <- function(v) purrr::keep(v, ~ stringr::str_detect(.x,
    stringr::regex("P[o\u00f3]s.Doutorad|supervis\u00e3o|est[\u00e1a]gio.+p[o\u00f3]s", ignore_case = TRUE)))

  res <- .orientacoes_textos(doc, filtro)
  if (length(res$textos) == 0) return(na_ret)

  txts <- res$textos
  aluno <- stringr::str_match(txts, "^\\s*([^\\.]+)\\.")[, 2] |> stringr::str_squish()
  ano   <- .parse_ano(txts)

  txt1 <- sub("^\\s*[^\\.]+\\.\\s*", "", txts)
  titulo <- vapply(txt1, function(t) {
    pos <- regexpr("\\b[12][0-9]{3}\\b", t)
    tit <- if (pos > 0) substr(t, 1, pos - 1) else t
    tit <- stringr::str_remove(stringr::str_squish(tit),
      stringr::regex("[.;,:\\s]*(?:In[\u00edi]cio|Ano)[.;,:\\s]*$", ignore_case = TRUE))
    stringr::str_replace(tit, "[.;,:\\s]+$", "")
  }, character(1))

  inst_m <- stringr::str_match(txts,
    "(?i)(?:P[o\u00f3]s.Doutorad|est[\u00e1a]gio)[^-]*[-\u2013\u2014]\\s*([^.,;\\n]+)")
  instituicao <- inst_m[, 2] |> stringr::str_squish()

  n <- length(txts)
  tibble::tibble(
    aluno       = aluno,
    titulo      = titulo,
    ano         = ano,
    instituicao = instituicao,
    situacao    = res$situacao,
    id_lattes   = rep(id_lattes, n)
  )
}
