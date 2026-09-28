# Shared anchors for orientation sections
.anchors_concluidas  <- c("Orientacoesconcluidas", "OrientacoesConcluidas")
# "Orientacoes" = general section (em andamento); "Orientacaoemandamento" = alternate name
.anchors_emandamento <- c(
  "Orientacoes", "Orientacaoemandamento", "OrientacaoEmAndamento",
  "Orientacoesemandamento", "OrientacoesEmAndamento"
)

# Helper: collect and classify orientation texts. Items are taken from the
# orientation sections only; the situation comes from the group header
# ("Orienta\u00e7\u00f5es e supervis\u00f5es conclu\u00eddas"/"em andamento") and the kind from
# the subsection header ("Tese de doutorado", "Supervis\u00e3o de p\u00f3s-doutorado").
# `sub_pat` selects by subsection; `filtro_fn` is the fallback for items
# without a subsection header.
.orientacoes_textos <- function(doc, sub_pat, filtro_fn) {
  it <- .itens_secao(doc, c(.anchors_emandamento, .anchors_concluidas))

  situacao <- ifelse(
    !is.na(it$grupo) &
      stringr::str_detect(it$grupo, stringr::regex("andamento", ignore_case = TRUE)),
    "em andamento",
    ifelse(
      !is.na(it$grupo) &
        stringr::str_detect(it$grupo, stringr::regex("conclu", ignore_case = TRUE)),
      "conclu\u00edda",
      ifelse(it$secao %in% .anchors_concluidas, "conclu\u00edda", "em andamento")
    )
  )

  sub <- ifelse(is.na(it$subsecao), "", it$subsecao)
  keep <- ifelse(
    is.na(it$subsecao),
    it$textos %in% filtro_fn(it$textos),
    stringr::str_detect(sub, stringr::regex(sub_pat, ignore_case = TRUE))
  )

  ordem <- order(situacao[keep] != "conclu\u00edda")
  list(
    textos   = it$textos[keep][ordem],
    situacao = situacao[keep][ordem]
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

  # The course may hold one nested pair of parentheses:
  # "Disserta\u00e7\u00e3o (Mestrado em Ci\u00eancias Biol\u00f3gicas (Biof\u00edsica)) - UFRJ"
  parens <- "\\(((?:[^()]|\\([^()]*\\))*)\\)"
  curso_m <- stringr::str_match(txt,
    paste0("(?i)", tipo_label, "\\s*", parens))
  curso <- if (!is.na(curso_m[, 2])) stringr::str_squish(curso_m[, 2]) else NA_character_

  inst_m <- stringr::str_match(txt,
    paste0("(?i)", tipo_label, "\\s*", parens, "\\s*[-\u2013\u2014]\\s*([^.,;\\n]+)"))
  instituicao <- if (!is.na(inst_m[, 3])) stringr::str_squish(inst_m[, 3]) else NA_character_

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

  res <- .orientacoes_textos(doc, "^Tese", filtro)
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

  res <- .orientacoes_textos(doc, "^Disserta", filtro)
  if (length(res$textos) == 0) return(na_ret)

  # \S rather than \w: damaged downloads carry U+FFFD inside "Dissertacao"
  parsed <- lapply(res$textos, .parse_orientacao,
    "(?i)Disserta\\S*\\s*\\(", "Disserta\\S*?")
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

  res <- .orientacoes_textos(doc, "p\\S{1,6}s.doutor|^Supervis", filtro)
  if (length(res$textos) == 0) return(na_ret)

  # "Nome. [Titulo.] 2011. Universidade do Minho, [Agencia]. Supervisor."
  # "Nome. In\u00edcio: 2023. Universidade X, Agencia."
  txts <- res$textos
  m <- stringr::str_match(
    stringr::str_remove(txts, "(?i)\\bIn\\S{1,3}cio\\s*:\\s*"),
    paste0(
      "^\\s*([^.]+)\\.\\s*",                # aluno
      "(?:(.*?)[.\\s]+)?",                  # titulo (optional)
      "((?:1[89]|20)\\d{2})\\.\\s*",        # ano
      "([^,.]*)"                            # instituicao
    )
  )
  aluno       <- .nz(stringr::str_squish(m[, 2]))
  titulo      <- .nz(stringr::str_squish(m[, 3]))
  ano         <- ifelse(is.na(m[, 4]), .parse_ano(txts), m[, 4])
  instituicao <- .nz(stringr::str_squish(m[, 5]))

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
