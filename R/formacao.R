# Helper: get cell-3/cell-9 pairs from a forma\u00e7\u00e3o section. `t9` is
# squished; `t9_linhas` keeps the line breaks (<br>) of each cell, which
# delimit the labelled fields ("Título:", "Orientador:").
.formacao_pares <- function(doc, ancora) {
  secao <- .secao_data_cell(doc, ancora)
  if (inherits(secao, "xml_missing")) {
    return(list(t3 = character(), t9 = character(), t9_linhas = list()))
  }

  # The Oasisbr link after a thesis title carries a hidden tooltip ("O Portal
  # Brasileiro de Publicações...") that would otherwise leak into the title.
  xml2::xml_remove(xml2::xml_find_all(secao, ".//a[contains(@class,'tooltip-oasis')]"))

  t3 <- secao |>
    rvest::html_elements("div.layout-cell-3 div.layout-cell-pad-5") |>
    rvest::html_text2() |> stringr::str_squish()
  brutos <- secao |>
    rvest::html_elements("div.layout-cell-9 div.layout-cell-pad-5") |>
    rvest::html_text2()
  t9_linhas <- lapply(strsplit(brutos, "\n", fixed = TRUE), function(l) {
    l <- stringr::str_squish(l)
    l[nzchar(l)]
  })

  list(t3 = t3, t9 = stringr::str_squish(brutos), t9_linhas = t9_linhas)
}

.rx_graduacao <- function() {
  paste0(
    .rotulo("Gradua\u00e7\u00e3o"), "|Ensino ", .rotulo("M\u00e9dio"), "|",
    .rotulo("Aperfei\u00e7oamento"), "|", .rotulo("Especializa\u00e7\u00e3o")
  )
}

# Parse one academic-degree cell:
# "NIVEL em CURSO. \n INSTITUICAO, SIGLA, PAIS. \n Título: X, Ano de obtenção:
#  AAAA. \n Orientador: Y. \n Bolsista do(a): ..."
.parse_formacao <- function(txt, linhas, nivel_rx) {
  curso <- stringr::str_match(txt, stringr::regex(
    paste0("^(?:", nivel_rx, ")(?:\\s+em\\s+)?([^.\n]+)"), ignore_case = TRUE))[, 2]
  instituicao <- stringr::str_match(txt, stringr::regex(
    paste0("^(?:", nivel_rx, ")[^.]*\\.\\s*([^,.]+)"), ignore_case = TRUE))[, 2]

  rx_tit <- stringr::regex(paste0("^", .rotulo("T\u00edtulo"), "\\s*:\\s*"), ignore_case = TRUE)
  rx_ano <- stringr::regex(paste0(",?\\s*Ano de ", .rotulo("obten\u00e7\u00e3o"),
                                  "\\s*:\\s*((?:1[89]|20)\\d{2}).*$"), ignore_case = TRUE)
  lin_tit <- linhas[stringr::str_detect(linhas, rx_tit)][1]
  lin_ano <- linhas[stringr::str_detect(linhas, rx_ano)][1]
  titulo <- if (is.na(lin_tit)) NA_character_ else
    lin_tit |> stringr::str_remove(rx_tit) |> stringr::str_remove(rx_ano) |>
      stringr::str_remove("[.,\\s]+$")
  ano <- if (is.na(lin_ano)) NA_character_ else stringr::str_match(lin_ano, rx_ano)[, 2]

  # The main advisor has its own line; a sandwich-period advisor appears in
  # parentheses on the institution line and is ignored.
  rx_ori <- stringr::regex("^Orientador(?:\\(a\\)|/a|a)?\\s*:\\s*", ignore_case = TRUE)
  lin_ori <- linhas[stringr::str_detect(linhas, rx_ori)][1]
  orientador <- if (is.na(lin_ori)) NA_character_ else
    lin_ori |> stringr::str_remove(rx_ori) |> stringr::str_remove("[.\\s]+$")

  c(curso = .nz(stringr::str_squish(curso)),
    instituicao = .nz(stringr::str_squish(instituicao)),
    titulo = .nz(stringr::str_squish(titulo)), ano = ano,
    orientador = .nz(stringr::str_squish(orientador)))
}

# Rows of FormacaoAcademicaTitulacao whose cell starts with `nivel_rx`
.formacao_nivel <- function(doc, nivel_rx) {
  pares <- .formacao_pares(doc, "FormacaoAcademicaTitulacao")
  idx <- which(stringr::str_detect(pares$t9,
    stringr::regex(paste0("^(?:", nivel_rx, ")"), ignore_case = TRUE)))
  if (length(idx) == 0) return(NULL)

  periodos <- if (length(pares$t3) >= max(idx)) pares$t3[idx] else rep(NA_character_, length(idx))
  parsed <- Map(.parse_formacao, pares$t9[idx], pares$t9_linhas[idx], nivel_rx)
  campo <- function(nm) unname(vapply(parsed, `[[`, character(1), nm))
  list(
    periodo = periodos, curso = campo("curso"), instituicao = campo("instituicao"),
    titulo = campo("titulo"), ano = campo("ano"), orientador = campo("orientador")
  )
}

#' Extract undergraduate formation
#'
#' Also returns "Ensino Médio", "Aperfeiçoamento" and "Especialização"
#' entries.
#'
#' @inheritParams get_id
#' @return A tibble with columns: periodo, curso, instituicao, titulo,
#'   orientador, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_formacao_graduacao(html)
#' @export
get_formacao_graduacao <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    periodo = NA_character_, curso = NA_character_,
    instituicao = NA_character_, titulo = NA_character_,
    orientador = NA_character_, id_lattes = id_lattes
  )

  f <- .formacao_nivel(doc, .rx_graduacao())
  if (is.null(f)) return(na_ret)

  tibble::tibble(
    periodo     = f$periodo,
    curso       = f$curso,
    instituicao = f$instituicao,
    titulo      = f$titulo,
    orientador  = f$orientador,
    id_lattes   = rep(id_lattes, length(f$curso))
  )
}

#' Extract master's formation
#'
#' @inheritParams get_id
#' @return A tibble with columns: periodo, curso, instituicao, titulo, ano,
#'   orientador, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_formacao_mestrado(html)
#' @export
get_formacao_mestrado <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    periodo = NA_character_, curso = NA_character_,
    instituicao = NA_character_, titulo = NA_character_,
    ano = NA_character_, orientador = NA_character_,
    id_lattes = id_lattes
  )

  f <- .formacao_nivel(doc, "Mestrado")
  if (is.null(f)) return(na_ret)

  tibble::tibble(
    periodo     = f$periodo,
    curso       = f$curso,
    instituicao = f$instituicao,
    titulo      = f$titulo,
    ano         = f$ano,
    orientador  = f$orientador,
    id_lattes   = rep(id_lattes, length(f$curso))
  )
}

#' Extract doctoral formation
#'
#' @inheritParams get_id
#' @return A tibble with columns: periodo, curso, instituicao, titulo, ano,
#'   orientador, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_formacao_doutorado(html)
#' @export
get_formacao_doutorado <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    periodo = NA_character_, curso = NA_character_,
    instituicao = NA_character_, titulo = NA_character_,
    ano = NA_character_, orientador = NA_character_,
    id_lattes = id_lattes
  )

  f <- .formacao_nivel(doc, "Doutorado")
  if (is.null(f)) return(na_ret)

  tibble::tibble(
    periodo     = f$periodo,
    curso       = f$curso,
    instituicao = f$instituicao,
    titulo      = f$titulo,
    ano         = f$ano,
    orientador  = f$orientador,
    id_lattes   = rep(id_lattes, length(f$curso))
  )
}

#' Extract post-doctoral formation
#'
#' @inheritParams get_id
#' @return A tibble with columns: periodo, area, instituicao, id_lattes.
#'   `area` is the most specific knowledge area informed ("Área", or
#'   "Grande área" when that is all there is).
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_formacao_pos_doutorado(html)
#' @export
get_formacao_pos_doutorado <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    periodo = NA_character_, area = NA_character_,
    instituicao = NA_character_, id_lattes = id_lattes
  )

  rx_pos <- paste0("P", .rotulo("\u00f3"), "s.", "[Dd]outor")

  # Try dedicated section first
  pares <- .formacao_pares(doc, "FormacaoAcademicaPosDoutorado")

  if (length(pares$t9) == 0) {
    # Fallback: look for p\u00f3s-doutorado entries within main FormacaoAcademica section
    pares2 <- .formacao_pares(doc, "FormacaoAcademicaTitulacao")
    idx <- which(stringr::str_detect(pares2$t9, stringr::regex(rx_pos, ignore_case = TRUE)))
    if (length(idx) == 0) return(na_ret)
    pares <- list(
      t3 = if (length(pares2$t3) >= max(idx)) pares2$t3[idx] else rep(NA_character_, length(idx)),
      t9 = pares2$t9[idx]
    )
  }

  idx <- which(nzchar(pares$t9))
  if (length(idx) == 0) return(na_ret)

  conteudos <- pares$t9[idx]
  periodos <- if (length(pares$t3) >= max(idx))
    pares$t3[idx] else rep(NA_character_, length(idx))

  # "Pós-Doutorado. INSTITUICAO, SIGLA, PAIS. ... Grande área: X / Área: Y"
  instituicao <- stringr::str_match(conteudos, stringr::regex(
    paste0(rx_pos, "[^.]*\\.\\s*([^,.]+)"), ignore_case = TRUE))[, 2]
  area_esp <- stringr::str_match(conteudos,
    paste0("(?<![Gg]rande )(?<!Sub)", .rotulo("\u00c1"), "rea:\\s*([^/.]+)"))[, 2]
  grande <- stringr::str_match(conteudos, stringr::regex(
    paste0("Grande ", .rotulo("\u00e1"), "rea:\\s*(.+?)(?=\\s*/|\\s+Grande|\\.|$)"),
    ignore_case = TRUE))[, 2]
  area <- ifelse(is.na(area_esp), grande, area_esp)

  tibble::tibble(
    periodo     = periodos,
    area        = .nz(stringr::str_squish(area)),
    instituicao = .nz(stringr::str_squish(instituicao)),
    id_lattes   = rep(id_lattes, length(conteudos))
  )
}

#' Extract complementary formation
#'
#' @inheritParams get_id
#' @return A tibble with columns: periodo, curso, horas, instituicao, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_formacao_complementar(html)
#' @export
get_formacao_complementar <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    periodo = NA_character_, curso = NA_character_,
    horas = NA_character_, instituicao = NA_character_,
    id_lattes = id_lattes
  )

  pares <- .formacao_pares(doc, "FormacaoComplementar")
  if (length(pares$t9) == 0) return(na_ret)

  idx <- which(nzchar(pares$t9))
  if (length(idx) == 0) return(na_ret)

  conteudos <- pares$t9[idx]
  linhas    <- pares$t9_linhas[idx]
  periodos  <- if (length(pares$t3) >= max(idx))
    pares$t3[idx] else rep(NA_character_, length(idx))

  # "CURSO. (Carga horária: 30h). \n INSTITUICAO, SIGLA, PAIS."
  instituicao <- vapply(linhas, function(l) {
    if (length(l) < 2) return(NA_character_)
    stringr::str_squish(stringr::str_match(l[2], "^([^,]+)")[, 2])
  }, character(1))
  primeira <- vapply(linhas, function(l) if (length(l)) l[1] else NA_character_, character(1))

  tibble::tibble(
    periodo     = periodos,
    curso       = primeira |>
      stringr::str_remove(stringr::regex("\\s*\\(Carga hor.*$", ignore_case = TRUE)) |>
      stringr::str_remove("[.\\s]+$") |> stringr::str_squish() |> unname() |> .nz(),
    horas       = stringr::str_match(conteudos,
      paste0("(?i)Carga hor", .rotulo("\u00e1"), "ria:\\s*(\\d+h?)"))[, 2],
    instituicao = .nz(unname(instituicao)),
    id_lattes   = rep(id_lattes, length(conteudos))
  )
}
