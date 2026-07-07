# Helper: get cell-3/cell-9 pairs from a forma\u00e7\u00e3o section
.formacao_pares <- function(doc, ancora) {
  secao <- .secao_data_cell(doc, ancora)
  if (inherits(secao, "xml_missing")) return(list(t3 = character(), t9 = character()))

  t3 <- secao |>
    rvest::html_elements("div.layout-cell-3 div.layout-cell-pad-5") |>
    rvest::html_text2() |> stringr::str_squish()
  t9 <- secao |>
    rvest::html_elements("div.layout-cell-9 div.layout-cell-pad-5") |>
    rvest::html_text2() |> stringr::str_squish()

  list(t3 = t3, t9 = t9)
}

#' Extract undergraduate formation
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

  pares <- .formacao_pares(doc, "FormacaoAcademicaTitulacao")
  if (length(pares$t9) == 0) return(na_ret)

  idx <- which(stringr::str_detect(pares$t9,
    stringr::regex("^Gradua[\u00e7c][\u00e3a]o|^Ensino M[\u00e9e]dio|^Aperfei[\u00e7c]oamento|^Especializa[\u00e7c][\u00e3a]o",
                   ignore_case = TRUE)))
  if (length(idx) == 0) return(na_ret)

  conteudos <- pares$t9[idx]
  periodos  <- if (length(pares$t3) >= max(idx)) pares$t3[idx] else rep(NA_character_, length(idx))

  tibble::tibble(
    periodo     = periodos,
    curso       = stringr::str_match(conteudos,
      stringr::regex("^(?:Gradua[\u00e7c][\u00e3a]o|Ensino M[\u00e9e]dio|Aperfei[\u00e7c]oamento|Especializa[\u00e7c][\u00e3a]o)(?:\\s+em\\s+)?([^\\.\n]+)", ignore_case = TRUE))[, 2] |>
      stringr::str_squish(),
    instituicao = stringr::str_match(conteudos,
      "(?i)(?:Gradua[\u00e7c][\u00e3a]o|Ensino M[\u00e9e]dio|Aperfei[\u00e7c]oamento|Especializa[\u00e7c][\u00e3a]o)[^\\n\\.]+\\.\\s*([^,\\.\\n]+)")[, 2] |>
      stringr::str_squish(),
    titulo      = stringr::str_match(conteudos, "(?i)T[i\u00ed]tulo:\\s*([^\n,]+)")[, 2] |>
      stringr::str_remove(",\\s*Ano.*$") |> stringr::str_squish(),
    orientador  = stringr::str_match(conteudos, "(?i)Orientador(?:/a)?:\\s*([^\\.\n]+)")[, 2] |>
      stringr::str_squish(),
    id_lattes   = rep(id_lattes, length(conteudos))
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

  pares <- .formacao_pares(doc, "FormacaoAcademicaTitulacao")
  if (length(pares$t9) == 0) return(na_ret)

  idx <- which(stringr::str_detect(pares$t9,
    stringr::regex("^Mestrado", ignore_case = TRUE)))
  if (length(idx) == 0) return(na_ret)

  conteudos <- pares$t9[idx]
  periodos  <- if (length(pares$t3) >= max(idx)) pares$t3[idx] else rep(NA_character_, length(idx))

  tibble::tibble(
    periodo     = periodos,
    curso       = stringr::str_match(conteudos, "(?i)^Mestrado(?:\\s+em\\s+)?([^\\.\n]+)")[, 2] |>
      stringr::str_squish(),
    instituicao = stringr::str_match(conteudos,
      "(?i)^Mestrado[^\\n\\.]+\\.\\s*([^,\\.\\n]+)")[, 2] |> stringr::str_squish(),
    titulo      = stringr::str_match(conteudos, "(?i)T[i\u00ed]tulo:\\s*([^\n]+)")[, 2] |>
      stringr::str_remove("(?i),?\\s*Ano.*$") |> stringr::str_squish(),
    ano         = stringr::str_match(conteudos,
      "(?i)Ano de obten[\u00e7c][\u00e3a]o:\\s*(\\d{4})")[, 2],
    orientador  = stringr::str_match(conteudos, "(?i)Orientador(?:/a)?:\\s*([^\\.\n]+)")[, 2] |>
      stringr::str_squish(),
    id_lattes   = rep(id_lattes, length(conteudos))
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

  pares <- .formacao_pares(doc, "FormacaoAcademicaTitulacao")
  if (length(pares$t9) == 0) return(na_ret)

  idx <- which(stringr::str_detect(pares$t9,
    stringr::regex("^Doutorado", ignore_case = TRUE)))
  if (length(idx) == 0) return(na_ret)

  conteudos <- pares$t9[idx]
  periodos  <- if (length(pares$t3) >= max(idx)) pares$t3[idx] else rep(NA_character_, length(idx))

  tibble::tibble(
    periodo     = periodos,
    curso       = stringr::str_match(conteudos, "(?i)^Doutorado(?:\\s+em\\s+)?([^\\.\n]+)")[, 2] |>
      stringr::str_squish(),
    instituicao = stringr::str_match(conteudos,
      "(?i)^Doutorado[^\\n\\.]+\\.\\s*([^,\\.\\n]+)")[, 2] |> stringr::str_squish(),
    titulo      = stringr::str_match(conteudos, "(?i)T[i\u00ed]tulo:\\s*([^\n]+)")[, 2] |>
      stringr::str_remove("(?i),?\\s*Ano.*$") |> stringr::str_squish(),
    ano         = stringr::str_match(conteudos,
      "(?i)Ano de obten[\u00e7c][\u00e3a]o:\\s*(\\d{4})")[, 2],
    orientador  = stringr::str_match(conteudos, "(?i)Orientador(?:/a)?:\\s*([^\\.\n]+)")[, 2] |>
      stringr::str_squish(),
    id_lattes   = rep(id_lattes, length(conteudos))
  )
}

#' Extract post-doctoral formation
#'
#' @inheritParams get_id
#' @return A tibble with columns: periodo, area, instituicao, id_lattes.
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

  # Try dedicated section first
  pares <- .formacao_pares(doc, "FormacaoAcademicaPosDoutorado")

  if (length(pares$t9) == 0) {
    # Fallback: look for p\u00f3s-doutorado entries within main FormacaoAcademica section
    pares2 <- .formacao_pares(doc, "FormacaoAcademicaTitulacao")
    idx <- which(stringr::str_detect(pares2$t9,
      stringr::regex("P[o\u00f3]s.Doutorad|P[o\u00f3]s.doutor", ignore_case = TRUE)))
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

  tibble::tibble(
    periodo     = periodos,
    area        = stringr::str_match(conteudos,
      "(?i)P[o\u00f3]s.[Dd]outor(?:ado|al)?(?:\\s+em\\s+)?([^\\.\n,]+)")[, 2] |> stringr::str_squish(),
    instituicao = stringr::str_match(conteudos,
      "(?i)P[o\u00f3]s.doutor[^\\.\n]+\\.\\s*([^,\\.\\n]+)")[, 2] |> stringr::str_squish(),
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
  periodos  <- if (length(pares$t3) >= max(idx))
    pares$t3[idx] else rep(NA_character_, length(idx))

  tibble::tibble(
    periodo     = periodos,
    curso       = stringr::str_match(conteudos, "^([^\\(\\.\n]+)")[, 2] |> stringr::str_squish(),
    horas       = stringr::str_match(conteudos, "(?i)Carga hor[\u00e1a]ria:\\s*(\\d+h?)")[, 2],
    instituicao = stringr::str_match(conteudos, "\\.\\s*([^,\\.\\n]+,\\s*[A-Z]+,\\s*\\w+\\.)")[, 2] |>
      stringr::str_squish(),
    id_lattes   = rep(id_lattes, length(conteudos))
  )
}
