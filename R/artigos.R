# Squish and drop symmetric surrounding double quotes some authors use
.limpa_titulo <- function(x) {
  if (is.na(x)) return(NA_character_)
  x <- stringr::str_squish(x)
  .nz(stringr::str_squish(stringr::str_remove_all(x, "^\"|\"$")))
}

# Titulo from the visible citation: some CVs leave titulo= empty in cvuri but
# quote the title in the citation text.
.titulo_do_texto <- function(txt) {
  stringr::str_match(txt, "\"\\s*(.+?)\\s*\"")[, 2]
}

# Periodico from the visible citation tail: ". PERIODICO, v. X, p. A-B, ANO."
.periodico_do_texto <- function(txt) {
  m <- stringr::str_match(txt,
    "[.\"]\\s*([^,]+),\\s*v\\.[^,]*,\\s*p\\.[^,]*,\\s*(?:1[89]|20)\\d{2}\\s*\\.?\\s*$")
  per <- m[, 2]
  if (!is.na(per)) {
    per <- stringr::str_remove(per, "^[\\s.]+")
    # Drop a leading page range left by free-form citations ("113-116.. NOME")
    per <- stringr::str_remove(per, "^[\\d\\s-]+\\.+\\s*")
  }
  .nz(stringr::str_squish(per))
}

# Bibliographic tail of a journal citation: ", v. X, n. Y, p. A-B, ANO."
# Every part but the year is optional.
.cauda_periodico <- function(txt) {
  m <- stringr::str_match(txt, paste0(
    "(?:,\\s*v\\.\\s*([^,]*?))?",
    "(?:,\\s*n\\.\\s*([^,]*?))?",
    "(?:,\\s*p\\.\\s*([^,]*?))?",
    ",\\s*((?:1[89]|20)\\d{2})\\s*\\.?\\s*$"
  ))
  pags <- stringr::str_match(m[, 4] %||% "", "^\\s*([^-\\s]+)\\s*-\\s*([^-\\s]+)\\s*$")
  c(
    volume         = .nz(stringr::str_squish(m[, 2])),
    numero         = .nz(stringr::str_squish(m[, 3])),
    pagina_inicial = .nz(if (!is.na(pags[, 2])) pags[, 2] else stringr::str_squish(m[, 4])),
    pagina_final   = .nz(pags[, 3]),
    ano            = m[, 5]
  )
}

# Citation text of an article, without the hidden sort keys
# (span.informacao-artigo) that Lattes prepends to it.
.citacao_artigo <- function(div) {
  tr <- div |> rvest::html_element("span.transform")
  if (inherits(tr, "xml_missing")) tr <- div
  txt <- rvest::html_text2(tr) |> stringr::str_squish()
  info <- tr |> rvest::html_elements("span.informacao-artigo") |>
    rvest::html_text2() |> paste(collapse = "")
  info <- stringr::str_squish(info)
  if (nzchar(info) && startsWith(txt, info)) {
    txt <- stringr::str_squish(substring(txt, nchar(info) + 1))
  }
  txt
}

# Helper: parse one artigo-completo div → named character vector
.parse_artigo_div <- function(div) {
  txt <- .citacao_artigo(div)
  cauda <- .cauda_periodico(txt)
  autores <- .split_autores_titulo(txt)[["autores"]]

  # Try cvuri attribute first (structured data)
  span_cit <- div |> rvest::html_element("span.citacoes[cvuri]")
  if (!inherits(span_cit, "xml_missing")) {
    raw <- rvest::html_attr(span_cit, "cvuri")
    if (!is.na(raw) && nzchar(raw)) {
      qs <- .parse_cvuri(raw)
      doi_node <- div |> rvest::html_element("a.icone-doi")
      doi <- if (!inherits(doi_node, "xml_missing")) {
        href <- rvest::html_attr(doi_node, "href")
        stringr::str_remove(href %||% "", "https?://(?:dx\\.)?doi\\.org/")
      } else .cvuri_field(qs, "doi")

      # Year lives in span.informacao-artigo[data-tipo-ordenacao="ano"], not in cvuri
      ano_node <- div |> rvest::html_element(
        xpath = ".//span[contains(@class,'informacao-artigo')][@data-tipo-ordenacao='ano']"
      )
      ano <- if (!inherits(ano_node, "xml_missing")) {
        .nz(rvest::html_text2(ano_node))
      } else {
        .cvuri_field(qs, "anoPublicacao", "ano", "ano_artigo")
      }
      if (is.na(ano)) ano <- cauda[["ano"]]

      # cvuri may carry empty titulo/nomePeriodico; fall back to the citation
      titulo <- .cvuri_field(qs, "titulo")
      if (is.na(titulo)) titulo <- .titulo_do_texto(txt)
      periodico <- .cvuri_field(qs, "nomePeriodico")
      if (is.na(periodico)) periodico <- .periodico_do_texto(txt)

      # cvuri never carries the final page and usually leaves issue empty,
      # so those come from the visible citation tail.
      return(c(
        autores         = autores,
        titulo          = .limpa_titulo(titulo),
        periodico       = periodico,
        issn            = .cvuri_field(qs, "issn"),
        volume          = .cvuri_field(qs, "volume") %||% cauda[["volume"]],
        numero          = .cvuri_field(qs, "issue", "numero") %||% cauda[["numero"]],
        pagina_inicial  = .cvuri_field(qs, "paginaInicial") %||% cauda[["pagina_inicial"]],
        pagina_final    = .cvuri_field(qs, "paginaFinal") %||% cauda[["pagina_final"]],
        doi             = .nz(doi),
        ano             = ano
      ))
    }
  }

  # Fallback: parse the visible citation text
  c(
    autores        = autores,
    titulo         = .limpa_titulo(.titulo_do_texto(txt)),
    periodico      = .periodico_do_texto(txt),
    issn           = NA_character_,
    volume         = cauda[["volume"]],
    numero         = cauda[["numero"]],
    pagina_inicial = cauda[["pagina_inicial"]],
    pagina_final   = cauda[["pagina_final"]],
    doi            = NA_character_,
    ano            = cauda[["ano"]] %||% .parse_ano(txt)
  )
}

#' Extract published journal articles
#'
#' @inheritParams get_id
#' @return A tibble with columns: ano, autores, titulo, issn, periodico,
#'   volume, numero, pagina_inicial, pagina_final, doi, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_artigos_publicados(html)
#' @export
get_artigos_publicados <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    ano = NA_character_, autores = NA_character_, titulo = NA_character_,
    issn = NA_character_, periodico = NA_character_,
    volume = NA_character_, numero = NA_character_,
    pagina_inicial = NA_character_, pagina_final = NA_character_,
    doi = NA_character_, id_lattes = id_lattes
  )

  # The "Educação e Popularização de C&T" section repeats articles already
  # listed above it, reusing div.artigo-completo; skip that copy.
  divs <- xml2::xml_find_all(doc, paste0(
    "//div[contains(concat(' ', normalize-space(@class), ' '), ' artigo-completo ')]",
    "[not(preceding::a[@name='EducacaoPopularizacaoCTA' or @name='ArtigosEPCTA'])]"
  ))
  if (length(divs) == 0) return(na_ret)

  registros <- lapply(divs, .parse_artigo_div)

  tibble::tibble(
    ano            = sapply(registros, `[[`, "ano"),
    autores        = sapply(registros, `[[`, "autores"),
    titulo         = sapply(registros, `[[`, "titulo"),
    issn           = sapply(registros, `[[`, "issn"),
    periodico      = sapply(registros, `[[`, "periodico"),
    volume         = sapply(registros, `[[`, "volume"),
    numero         = sapply(registros, `[[`, "numero"),
    pagina_inicial = sapply(registros, `[[`, "pagina_inicial"),
    pagina_final   = sapply(registros, `[[`, "pagina_final"),
    doi            = sapply(registros, `[[`, "doi"),
    id_lattes      = rep(id_lattes, length(registros))
  )
}

#' Extract articles accepted for publication
#'
#' Parses the "Artigos aceitos para publicação" section. Accepted
#' articles carry fewer fields than published ones (usually no volume, issue,
#' or pages yet).
#'
#' @inheritParams get_id
#' @return A tibble with the same columns as [get_artigos_publicados()].
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_artigos_aceitos(html)
#' @export
get_artigos_aceitos <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    ano = NA_character_, autores = NA_character_, titulo = NA_character_,
    issn = NA_character_, periodico = NA_character_,
    volume = NA_character_, numero = NA_character_,
    pagina_inicial = NA_character_, pagina_final = NA_character_,
    doi = NA_character_, id_lattes = id_lattes
  )

  spans <- .spans_entre_strict(doc, "ArtigosAceitos",
    c("LivrosCapitulos", "TextosJornaisRevistas",
      "TrabalhosPublicadosAnaisCongresso", "ApresentacoesTrabalho",
      "OutrasProducoesBibliograficas", "ProducaoTecnica"))
  if (length(spans) == 0) return(na_ret)

  registros <- lapply(spans, .parse_artigo_aceito)
  registros <- registros[!duplicated(sapply(registros, `[[`, "chave"))]

  tibble::tibble(
    ano            = sapply(registros, `[[`, "ano"),
    autores        = sapply(registros, `[[`, "autores"),
    titulo         = sapply(registros, `[[`, "titulo"),
    issn           = sapply(registros, `[[`, "issn"),
    periodico      = sapply(registros, `[[`, "periodico"),
    volume         = sapply(registros, `[[`, "volume"),
    numero         = sapply(registros, `[[`, "numero"),
    pagina_inicial = sapply(registros, `[[`, "pagina_inicial"),
    pagina_final   = sapply(registros, `[[`, "pagina_final"),
    doi            = sapply(registros, `[[`, "doi"),
    id_lattes      = rep(id_lattes, length(registros))
  )
}

# Position of the last "." outside parentheses, or NA if none
.ultimo_ponto_fora_parens <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  prof <- 0L
  pos <- NA_integer_
  for (i in seq_along(chars)) {
    ch <- chars[[i]]
    if (ch == "(") {
      prof <- prof + 1L
    } else if (ch == ")" && prof > 0L) {
      prof <- prof - 1L
    } else if (ch == "." && prof == 0L) {
      pos <- i
    }
  }
  pos
}

# Parse one accepted-article span:
# "AUTORES . Titulo. PERIODICO, ANO." (ISSN comes from the JCR img attribute)
.parse_artigo_aceito <- function(span) {
  txt <- span |> rvest::html_text2() |> stringr::str_squish()

  issn_nd <- xml2::xml_find_first(span, ".//img[@data-issn]")
  issn <- if (inherits(issn_nd, "xml_missing")) NA_character_ else
    xml2::xml_attr(issn_nd, "data-issn")

  doi_nd <- xml2::xml_find_first(span, ".//a[contains(@class,'icone-doi')]")
  doi <- if (inherits(doi_nd, "xml_missing")) NA_character_ else
    stringr::str_remove(xml2::xml_attr(doi_nd, "href") %||% "",
                        "https?://(?:dx\\.)?doi\\.org/")

  at    <- .split_autores_titulo(txt)
  resto <- at[["resto"]]

  # Tail: "TITULO. PERIODICO, ANO." — the split between titulo and periodico
  # is the last period outside parentheses, so journal names such as
  # "PSICO (PUCRS. ONLINE)" stay whole.
  m_ano <- stringr::str_match(resto, ",\\s*((?:1[89]|20)\\d{2})\\s*\\.?\\s*$")
  if (!is.na(m_ano[, 2])) {
    ano   <- m_ano[, 2]
    corpo <- stringr::str_remove(resto, ",\\s*(?:1[89]|20)\\d{2}\\s*\\.?\\s*$")
    pos   <- .ultimo_ponto_fora_parens(corpo)
    if (!is.na(pos)) {
      titulo    <- stringr::str_sub(corpo, 1, pos - 1)
      periodico <- stringr::str_sub(corpo, pos + 1)
    } else {
      titulo    <- corpo
      periodico <- NA_character_
    }
    titulo    <- stringr::str_squish(titulo)
    periodico <- .nz(stringr::str_squish(periodico))
  } else {
    titulo    <- stringr::str_squish(stringr::str_remove(resto, "[.\\s]+$"))
    periodico <- NA_character_
    ano       <- .parse_ano(resto)
  }

  list(
    chave = txt, ano = ano, autores = at[["autores"]], titulo = titulo, issn = issn,
    periodico = periodico,
    volume         = stringr::str_match(txt, "(?i)\\bv\\.\\s*(\\d+)")[, 2],
    numero         = stringr::str_match(txt, "(?i)\\bn\\.\\s*(\\d+)")[, 2],
    pagina_inicial = stringr::str_match(txt, "p\\.\\s*(\\d+)-")[, 2],
    pagina_final   = stringr::str_match(txt, "p\\.\\s*\\d+-(\\d+)")[, 2],
    doi = doi
  )
}
