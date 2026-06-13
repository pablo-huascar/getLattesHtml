# Helper: parse one artigo-completo div → named character vector
.parse_artigo_div <- function(div) {
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
      } else qs[["doi"]] %||% NA_character_

      # Year lives in span.informacao-artigo[data-tipo-ordenacao="ano"], not in cvuri
      ano_node <- div |> rvest::html_element(
        xpath = ".//span[contains(@class,'informacao-artigo')][@data-tipo-ordenacao='ano']"
      )
      ano <- if (!inherits(ano_node, "xml_missing")) {
        rvest::html_text2(ano_node)
      } else {
        .cvuri_field(qs, "anoPublicacao", "ano", "ano_artigo")
      }

      return(c(
        titulo          = .cvuri_field(qs, "titulo"),
        periodico       = .cvuri_field(qs, "nomePeriodico"),
        issn            = .cvuri_field(qs, "issn"),
        volume          = .cvuri_field(qs, "volume"),
        numero          = .cvuri_field(qs, "issue", "numero"),
        pagina_inicial  = .cvuri_field(qs, "paginaInicial"),
        pagina_final    = .cvuri_field(qs, "paginaFinal"),
        doi             = doi,
        ano             = ano
      ))
    }
  }

  # Fallback: parse the visible citation text
  txt <- div |> rvest::html_text2() |> stringr::str_squish()
  c(
    titulo         = NA_character_,
    periodico      = NA_character_,
    issn           = NA_character_,
    volume         = stringr::str_match(txt, "(?i)v\\.\\s*(\\d+)")[, 2],
    numero         = stringr::str_match(txt, "(?i)n\\.\\s*(\\d+)")[, 2],
    pagina_inicial = stringr::str_match(txt, "p\\.\\s*(\\d+)-")[, 2],
    pagina_final   = stringr::str_match(txt, "-(\\d+)\\s*\\.")[, 2],
    doi            = NA_character_,
    ano            = .parse_ano(txt)
  )
}

#' Extract published journal articles
#'
#' @inheritParams get_id
#' @return A tibble with columns: ano, titulo, issn, periodico, volume, numero,
#'   pagina_inicial, pagina_final, doi, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_artigos_publicados(html)
#' @export
get_artigos_publicados <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    ano = NA_character_, titulo = NA_character_,
    issn = NA_character_, periodico = NA_character_,
    volume = NA_character_, numero = NA_character_,
    pagina_inicial = NA_character_, pagina_final = NA_character_,
    doi = NA_character_, id_lattes = id_lattes
  )

  divs <- doc |> rvest::html_elements("div.artigo-completo")
  if (length(divs) == 0) return(na_ret)

  registros <- lapply(divs, .parse_artigo_div)

  tibble::tibble(
    ano            = sapply(registros, `[[`, "ano"),
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
#' The Lattes HTML format does not expose accepted-not-yet-published articles
#' as a separate section. This function always returns a single-row NA tibble.
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
  tibble::tibble(
    ano = NA_character_, titulo = NA_character_,
    issn = NA_character_, periodico = NA_character_,
    volume = NA_character_, numero = NA_character_,
    pagina_inicial = NA_character_, pagina_final = NA_character_,
    doi = NA_character_, id_lattes = id_lattes
  )
}
