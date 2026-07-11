#' Extract professional activities
#'
#' Returns one row per institutional bond (vínculo). Each row records the
#' institution, period, and the raw vínculo text from Lattes.
#'
#' @inheritParams get_id
#' @return A tibble with columns: instituicao, periodo, vinculo, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_atuacoes_profissionais(html)
#' @export
get_atuacoes_profissionais <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    instituicao = NA_character_, periodo = NA_character_,
    vinculo = NA_character_, id_lattes = id_lattes
  )

  secao <- .secao_data_cell(doc, "AtuacaoProfissional")
  if (inherits(secao, "xml_missing")) return(na_ret)

  # Get all vínculo-carrying cell-9 nodes (pad-5 divs that contain "Vínculo:")
  # Using XPath — the 'í' may render as-is or as UTF-8; use contains for safety
  nos_vinculo <- secao |> xml2::xml_find_all(
    ".//div[contains(@class,'layout-cell-9')]//div[contains(@class,'layout-cell-pad-5')][contains(.,'nculo:')]"
  )

  if (length(nos_vinculo) == 0) return(na_ret)

  # For each vínculo node, find the closest preceding inst_back and period
  inst_names <- character(length(nos_vinculo))
  periodos   <- character(length(nos_vinculo))
  vinculos   <- character(length(nos_vinculo))

  for (i in seq_along(nos_vinculo)) {
    nd <- nos_vinculo[[i]]

    inst_b <- xml2::xml_find_first(nd, "preceding::div[@class='inst_back'][1]/b")
    inst_names[i] <- if (inherits(inst_b, "xml_missing")) NA_character_ else
      stringr::str_squish(rvest::html_text2(inst_b))

    per_nd <- xml2::xml_find_first(
      nd,
      "preceding::div[contains(@class,'layout-cell-3') and not(contains(@class,'subtit-1'))][1]//b"
    )
    periodos[i] <- if (inherits(per_nd, "xml_missing")) NA_character_ else
      stringr::str_squish(rvest::html_text2(per_nd))

    vinculos[i] <- stringr::str_squish(rvest::html_text2(nd)) |>
      stringr::str_remove("^V[\u00edi]nculo:\\s*")
  }

  tibble::tibble(
    instituicao = inst_names,
    periodo     = periodos,
    vinculo     = vinculos,
    id_lattes   = rep(id_lattes, length(nos_vinculo))
  )
}
