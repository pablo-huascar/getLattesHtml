#' Extract research and extension project participation
#'
#' Returns one row per project found in the ProjetosPesquisa,
#' ProjetosExtensao and OutrosProjetos sections.
#'
#' @inheritParams get_id
#' @return A tibble with columns: titulo, periodo, descricao, situacao,
#'   natureza, integrantes, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_participacao_projeto(html)
#' @export
get_participacao_projeto <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    titulo = NA_character_, periodo = NA_character_,
    descricao = NA_character_, situacao = NA_character_,
    natureza = NA_character_, integrantes = NA_character_,
    id_lattes = id_lattes
  )

  # Projects are identified by anchors starting with "PP_"
  pp_nos <- doc |> xml2::xml_find_all("//a[starts-with(@name,'PP_')]")
  if (length(pp_nos) == 0) return(na_ret)

  titulos     <- character(length(pp_nos))
  periodos    <- character(length(pp_nos))
  descricoes  <- character(length(pp_nos))
  situacoes   <- character(length(pp_nos))
  naturezas   <- character(length(pp_nos))
  integrantes_v <- character(length(pp_nos))

  for (i in seq_along(pp_nos)) {
    nd <- pp_nos[[i]]

    # Period: in the layout-cell-3 that is closest ancestor's sibling
    per_nd <- xml2::xml_find_first(nd,
      "ancestor::div[contains(@class,'layout-cell-3')][1]//b")
    periodos[i] <- if (inherits(per_nd, "xml_missing")) NA_character_ else
      stringr::str_squish(rvest::html_text2(per_nd))

    # Title: the link text of the PP_ anchor itself, or sibling cell-9
    tit_self <- rvest::html_text2(nd) |> stringr::str_squish()
    if (nzchar(tit_self)) {
      titulos[i] <- tit_self
    } else {
      tit_nd <- xml2::xml_find_first(nd,
        "following::div[contains(@class,'layout-cell-9')][1]//div[contains(@class,'layout-cell-pad-5')]")
      titulos[i] <- if (inherits(tit_nd, "xml_missing")) NA_character_ else
        stringr::str_squish(rvest::html_text2(tit_nd))
    }

    # Remaining cell-9s for this project (up to the next PP_ anchor)
    # Use "following" axis limited to before next PP_ anchor
    if (i < length(pp_nos)) {
      next_pp <- xml2::xml_find_first(pp_nos[[i]], sprintf(
        "following::a[starts-with(@name,'PP_')][1]"
      ))
      if (!inherits(next_pp, "xml_missing")) {
        nos9 <- xml2::xml_find_all(nd, paste0(
          "following::div[contains(@class,'layout-cell-9')]",
          "//div[contains(@class,'layout-cell-pad-5')]",
          "[following::a[starts-with(@name,'PP_')]]"
        ))
      } else {
        nos9 <- xml2::xml_find_all(nd,
          "following::div[contains(@class,'layout-cell-9')]//div[contains(@class,'layout-cell-pad-5')]")
      }
    } else {
      nos9 <- xml2::xml_find_all(nd,
        "following::div[contains(@class,'layout-cell-9')]//div[contains(@class,'layout-cell-pad-5')]")
    }

    txts9 <- nos9 |> rvest::html_text2() |> stringr::str_squish()
    txts9 <- txts9[nzchar(txts9)]

    # Classify cell-9 blocks
    desc_txt <- txts9[stringr::str_detect(txts9, stringr::regex("Descri[\u00e7c][\u00e3a]o", ignore_case = TRUE))][1]
    sit_txt  <- txts9[stringr::str_detect(txts9, stringr::regex("Situa[\u00e7c][\u00e3a]o", ignore_case = TRUE))][1]
    nat_txt  <- txts9[stringr::str_detect(txts9, stringr::regex("Natureza", ignore_case = TRUE))][1]
    int_txt  <- txts9[stringr::str_detect(txts9, stringr::regex("Integrantes|Membros", ignore_case = TRUE))][1]

    descricoes[i]    <- stringr::str_remove(desc_txt %||% NA_character_,
      stringr::regex("^Descri[\u00e7c][\u00e3a]o:\\s*", ignore_case = TRUE)) |> stringr::str_squish()
    situacoes[i]     <- stringr::str_remove(sit_txt %||% NA_character_,
      stringr::regex("^Situa[\u00e7c][\u00e3a]o:\\s*", ignore_case = TRUE)) |> stringr::str_squish()
    naturezas[i]     <- stringr::str_remove(nat_txt %||% NA_character_,
      stringr::regex("^Natureza:\\s*", ignore_case = TRUE)) |> stringr::str_squish()
    integrantes_v[i] <- stringr::str_remove(int_txt %||% NA_character_,
      stringr::regex("^Integrantes:\\s*", ignore_case = TRUE)) |> stringr::str_squish()
  }

  tibble::tibble(
    titulo      = titulos,
    periodo     = periodos,
    descricao   = descricoes,
    situacao    = situacoes,
    natureza    = naturezas,
    integrantes = integrantes_v,
    id_lattes   = rep(id_lattes, length(pp_nos))
  )
}
