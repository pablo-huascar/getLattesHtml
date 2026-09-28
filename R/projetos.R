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

  # The PP_ anchor and the cells of its project are siblings inside the
  # section's data-cell; a project's cells run until the next sibling anchor.
  # Tag each cell with the anchor that precedes it so fields never leak from
  # one project into the next (or into the sections after the last project).
  n_pp_antes <- "count(preceding-sibling::a[starts-with(@name,'PP_')])"

  for (i in seq_along(pp_nos)) {
    nd <- pp_nos[[i]]
    k  <- xml2::xml_find_num(nd, n_pp_antes) + 1
    celulas <- function(classe) xml2::xml_find_all(nd, sprintf(paste0(
      "following-sibling::div[contains(@class,'%s')][%s = %d]",
      "//div[contains(@class,'layout-cell-pad-5')]"), classe, n_pp_antes, k))

    # Period: the bold text of the first layout-cell-3 after the anchor
    txts3 <- celulas("layout-cell-3") |> rvest::html_text2() |> stringr::str_squish()
    periodos[i] <- .nz(txts3[nzchar(txts3)][1] %||% NA_character_)

    txts9 <- celulas("layout-cell-9") |> rvest::html_text2() |> stringr::str_squish()
    txts9 <- txts9[nzchar(txts9)]

    # Title: the link text of the PP_ anchor itself, or the first cell-9
    tit_self <- rvest::html_text2(nd) |> stringr::str_squish()
    titulos[i] <- if (nzchar(tit_self)) tit_self else .nz(txts9[1] %||% NA_character_)
    txts9 <- txts9[-1]

    # All labels may share a single cell-9 block ("Descri\u00e7\u00e3o: ... Situa\u00e7\u00e3o: ...;
    # Natureza: .... Integrantes: ..."), so extract each value up to the next label.
    rotulos <- paste0(
      .rotulo("Descri\u00e7\u00e3o"), "|", .rotulo("Situa\u00e7\u00e3o"), "|Natureza|",
      "Alunos envolvidos|Integrantes|Membros|Financiador(?:\\(es\\)|es)?|",
      .rotulo("N\u00famero de produ\u00e7\u00f5es")
    )
    pega_campo <- function(rotulo, strip = "[;\\s]+$") {
      bloco <- txts9[stringr::str_detect(txts9,
        stringr::regex(paste0(rotulo, ":"), ignore_case = TRUE))][1]
      if (is.na(bloco %||% NA_character_)) return(NA_character_)
      m <- stringr::str_match(bloco, stringr::regex(paste0(
        rotulo, ":\\s*(.*?)(?=\\s*\\b(?:", rotulos, ")\\s*:|$)"
      ), ignore_case = TRUE))
      .nz(stringr::str_remove(stringr::str_squish(m[, 2]), strip))
    }

    descricoes[i]    <- pega_campo(.rotulo("Descri\u00e7\u00e3o"), strip = "[;\\s]+$|(?<=\\.)\\.+\\s*$")
    situacoes[i]     <- pega_campo(.rotulo("Situa\u00e7\u00e3o"), strip = "[;.\\s]+$")
    naturezas[i]     <- pega_campo("Natureza", strip = "[;.\\s]+$")
    integrantes_v[i] <- pega_campo("(?:Integrantes|Membros)")
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
