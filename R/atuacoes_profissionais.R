# Parse the vinculo cell text into its labelled fields. The Lattes format is
# "Vínculo: TIPO, Atividade: A, Enquadramento Funcional: X, Carga horária: N,
# Regime: Y." — commas separate the fields and each label ends in ":". Label
# matching is case-insensitive and tolerant to mojibake (some CVs were
# downloaded with the accents already replaced by U+FFFD, e.g.
# "V?nculo"/"Carga hor?ria"). Any labelled field that is not one of the known
# ones is collected into `outras_informacoes`.
.parse_vinculo <- function(txt) {
  txt <- stringr::str_squish(txt) |>
    stringr::str_remove("(?i)^V\\S{0,6}nculo\\s*:\\s*") |>
    stringr::str_remove("\\.\\s*$")

  # A label starts with an uppercase letter, runs up to a ":" without crossing a
  # comma, and is at most a few words long. Using [^,:] (rather than \w) keeps
  # the split working when accents were replaced by U+FFFD in the source.
  partes <- stringr::str_split_1(
    txt, ",\\s*(?=\\p{Lu}[^,:]{0,40}:)"
  )

  campos <- c(
    vinculo = .nz(stringr::str_squish(partes[1])),
    atividade = NA_character_,
    enquadramento_funcional = NA_character_,
    carga_horaria = NA_character_,
    regime = NA_character_,
    outras_informacoes = NA_character_
  )
  extras <- character(0)
  for (p in partes[-1]) {
    valor <- .nz(stringr::str_squish(sub("^[^:]*:\\s*", "", p)))
    if (stringr::str_detect(p, "(?i)^Atividade")) {
      campos[["atividade"]] <- valor
    } else if (stringr::str_detect(p, "(?i)^Enquadramento")) {
      campos[["enquadramento_funcional"]] <- valor
    } else if (stringr::str_detect(p, "(?i)^Carga")) {
      campos[["carga_horaria"]] <- valor
    } else if (stringr::str_detect(p, "(?i)^Regime")) {
      campos[["regime"]] <- valor
    } else if (!is.na(valor)) {
      extras <- c(extras, stringr::str_squish(p))
    }
  }
  if (length(extras) > 0) {
    campos[["outras_informacoes"]] <- paste(extras, collapse = "; ")
  }
  campos
}

#' Extract professional activities
#'
#' Returns one row per institutional bond (vínculo). The labelled fields of
#' the vínculo cell ("Atividade", "Enquadramento Funcional", "Carga horária",
#' "Regime") are split into their own columns; any other labelled field goes to
#' "outras_informacoes".
#'
#' @inheritParams get_id
#' @return A tibble with columns: instituicao, periodo, vinculo, atividade,
#'   enquadramento_funcional, carga_horaria, regime, outras_informacoes,
#'   id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_atuacoes_profissionais(html)
#' @export
get_atuacoes_profissionais <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    instituicao = NA_character_, periodo = NA_character_,
    vinculo = NA_character_, atividade = NA_character_,
    enquadramento_funcional = NA_character_,
    carga_horaria = NA_character_, regime = NA_character_,
    outras_informacoes = NA_character_, id_lattes = id_lattes
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
  campos     <- vector("list", length(nos_vinculo))

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

    campos[[i]] <- .parse_vinculo(rvest::html_text2(nd))
  }

  tibble::tibble(
    instituicao             = inst_names,
    periodo                 = periodos,
    vinculo                 = sapply(campos, `[[`, "vinculo"),
    atividade               = sapply(campos, `[[`, "atividade"),
    enquadramento_funcional = sapply(campos, `[[`, "enquadramento_funcional"),
    carga_horaria           = sapply(campos, `[[`, "carga_horaria"),
    regime                  = sapply(campos, `[[`, "regime"),
    outras_informacoes      = sapply(campos, `[[`, "outras_informacoes"),
    id_lattes               = rep(id_lattes, length(nos_vinculo))
  )
}
