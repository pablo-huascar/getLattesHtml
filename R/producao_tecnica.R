# Helper: parse a generic technical production transform text:
# "AUTORES . Titulo. ANO. (TIPO)." / "AUTOR, Nome. Titulo. ANO; Tema: X. (TIPO)."
# `tipo_padrao` (the subsection header) is used when the entry carries no
# trailing "(TIPO)".
.parse_prod_tecnica <- function(txt, tipo_padrao = NA_character_) {
  txt <- stringr::str_squish(stringr::str_remove(txt, "^\\d+\\.\\s*"))

  at      <- .split_autores_titulo(txt)
  autores <- at[["autores"]]
  resto   <- at[["resto"]]

  tipo <- stringr::str_match(resto,
    "\\(((?:[^()]|\\([^()]*\\))*)\\)\\s*\\.?\\s*$")[, 2]
  tipo <- .nz(stringr::str_squish(tipo))
  if (is.na(tipo)) tipo <- .nz(tipo_padrao)

  # Title runs up to the first ". ANO" (the year closes the title even when
  # the title itself contains a year, as in "Evento Polifonias 2021. 2021.")
  m <- stringr::str_match(resto,
    "^(.*?)(?:\\.|[?!]\\.?)\\s+((?:1[89]|20)\\d{2})(?=[.;,]|\\s*$)")
  if (!is.na(m[, 1])) {
    titulo <- m[, 2]
    ano    <- m[, 3]
  } else {
    titulo <- stringr::str_remove(resto, "\\s*\\((?:[^()]|\\([^()]*\\))*\\)\\s*\\.?\\s*$")
    ano    <- .parse_ano(resto)
  }
  titulo <- .nz(stringr::str_squish(stringr::str_remove(titulo, "[.\\s]+$")))

  c(autores = autores, titulo = titulo, ano = ano, tipo = tipo)
}

.tibble_prod_tecnica <- function(it, id_lattes) {
  parsed <- Map(.parse_prod_tecnica, it$textos, it$subsecao)
  tibble::tibble(
    autores   = unname(sapply(parsed, `[[`, "autores")),
    titulo    = unname(sapply(parsed, `[[`, "titulo")),
    ano       = unname(sapply(parsed, `[[`, "ano")),
    tipo      = unname(sapply(parsed, `[[`, "tipo")),
    id_lattes = rep(id_lattes, length(parsed))
  )
}

# Sections of "Produção técnica" other than patents/registrations and the
# catch-all "Demais tipos de produção técnica"
.anchors_prod_tecnica <- c(
  "ProducaoTecnica", "AssessoriaConsultoria", "SoftwareSemPatente",
  "ProdutosTecnologicos", "ProcessosTecnicas", "TrabalhosTecnicos",
  "EntrevistasMesasRedondas", "RedesSociais"
)
.anchors_patentes <- c(
  "PatentesRegistros", "patente", "Patente", "programaComputador",
  "ProgramaComputador", "desenhoIndustrial", "DesenhoIndustrial",
  "marca", "Marca", "cultivar", "Cultivar", "topografiaCircuito",
  "TopografiaCircuito"
)

#' Extract technical production
#'
#' Covers software, reports, manuals, working papers and similar outputs.
#'
#' @inheritParams get_id
#' @return A tibble with columns: autores, titulo, ano, tipo, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_producao_tecnica(html)
#' @export
get_producao_tecnica <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    autores = NA_character_, titulo = NA_character_,
    ano = NA_character_, tipo = NA_character_,
    id_lattes = id_lattes
  )

  # Subsections: Assessoria e consultoria, Programas de computador sem
  # registro, Produtos tecnol\u00f3gicos, Processos ou t\u00e9cnicas, Trabalhos
  # t\u00e9cnicos, Entrevistas/mesas redondas, Redes sociais. Patents live in their
  # own sections (get_patentes) and "Demais tipos" in
  # get_outras_producoes_tecnicas().
  it <- .itens_secao(doc, .anchors_prod_tecnica)
  if (length(it$textos) == 0) return(na_ret)

  .tibble_prod_tecnica(it, id_lattes)
}

#' Extract other technical productions
#'
#' @inheritParams get_id
#' @return A tibble with columns: autores, titulo, ano, tipo, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_outras_producoes_tecnicas(html)
#' @export
get_outras_producoes_tecnicas <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    autores = NA_character_, titulo = NA_character_,
    ano = NA_character_, tipo = NA_character_,
    id_lattes = id_lattes
  )

  it <- .itens_secao(doc,
    c("DemaisProducaoTecnica", "OutrasProducoesTecnicas", "OutrosProducoesTecnicas"))
  if (length(it$textos) == 0) return(na_ret)

  # The subsection header is just "Demais tipos de produ\u00e7\u00e3o t\u00e9cnica"; it is not
  # a useful default for tipo.
  it$subsecao <- rep(NA_character_, length(it$textos))
  .tibble_prod_tecnica(it, id_lattes)
}

#' Extract patents and software registrations
#'
#' Reads the "Patentes e registros" sections. Unregistered software and
#' "Processos ou técnicas" are technical production in Lattes and are returned
#' by [get_producao_tecnica()].
#'
#' @inheritParams get_id
#' @return A tibble with columns: autores, titulo, ano, tipo, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_patentes(html)
#' @export
get_patentes <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    autores = NA_character_, titulo = NA_character_,
    ano = NA_character_, tipo = NA_character_,
    id_lattes = id_lattes
  )

  it <- .itens_secao(doc, .anchors_patentes)
  if (length(it$textos) == 0) return(na_ret)

  res <- .tibble_prod_tecnica(it, id_lattes)
  # "... 2022, Brasil. Patente: Privilégio de Inovação. Número do registro: ..."
  tipo_pat <- stringr::str_match(it$textos, "\\bPatente:\\s*([^.]+)")[, 2]
  res$tipo <- ifelse(is.na(tipo_pat), res$tipo, stringr::str_squish(tipo_pat))
  res
}
