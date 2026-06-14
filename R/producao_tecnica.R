# Helper: parse a generic technical production transform text
.parse_prod_tecnica <- function(txt) {
  txt <- stringr::str_remove(txt, "^\\d+\\.\\s*")
  ano <- .parse_ano(txt)

  tipo <- stringr::str_match(txt, "\\(([^)]+)\\)")[, 2] |> stringr::str_squish()

  # Separate the author block from the title. In Lattes citation style the
  # author list is terminated by a lone period surrounded by spaces (" . ").
  # Author initials such as "F. P. H. A." never have a space *before* the
  # period, so this split reliably isolates the authors whether names are
  # abbreviated to initials or spelled out.
  partes <- stringr::str_split_fixed(txt, "\\s+\\.\\s+", 2)
  if (nzchar(partes[, 2])) {
    autores <- stringr::str_squish(partes[, 1])
    resto   <- partes[, 2]
  } else {
    autores <- NA_character_
    resto   <- txt
  }

  titulo_raw <- sub(paste0("\\s*", if (!is.na(ano)) ano else "\\d{4}", ".*$"), "", resto) |>
    stringr::str_squish()
  titulo <- if (nzchar(titulo_raw %||% "")) titulo_raw else NA_character_

  c(autores = autores, titulo = titulo, ano = ano, tipo = tipo)
}

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

  # Collect from all technical-production subsections (stop before patent and demais sections)
  # Subsection order: ProducaoTecnica \u2192 AssessoriaConsultoria \u2192 ProdutosTecnologicos \u2192
  #   TrabalhosTecnicos \u2192 EntrevistasMesasRedondas \u2192 RedesSociais \u2192 DemaisProducaoTecnica
  all_stops <- c("ProcessosTecnicas", "SoftwareSemPatente",
                 "DemaisProducaoTecnica", "ProducaoArtisticaCultural",
                 "DemaisTrabalhos", "Bancas")
  txts <- unique(c(
    .transforms_entre_strict(doc, c("ProducaoTecnica"),
      c("AssessoriaConsultoria", "ProdutosTecnologicos", "TrabalhosTecnicos",
        "EntrevistasMesasRedondas", "RedesSociais", all_stops)),
    .transforms_entre_strict(doc, c("AssessoriaConsultoria"),
      c("ProdutosTecnologicos", "TrabalhosTecnicos",
        "EntrevistasMesasRedondas", "RedesSociais", all_stops)),
    .transforms_entre_strict(doc, c("ProdutosTecnologicos"),
      c("TrabalhosTecnicos", "EntrevistasMesasRedondas", "RedesSociais", all_stops)),
    .transforms_entre_strict(doc, c("TrabalhosTecnicos"),
      c("EntrevistasMesasRedondas", "RedesSociais", all_stops)),
    .transforms_entre_strict(doc, c("EntrevistasMesasRedondas"),
      c("RedesSociais", all_stops)),
    .transforms_entre_strict(doc, c("RedesSociais"), all_stops)
  ))

  # Exclude patent entries (covered by get_patentes)
  txts <- txts[!stringr::str_detect(txts,
    stringr::regex("Patente|Registro de (Programa|Software)|Modelo de Utilidade|Desenho Industrial",
                   ignore_case = TRUE))]

  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_prod_tecnica)

  tibble::tibble(
    autores   = sapply(parsed, `[[`, "autores"),
    titulo    = sapply(parsed, `[[`, "titulo"),
    ano       = sapply(parsed, `[[`, "ano"),
    tipo      = sapply(parsed, `[[`, "tipo"),
    id_lattes = rep(id_lattes, length(parsed))
  )
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

  # In Lattes HTML: ... \u2192 DemaisProducaoTecnica \u2192 ProducaoArtisticaCultural/DemaisTrabalhos \u2192 Bancas
  txts <- .transforms_entre_strict(doc,
    c("DemaisProducaoTecnica", "OutrasProducoesTecnicas", "OutrosProducoesTecnicas"),
    c("ProducaoArtisticaCultural", "DemaisTrabalhos", "Bancas",
      "ParticipacaoBancasTrabalho", "Eventos", "ParticipacaoEventos"))
  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_prod_tecnica)

  tibble::tibble(
    autores   = sapply(parsed, `[[`, "autores"),
    titulo    = sapply(parsed, `[[`, "titulo"),
    ano       = sapply(parsed, `[[`, "ano"),
    tipo      = sapply(parsed, `[[`, "tipo"),
    id_lattes = rep(id_lattes, length(parsed))
  )
}

#' Extract patents and software registrations
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

  # Collect from patent subsections, stopping before next sibling subsection
  txts <- unique(c(
    .transforms_entre_strict(doc,
      c("ProcessosTecnicas", "PatentesRegistros", "ProcessoOuTecnica"),
      c("SoftwareSemPatente", "ProdutosTecnologicos", "AssessoriaConsultoria",
        "TrabalhosTecnicos", "EntrevistasMesasRedondas", "RedesSociais",
        "DemaisProducaoTecnica", "ProducaoArtisticaCultural", "DemaisTrabalhos", "Bancas")),
    .transforms_entre_strict(doc,
      c("SoftwareSemPatente"),
      c("ProdutosTecnologicos", "AssessoriaConsultoria", "TrabalhosTecnicos",
        "EntrevistasMesasRedondas", "RedesSociais", "DemaisProducaoTecnica",
        "ProducaoArtisticaCultural", "DemaisTrabalhos", "Bancas"))
  ))
  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_prod_tecnica)

  tibble::tibble(
    autores   = sapply(parsed, `[[`, "autores"),
    titulo    = sapply(parsed, `[[`, "titulo"),
    ano       = sapply(parsed, `[[`, "ano"),
    tipo      = sapply(parsed, `[[`, "tipo"),
    id_lattes = rep(id_lattes, length(parsed))
  )
}
