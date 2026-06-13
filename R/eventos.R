# Helper: parse a trabalho-em-evento transform text
.parse_trabalho_evento <- function(txt) {
  txt <- stringr::str_remove(txt, "^\\d+\\.\\s*")
  ano <- .parse_ano(txt)

  tipo <- stringr::str_match(txt, "\\(([^)]+)\\)")[, 2] |> stringr::str_squish()

  autores_m <- stringr::str_match(txt,
    "^((?:[A-Z\u00c1\u00c9\u00cd\u00d3\u00da\u00c3\u00d5\u00c2\u00ca\u00d4\u00c0\u00c7][^a-z\u00e1\u00e9\u00ed\u00f3\u00fa\u00e3\u00f5\u00e2\u00ea\u00f4\u00e0\u00e7\\.]{0,5}[^\\.]+\\.\\s*)+)")
  autores <- if (!is.na(autores_m[, 2])) stringr::str_squish(autores_m[, 2]) else NA_character_

  autores_match <- autores_m[, 1] %||% ""
  n_match <- nchar(autores_match)
  resto <- if (n_match > 0) substr(txt, n_match + 1, nchar(txt)) else txt
  titulo_raw <- sub(paste0("\\s*", if (!is.na(ano)) ano else "\\d{4}", ".*$"), "", resto) |>
    stringr::str_squish()
  titulo <- if (nzchar(titulo_raw %||% "")) titulo_raw else NA_character_

  evento <- NA_character_

  c(autores = autores, titulo = titulo, ano = ano, tipo = tipo, evento = evento)
}

#' Extract work presented at events (presentations/talks)
#'
#' Returns items from the "Apresentações de Trabalho" section of the Lattes
#' curriculum. For complete papers in congress proceedings, use
#' [get_trabalhos_anais_congresso()].
#'
#' @inheritParams get_id
#' @return A tibble with columns: autores, titulo, ano, tipo, evento, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_trabalhos_em_eventos(html)
#' @export
get_trabalhos_em_eventos <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    autores = NA_character_, titulo = NA_character_,
    ano = NA_character_, tipo = NA_character_,
    evento = NA_character_, id_lattes = id_lattes
  )

  # In Lattes HTML: ApresentacoesTrabalho \u2192 ProducaoTecnica \u2192 ... \u2192 Bancas \u2192 Eventos
  anchors_pre <- c("ApresentacoesTrabalho", "ApresentacoesTrabalhoEPCTA", "ApresTrabemEventos")
  anchors_pos <- c("ProducaoTecnica", "ProcessosTecnicas", "AssessoriaConsultoria",
                   "EntrevistasMesasRedondas", "RedesSociais", "DemaisProducaoTecnica",
                   "ProducaoArtisticaCultural", "DemaisTrabalhos", "Bancas")

  txts <- .transforms_entre_strict(doc, anchors_pre, anchors_pos)
  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_trabalho_evento)

  tibble::tibble(
    autores   = sapply(parsed, `[[`, "autores"),
    titulo    = sapply(parsed, `[[`, "titulo"),
    ano       = sapply(parsed, `[[`, "ano"),
    tipo      = sapply(parsed, `[[`, "tipo"),
    evento    = sapply(parsed, `[[`, "evento"),
    id_lattes = rep(id_lattes, length(parsed))
  )
}

#' Extract papers published in congress proceedings
#'
#' @inheritParams get_id
#' @return A tibble with columns: autores, titulo, ano, tipo, evento, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_trabalhos_anais_congresso(html)
#' @export
get_trabalhos_anais_congresso <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    autores = NA_character_, titulo = NA_character_,
    ano = NA_character_, tipo = NA_character_,
    evento = NA_character_, id_lattes = id_lattes
  )

  # In Lattes HTML: TrabalhosPublicadosAnaisCongresso \u2192 ApresentacoesTrabalho
  anchors_pre <- c("TrabalhosPublicadosAnaisCongresso")
  anchors_pos <- c("ApresentacoesTrabalho", "ApresentacoesTrabalhoEPCTA",
                   "ProducaoTecnica", "AssessoriaConsultoria")

  txts <- .transforms_entre_strict(doc, anchors_pre, anchors_pos)

  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_trabalho_evento)

  tibble::tibble(
    autores   = sapply(parsed, `[[`, "autores"),
    titulo    = sapply(parsed, `[[`, "titulo"),
    ano       = sapply(parsed, `[[`, "ano"),
    tipo      = sapply(parsed, `[[`, "tipo"),
    evento    = sapply(parsed, `[[`, "evento"),
    id_lattes = rep(id_lattes, length(parsed))
  )
}

#' Extract participation in congresses and events
#'
#' @inheritParams get_id
#' @return A tibble with columns: titulo, ano, tipo, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_eventos_congressos(html)
#' @export
get_eventos_congressos <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    titulo = NA_character_, ano = NA_character_,
    tipo = NA_character_, id_lattes = id_lattes
  )

  # In Lattes HTML: ... \u2192 Bancas \u2192 Eventos \u2192 ParticipacaoEventos \u2192 OrganizacaoEventos
  txts <- .transforms_entre_strict(doc,
    c("ParticipacaoEventos", "EventosCongressos", "EventosCongressosPartOutros"),
    c("OrganizacaoEventos", "Orientacoes", "Orientacoesconcluidas",
      "OutrasInformacoesRelevantes", "PotencialInovacao"))
  if (length(txts) == 0) return(na_ret)

  tibble::tibble(
    titulo    = stringr::str_match(txts, "^[^\\.]+\\.\\s*([^\\.]+)")[, 2] |> stringr::str_squish(),
    ano       = .parse_ano(txts),
    tipo      = stringr::str_match(txts, "\\(([^)]+)\\)")[, 2] |> stringr::str_squish(),
    id_lattes = rep(id_lattes, length(txts))
  )
}

#' Extract event organization activities
#'
#' @inheritParams get_id
#' @return A tibble with columns: autores, titulo, ano, tipo, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_organizacao_eventos(html)
#' @export
get_organizacao_eventos <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    autores = NA_character_, titulo = NA_character_,
    ano = NA_character_, tipo = NA_character_,
    id_lattes = id_lattes
  )

  # In Lattes HTML: ParticipacaoEventos \u2192 OrganizacaoEventos \u2192 Orientacoes
  txts <- .transforms_entre_strict(doc,
    c("OrganizacaoEventos", "OrganizacaoDeEventos"),
    c("Orientacoes", "Orientacoesconcluidas", "OrientacaoEmAndamento",
      "OutrasInformacoesRelevantes", "PotencialInovacao", "ProjetoEnsino"))
  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_trabalho_evento)

  tibble::tibble(
    autores   = sapply(parsed, `[[`, "autores"),
    titulo    = sapply(parsed, `[[`, "titulo"),
    ano       = sapply(parsed, `[[`, "ano"),
    tipo      = sapply(parsed, `[[`, "tipo"),
    id_lattes = rep(id_lattes, length(parsed))
  )
}
