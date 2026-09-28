# Helper: parse a trabalho-em-evento transform text. Two layouts:
#   anais:        "AUTORES . Titulo. In: EVENTO, ANO, CIDADE. Anais..., ANO. p. 1-2."
#   apresentacao: "AUTORES . Titulo. ANO. (Apresenta\u00e7\u00e3o de Trabalho/Comunica\u00e7\u00e3o)."
# The author block is split off with .split_autores_titulo(), which handles
# initials ("SILVA, J. A.") and names written in full.
.parse_trabalho_evento <- function(txt) {
  txt <- stringr::str_squish(stringr::str_remove(txt, "^\\d+\\.\\s*"))
  rx_ano <- "(?:1[89]|20)\\d{2}"

  at      <- .split_autores_titulo(txt)
  autores <- at[["autores"]]
  resto   <- at[["resto"]]

  # Trailing "(TIPO)." \u2014 allows one nested pair: "(Outra (Mesa redonda))"
  tipo <- stringr::str_match(resto,
    "\\(((?:[^()]|\\([^()]*\\))*)\\)\\s*\\.?\\s*$")[, 2]
  sem_tipo <- if (is.na(tipo)) resto else
    stringr::str_remove(resto, "\\s*\\((?:[^()]|\\([^()]*\\))*\\)\\s*\\.?\\s*$")

  m_in <- stringr::str_match(sem_tipo, "^(.*?)[.\\s]*\\bIn:\\s*(.*)$")
  if (!is.na(m_in[, 1])) {
    titulo <- m_in[, 2]
    cauda  <- m_in[, 3]
    m_ev   <- stringr::str_match(cauda, paste0("^(.*?),\\s*(", rx_ano, ")\\b"))
    if (!is.na(m_ev[, 1])) {
      evento <- m_ev[, 2]
      ano    <- m_ev[, 3]
    } else {
      evento <- stringr::str_remove(cauda, "\\.\\s.*$")
      ano    <- .parse_ano(cauda)
    }
  } else {
    evento <- NA_character_
    m_ano  <- stringr::str_match(sem_tipo, paste0("^(.*?)[.,\\s]*\\b(", rx_ano, ")[.\\s]*$"))
    if (!is.na(m_ano[, 1])) {
      titulo <- m_ano[, 2]
      ano    <- m_ano[, 3]
    } else {
      titulo <- sem_tipo
      ano    <- .parse_ano(txt)
    }
  }
  if (is.na(ano)) ano <- .parse_ano(txt)

  limpa <- function(x) .nz(stringr::str_squish(stringr::str_remove(x %||% "", "[.,;:\\s]+$")))
  c(autores = autores, titulo = limpa(titulo), ano = ano,
    tipo = .nz(stringr::str_squish(tipo)), evento = limpa(evento))
}

# Kind of proceedings entry from its subsection header
.tipo_anais <- function(subsecao) {
  s <- ifelse(is.na(subsecao), "", subsecao)
  out <- .nz(s)
  out[stringr::str_detect(s, stringr::regex("expandido", ignore_case = TRUE))] <- "Resumo expandido"
  out[stringr::str_detect(s, stringr::regex("^Resumos publicados", ignore_case = TRUE))] <- "Resumo"
  out[stringr::str_detect(s, stringr::regex("^Trabalhos completos", ignore_case = TRUE))] <- "Trabalho completo"
  out
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

  txts <- .itens_secao(doc, c("ApresentacoesTrabalho", "ApresTrabemEventos"))$textos
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
#' Covers the three proceedings subsections of Lattes; `tipo` tells them
#' apart: "Trabalho completo", "Resumo expandido" or "Resumo".
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

  it <- .itens_secao(doc, "TrabalhosPublicadosAnaisCongresso")
  txts <- it$textos
  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_trabalho_evento)

  tibble::tibble(
    autores   = sapply(parsed, `[[`, "autores"),
    titulo    = sapply(parsed, `[[`, "titulo"),
    ano       = sapply(parsed, `[[`, "ano"),
    tipo      = .tipo_anais(it$subsecao),
    evento    = sapply(parsed, `[[`, "evento"),
    id_lattes = rep(id_lattes, length(parsed))
  )
}

# "EVENTO.TITULO DO TRABALHO. ANO. (TIPO)." — the work title is optional and
# Lattes repeats the event name when there is none.
.parse_participacao_evento <- function(txt) {
  rx_tipo <- "\\s*\\(((?:[^()]|\\([^()]*\\))*)\\)\\s*\\.?\\s*$"
  tipo <- stringr::str_match(txt, rx_tipo)[, 2]
  corpo <- stringr::str_remove(txt, rx_tipo)
  m_ano <- stringr::str_match(corpo, "^(.*?)[.,\\s]*\\b((?:1[89]|20)\\d{2})[.\\s]*$")
  if (!is.na(m_ano[, 1])) {
    ano <- m_ano[, 3]
    corpo <- m_ano[, 2]
  } else {
    ano <- .parse_ano(txt)
  }
  partes <- stringr::str_match(corpo, "^(.+?)\\.+\\s*(\\S.*)?$")
  if (is.na(partes[, 1])) {
    evento <- corpo
    titulo <- NA_character_
  } else {
    evento <- partes[, 2]
    titulo <- partes[, 3]
  }
  limpa <- function(x) .nz(stringr::str_squish(stringr::str_remove(x %||% "", "[.,;:\\s]+$")))
  evento <- limpa(evento)
  titulo <- limpa(titulo)
  if (!is.na(titulo) && !is.na(evento) && tolower(titulo) == tolower(evento)) {
    titulo <- NA_character_
  }
  c(evento = evento, titulo = titulo, ano = ano, tipo = .nz(stringr::str_squish(tipo)))
}

#' Extract participation in congresses and events
#'
#' @inheritParams get_id
#' @return A tibble with columns: evento, titulo (title of the work presented,
#'   when informed), ano, tipo, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_eventos_congressos(html)
#' @export
get_eventos_congressos <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    evento = NA_character_, titulo = NA_character_, ano = NA_character_,
    tipo = NA_character_, id_lattes = id_lattes
  )

  txts <- .itens_secao(doc,
    c("ParticipacaoEventos", "EventosCongressos", "EventosCongressosPartOutros"))$textos
  if (length(txts) == 0) return(na_ret)

  parsed <- lapply(txts, .parse_participacao_evento)

  tibble::tibble(
    evento    = sapply(parsed, `[[`, "evento"),
    titulo    = sapply(parsed, `[[`, "titulo"),
    ano       = sapply(parsed, `[[`, "ano"),
    tipo      = sapply(parsed, `[[`, "tipo"),
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

  txts <- .itens_secao(doc, c("OrganizacaoEventos", "OrganizacaoDeEventos"))$textos
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
