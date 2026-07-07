#' Extract the 16-digit Lattes identifier
#'
#' @param caminho_html Path to a Lattes HTML curriculum file.
#' @param encoding File encoding (default `"ISO-8859-1"`).
#' @return A one-row tibble with column `id_lattes`.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_id(html)
#' @export
get_id <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  tibble::tibble(id_lattes = .get_id_lattes(doc))
}

#' Extract general personal data
#'
#' Returns a single-row tibble with nome, id_lattes, data_atualizacao, resumo,
#' nome_em_citacoes (list-column), orcid, pais_nacionalidade and
#' endereco_profissional.
#'
#' @inheritParams get_id
#' @return A tibble with one row.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_dados_gerais(html)
#' @export
get_dados_gerais <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  info <- doc |> rvest::html_elements(".informacoes-autor li") |> rvest::html_text2()

  data_atualizacao <- info[stringr::str_detect(info, stringr::regex("atualiza", ignore_case = TRUE))] |>
    stringr::str_extract("\\d{2}/\\d{2}/\\d{4}") |>
    (\(v) v[!is.na(v)][1])() %||% NA_character_

  secao_id <- doc |> rvest::html_element(
    xpath = "//a[@name='Identificacao']/following-sibling::div[contains(@class,'layout-cell')]"
  )
  secao_end <- doc |> rvest::html_element(
    xpath = "//a[@name='Endereco']/following-sibling::div[contains(@class,'layout-cell')]"
  )

  fetch_label <- function(secao, label) {
    if (inherits(secao, "xml_missing") || length(secao) == 0) return(NA_character_)
    node <- secao |> rvest::html_element(xpath = paste0(
      ".//b[normalize-space(text())='", label, "']",
      "/ancestor::div[contains(@class,'layout-cell-3')]",
      "/following-sibling::div[contains(@class,'layout-cell-9')]",
      "//div[contains(@class,'layout-cell-pad-5')]"
    ))
    if (inherits(node, "xml_missing") || length(node) == 0) NA_character_
    else rvest::html_text2(node)
  }

  nome <- .nz(fetch_label(secao_id, "Nome"))

  cit_raw <- fetch_label(secao_id, "Nome em cita\u00e7\u00f5es bibliogr\u00e1ficas")
  nome_em_citacoes <- if (!is.na(cit_raw) && nzchar(cit_raw)) {
    stringr::str_split(cit_raw, ";")[[1]] |> stringr::str_trim() |> (\(v) v[nzchar(v)])()
  } else character(0)

  orcid_txt <- fetch_label(secao_id, "Orcid iD")
  orcid <- stringr::str_extract(orcid_txt %||% NA_character_, "https?://\\S+")

  pais_nacionalidade <- .nz(fetch_label(secao_id, "Pa\u00eds de Nacionalidade"))

  resumo_nos <- doc |> rvest::html_elements(".resumo")
  resumo <- if (length(resumo_nos) == 0) NA_character_ else {
    txts <- resumo_nos |> rvest::html_text2() |> stringr::str_squish()
    (txts[nzchar(txts)][1] %||% txts[1]) %||% NA_character_
  }

  end_txt <- fetch_label(secao_end, "Endere\u00e7o Profissional")
  endereco_profissional <- if (is.na(end_txt)) NA_character_ else
    stringr::str_squish(stringr::str_replace_all(end_txt, "\n", " "))

  tibble::tibble(
    nome = nome,
    id_lattes = id_lattes,
    data_atualizacao = data_atualizacao,
    resumo = resumo,
    nome_em_citacoes = list(nome_em_citacoes),
    orcid = orcid,
    pais_nacionalidade = pais_nacionalidade,
    endereco_profissional = endereco_profissional
  )
}

#' Extract professional address
#'
#' @inheritParams get_id
#' @return A tibble with columns: logradouro, complemento, cep, bairro, cidade,
#'   uf, pais, caixa_postal, telefone, ramal, fax, endereco_eletronico, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_endereco_profissional(html)
#' @export
get_endereco_profissional <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    logradouro = NA_character_, complemento = NA_character_,
    cep = NA_character_, bairro = NA_character_,
    cidade = NA_character_, uf = NA_character_,
    pais = NA_character_, caixa_postal = NA_character_,
    telefone = NA_character_, ramal = NA_character_,
    fax = NA_character_, endereco_eletronico = NA_character_,
    id_lattes = id_lattes
  )

  secao <- doc |> rvest::html_element(
    xpath = "//a[@name='Endereco']/following-sibling::div[contains(@class,'data-cell')]"
  )
  if (inherits(secao, "xml_missing")) return(na_ret)

  fetch_label <- function(label) {
    node <- secao |> rvest::html_element(xpath = paste0(
      ".//b[normalize-space(text())='", label, "']",
      "/ancestor::div[contains(@class,'layout-cell-3')]",
      "/following-sibling::div[contains(@class,'layout-cell-9')]",
      "//div[contains(@class,'layout-cell-pad-5')]"
    ))
    if (inherits(node, "xml_missing") || length(node) == 0) NA_character_
    else .nz(rvest::html_text2(node))
  }

  tibble::tibble(
    logradouro        = fetch_label("Logradouro"),
    complemento       = fetch_label("Complemento"),
    cep               = fetch_label("CEP"),
    bairro            = fetch_label("Bairro"),
    cidade            = fetch_label("Cidade"),
    uf                = fetch_label("UF"),
    pais              = fetch_label("Pa\u00eds"),
    caixa_postal      = fetch_label("Caixa Postal"),
    telefone          = fetch_label("Telefone"),
    ramal             = fetch_label("Ramal"),
    fax               = fetch_label("Fax"),
    endereco_eletronico = fetch_label("Endere\u00e7o eletr\u00f4nico"),
    id_lattes = id_lattes
  )
}

#' Extract languages
#'
#' @inheritParams get_id
#' @return A tibble with columns: idioma, compreende, fala, le, escreve, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_idiomas(html)
#' @export
get_idiomas <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    idioma = NA_character_, compreende = NA_character_,
    fala = NA_character_, le = NA_character_,
    escreve = NA_character_, id_lattes = id_lattes
  )

  secao <- .secao_data_cell(doc, "Idiomas")
  if (inherits(secao, "xml_missing")) return(na_ret)

  nomes <- secao |>
    rvest::html_elements("div.layout-cell-3 div.layout-cell-pad-5 b") |>
    rvest::html_text2() |> stringr::str_squish()
  profs <- secao |>
    rvest::html_elements("div.layout-cell-9 div.layout-cell-pad-5") |>
    rvest::html_text2() |> stringr::str_squish()

  n <- min(length(nomes), length(profs))
  if (n == 0) return(na_ret)

  nomes <- nomes[seq_len(n)]
  profs <- profs[seq_len(n)]

  parse_nivel <- function(txt, campo) {
    pat <- paste0("(?i)", campo, "\\s+([^,\\.]+)")
    m <- stringr::str_match(txt, pat)
    .nz(stringr::str_squish(m[, 2]))
  }

  tibble::tibble(
    idioma     = nomes,
    compreende = parse_nivel(profs, "Compreende"),
    fala       = parse_nivel(profs, "Fala"),
    le         = parse_nivel(profs, "L[e\u00ea]"),
    escreve    = parse_nivel(profs, "Escreve"),
    id_lattes  = rep(id_lattes, n)
  )
}

#' Extract research/activity areas
#'
#' @inheritParams get_id
#' @return A tibble with columns: numero, grande_area, area, subarea,
#'   especialidade, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_areas_atuacao(html)
#' @export
get_areas_atuacao <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    numero = NA_character_, grande_area = NA_character_,
    area = NA_character_, subarea = NA_character_,
    especialidade = NA_character_, id_lattes = id_lattes
  )

  secao <- .secao_data_cell(doc, "AreasAtuacao")
  if (inherits(secao, "xml_missing")) return(na_ret)

  txts <- secao |>
    rvest::html_elements("div.layout-cell-9 div.layout-cell-pad-5") |>
    rvest::html_text2() |> stringr::str_squish()
  txts <- txts[nzchar(txts)]
  if (length(txts) == 0) return(na_ret)

  extr <- function(txt, campo) {
    m <- stringr::str_match(txt, paste0("(?i)", campo, ":\\s*([^/\\.]+)"))
    .nz(stringr::str_squish(m[, 2]))
  }

  tibble::tibble(
    numero      = as.character(seq_along(txts)),
    grande_area = extr(txts, "Grande [\u00e1a]rea"),
    area        = extr(txts, "(?:Grande [\u00e1a]rea[^/]*/\\s*)?[\u00e1A]rea"),
    subarea     = extr(txts, "Sub[\u00e1a]rea"),
    especialidade = extr(txts, "Especialidade"),
    id_lattes   = rep(id_lattes, length(txts))
  )
}

#' Extract research lines
#'
#' @inheritParams get_id
#' @return A tibble with columns: numero, linha, objetivo, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_linha_pesquisa(html)
#' @export
get_linha_pesquisa <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  na_ret <- tibble::tibble(
    numero = NA_character_, linha = NA_character_,
    objetivo = NA_character_, id_lattes = id_lattes
  )

  secao <- .secao_data_cell(doc, "LinhaPesquisa")
  if (inherits(secao, "xml_missing")) return(na_ret)

  txts9 <- secao |>
    rvest::html_elements("div.layout-cell-9 div.layout-cell-pad-5") |>
    rvest::html_text2() |> stringr::str_squish()
  txts3 <- secao |>
    rvest::html_elements("div.layout-cell-3 div.layout-cell-pad-5") |>
    rvest::html_text2() |> stringr::str_squish()

  if (length(txts9) == 0) return(na_ret)

  # Pairs alternate: (numero/empty, line_name), (empty, objetivo)
  # Identify objetivo entries
  eh_obj <- stringr::str_detect(txts9, stringr::regex("^Objetivo:", ignore_case = TRUE))

  linhas   <- txts9[!eh_obj]
  objetivos <- txts9[eh_obj] |>
    stringr::str_remove(stringr::regex("^Objetivo:\\s*", ignore_case = TRUE))

  numeros3 <- txts3[nzchar(txts3) & stringr::str_detect(txts3, "^\\d")]
  numeros <- if (length(numeros3) >= length(linhas)) numeros3[seq_along(linhas)] else
    as.character(seq_along(linhas))

  n <- length(linhas)
  if (n == 0) return(na_ret)

  tibble::tibble(
    numero    = numeros[seq_len(n)],
    linha     = linhas,
    objetivo  = .fix_len(objetivos, n),
    id_lattes = rep(id_lattes, n)
  )
}
