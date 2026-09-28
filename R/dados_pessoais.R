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

  fetch_label <- .valor_rotulado

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
#' Lattes prints the professional address as a single free-text block, one
#' item per line: institution, street, (complement,) neighbourhood,
#' "CEP - Cidade, UF - Pais", optionally followed by "- Caixa-postal: N",
#' and then labelled lines
#' (Telefone, Ramal, Fax, URL da Homepage). This function splits that block.
#'
#' @inheritParams get_id
#' @return A tibble with columns: instituicao, logradouro, complemento, cep,
#'   bairro, cidade, uf, pais, caixa_postal, telefone, ramal, fax,
#'   endereco_eletronico, homepage, id_lattes.
#' @examples
#' html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")
#' get_endereco_profissional(html)
#' @export
get_endereco_profissional <- function(caminho_html, encoding = "ISO-8859-1") {
  doc <- .read_html_lattes(caminho_html, encoding)
  id_lattes <- .get_id_lattes(doc)

  campos <- c(
    instituicao = NA_character_, logradouro = NA_character_,
    complemento = NA_character_, cep = NA_character_,
    bairro = NA_character_, cidade = NA_character_,
    uf = NA_character_, pais = NA_character_,
    caixa_postal = NA_character_, telefone = NA_character_,
    ramal = NA_character_, fax = NA_character_,
    endereco_eletronico = NA_character_, homepage = NA_character_
  )
  monta <- function(campos) {
    tibble::as_tibble(as.list(campos)) |>
      tibble::add_column(id_lattes = id_lattes)
  }

  secao <- .secao_data_cell(doc, "Endereco")

  # Older layout: one labelled row per field ("Logradouro", "CEP", ...)
  rotulados <- c(
    logradouro = "Logradouro", complemento = "Complemento", cep = "CEP",
    bairro = "Bairro", cidade = "Cidade", uf = "UF", pais = "Pa\u00eds",
    caixa_postal = "Caixa Postal", telefone = "Telefone", ramal = "Ramal",
    fax = "Fax", endereco_eletronico = "Endere\u00e7o eletr\u00f4nico",
    homepage = "Homepage"
  )
  for (nm in names(rotulados)) {
    campos[[nm]] <- .nz(stringr::str_squish(.valor_rotulado(secao, rotulados[[nm]])))
  }

  # Current layout: a single free-text block, one item per line
  bloco <- .valor_rotulado(secao, "Endere\u00e7o Profissional")
  if (is.na(.nz(stringr::str_squish(bloco))) || !all(is.na(campos))) {
    return(monta(campos))
  }

  linhas <- stringr::str_squish(strsplit(bloco, "\n", fixed = TRUE)[[1]])
  linhas <- linhas[nzchar(linhas)]

  rotulos <- c(
    telefone = "^Telefone\\s*:", ramal = "^Ramal\\s*:", fax = "^Fax\\s*:",
    homepage = "^URL da Homepage\\s*:",
    endereco_eletronico = paste0("^(?:", .rotulo("Endere\u00e7o eletr\u00f4nico"), "|E-?mail)\\s*:")
  )
  livres <- character(0)
  for (ln in linhas) {
    hit <- names(rotulos)[vapply(rotulos, function(p)
      stringr::str_detect(ln, stringr::regex(p, ignore_case = TRUE)), logical(1))]
    if (length(hit) > 0) {
      campos[[hit[1]]] <- .nz(stringr::str_squish(sub("^[^:]*:\\s*", "", ln)))
    } else {
      livres <- c(livres, ln)
    }
  }

  # "62010560 - Sobral, CE - Brasil - Caixa-postal: 123" (CEP is optional)
  rx_local <- paste0(
    "^(?:([0-9][0-9.-]{4,9})\\s*-\\s*)?",
    "([^,]+),\\s*([A-Za-z]{2})\\s*-\\s*([^-]+?)",
    "(?:\\s*-\\s*Caixa-?\\s*postal\\s*:\\s*(.+))?$"
  )
  i_loc <- which(stringr::str_detect(livres, stringr::regex(rx_local, ignore_case = TRUE)))
  i_loc <- i_loc[i_loc > 1]
  if (length(i_loc) > 0) {
    i_loc <- i_loc[length(i_loc)]
    m <- stringr::str_match(livres[i_loc], stringr::regex(rx_local, ignore_case = TRUE))
    campos[["cep"]]          <- .nz(m[, 2])
    campos[["cidade"]]       <- .nz(stringr::str_squish(m[, 3]))
    campos[["uf"]]           <- toupper(m[, 4])
    campos[["pais"]]         <- .nz(stringr::str_squish(m[, 5]))
    campos[["caixa_postal"]] <- .nz(stringr::str_squish(m[, 6]))
    meio <- livres[seq_len(i_loc - 1)][-1]
  } else {
    meio <- livres[-1]
  }

  campos[["instituicao"]] <- .nz(stringr::str_remove(livres[1], "\\.$"))
  # Between institution and locality: street, [complement], neighbourhood
  if (length(meio) >= 1) campos[["logradouro"]] <- meio[1]
  if (length(meio) == 2) campos[["bairro"]] <- meio[2]
  if (length(meio) >= 3) {
    campos[["complemento"]] <- paste(meio[2:(length(meio) - 1)], collapse = "; ")
    campos[["bairro"]] <- meio[length(meio)]
  }

  monta(campos)
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
    le         = parse_nivel(profs, .rotulo("L\u00ea")),
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

  area_rx <- .rotulo("\u00e1rea")
  tibble::tibble(
    numero      = as.character(seq_along(txts)),
    grande_area = extr(txts, paste0("Grande ", area_rx)),
    area        = extr(txts, paste0("(?<![Gg]rande )(?<![Ss]ub)", area_rx)),
    subarea     = extr(txts, paste0("Sub", area_rx)),
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

  # Cells come in (cell-3, cell-9) pairs: ("1.", LINHA), then an optional
  # ("", "Objetivo: ...") that belongs to the line right before it.
  celulas <- xml2::xml_find_all(secao,
    "./div[contains(@class,'layout-cell-3') or contains(@class,'layout-cell-9')]")
  if (length(celulas) == 0) return(na_ret)
  eh9 <- stringr::str_detect(xml2::xml_attr(celulas, "class"), "layout-cell-9")
  # Only the first line of a cell: the objetivo cell continues with
  # "Grande área: ...", "Setores de atividade: ..." and "Palavras-chave: ..."
  txt <- rvest::html_text2(celulas) |> stringr::str_trim() |>
    stringr::str_extract("^[^\n]*") |> stringr::str_squish()
  txt[is.na(txt)] <- ""

  rx_obj <- stringr::regex("^Objetivo\\s*:\\s*", ignore_case = TRUE)
  numeros <- linhas <- objetivos <- character(0)
  rotulo <- NA_character_
  for (i in seq_along(celulas)) {
    if (!eh9[i]) {
      rotulo <- txt[i]
    } else if (stringr::str_detect(txt[i], rx_obj)) {
      if (length(linhas) > 0) {
        objetivos[length(linhas)] <- stringr::str_remove(txt[i], rx_obj)
      }
    } else if (nzchar(txt[i])) {
      linhas    <- c(linhas, txt[i])
      numeros   <- c(numeros, stringr::str_remove(rotulo %||% "", "\\.\\s*$"))
      objetivos <- c(objetivos, NA_character_)
    }
  }

  n <- length(linhas)
  if (n == 0) return(na_ret)
  numeros <- ifelse(nzchar(numeros), numeros, as.character(seq_len(n)))

  tibble::tibble(
    numero    = numeros,
    linha     = linhas,
    objetivo  = .nz(stringr::str_remove(stringr::str_squish(objetivos), "(?<=\\.)\\.+$")),
    id_lattes = rep(id_lattes, n)
  )
}
