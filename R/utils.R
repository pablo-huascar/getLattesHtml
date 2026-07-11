# Internal helpers — not exported

`%||%` <- function(x, y) if (length(x) == 0 || all(is.na(x))) y else x

.nz <- function(x) {
  if (length(x) == 0) return(NA_character_)
  ifelse(is.na(x) | stringr::str_trim(x) == "", NA_character_, x)
}

.read_html_lattes <- function(caminho, encoding = "ISO-8859-1") {
  rvest::read_html(caminho, encoding = encoding)
}

.get_id_lattes <- function(doc) {
  safe_txt <- function(n) {
    if (inherits(n, "xml_missing")) NA_character_ else rvest::html_text2(n)
  }

  id <- xml2::xml_find_first(
    doc,
    "//ul[contains(@class,'informacoes-autor')]//li[contains(., 'ID Lattes')]"
  ) |> safe_txt() |> stringr::str_extract("\\b\\d{16}\\b")

  if (is.na(id) || !nzchar(id %||% "")) {
    id <- xml2::xml_find_first(
      doc,
      "//ul[contains(@class,'informacoes-autor')]//li[contains(., 'lattes.cnpq.br')]"
    ) |> safe_txt() |> stringr::str_extract("\\b\\d{16}\\b")
  }

  if (is.na(id) || !nzchar(id %||% "")) {
    id <- doc |>
      rvest::html_elements("ul.informacoes-autor li") |>
      rvest::html_text2() |>
      stringr::str_extract("\\b\\d{16}\\b") |>
      (\(v) v[!is.na(v)][1])()
  }

  id %||% NA_character_
}

# Get span.transform texts after named anchor(s), up to optional stop anchor(s)
.transforms_entre <- function(doc, pre_anchors, pos_anchors = NULL) {
  if (!is.null(pos_anchors)) {
    textos <- .transforms_entre_strict(doc, pre_anchors, pos_anchors)
    if (length(textos) > 0) return(textos)
  }
  .transforms_apos(doc, pre_anchors)
}

.transforms_apos <- function(doc, anchors) {
  textos <- character(0)
  for (nm in anchors) {
    node <- doc |> rvest::html_element(xpath = sprintf("//a[@name='%s']", nm))
    if (!inherits(node, "xml_missing")) {
      seg <- node |>
        rvest::html_elements(xpath = "following::span[contains(@class,'transform')]") |>
        rvest::html_text2()
      textos <- c(textos, seg)
    }
  }
  unique(textos)
}

# Get span.transform texts between cita-artigos subsection headers
.transforms_por_cita <- function(doc, cita_pre, cita_pos = NULL) {
  if (!is.null(cita_pos)) {
    xp <- sprintf(paste0(
      "//span[contains(@class,'transform')]",
      "[preceding::div[contains(@class,'cita-artigos')][b[contains(.,'%s')]]]",
      "[following::div[contains(@class,'cita-artigos')][b[contains(.,'%s')]]]"
    ), cita_pre, cita_pos)
  } else {
    xp <- sprintf(paste0(
      "//span[contains(@class,'transform')]",
      "[preceding::div[contains(@class,'cita-artigos')][b[contains(.,'%s')]]]"
    ), cita_pre)
  }
  doc |> rvest::html_elements(xpath = xp) |> rvest::html_text2() |> unique()
}

# Get the data-cell div after a named anchor
.secao_data_cell <- function(doc, anchor_name) {
  doc |> rvest::html_element(
    xpath = sprintf(
      "//a[@name='%s']/following-sibling::div[contains(@class,'data-cell')]",
      anchor_name
    )
  )
}

# Get span.transform texts within the data-cell of the first matching section anchor
.transforms_na_secao <- function(doc, anchors) {
  for (nm in anchors) {
    secao <- .secao_data_cell(doc, nm)
    if (!inherits(secao, "xml_missing")) {
      txts <- secao |> rvest::html_elements("span.transform") |> rvest::html_text2()
      if (length(txts) > 0) return(unique(txts))
    }
  }
  character(0)
}

# Strict between-anchors: spans after the first pre_anchor and before the
# first pos_anchor that occurs AFTER it. A pos_anchor placed earlier in the
# document (e.g. "Bancas" before "Orientacoesconcluidas") must not stop the
# scan, so the stop node is resolved from the pre node's following axis.
.transforms_entre_strict <- function(doc, pre_anchors, pos_anchors) {
  .spans_entre_strict(doc, pre_anchors, pos_anchors) |>
    rvest::html_text2() |>
    unique()
}

# Node-level variant: returns the span nodes themselves, for callers that need
# attributes (e.g. data-issn) besides the text.
.spans_entre_strict <- function(doc, pre_anchors, pos_anchors) {
  pre_sel <- paste(sprintf("@name='%s'", pre_anchors), collapse = " or ")
  pre_node <- xml2::xml_find_first(doc, sprintf("//a[%s]", pre_sel))
  if (inherits(pre_node, "xml_missing")) return(xml2::xml_find_all(doc, "//nada"))

  spans <- xml2::xml_find_all(
    pre_node, "following::span[contains(@class,'transform')]"
  )
  if (length(spans) == 0) return(spans)

  pos_sel <- paste(sprintf("@name='%s'", pos_anchors), collapse = " or ")
  stop_node <- xml2::xml_find_first(pre_node, sprintf("following::a[%s]", pos_sel))
  if (!inherits(stop_node, "xml_missing")) {
    depois <- xml2::xml_find_all(
      stop_node, "following::span[contains(@class,'transform')]"
    )
    spans <- spans[!(xml2::xml_path(spans) %in% xml2::xml_path(depois))]
  }

  spans
}

# Parse cvuri URL-encoded query string from Lattes article spans
.parse_cvuri <- function(qs) {
  pairs <- strsplit(qs, "&(?=[a-zA-Z])", perl = TRUE)[[1]]
  result <- list()
  for (p in pairs) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    if (length(kv) >= 1 && nzchar(kv[[1]])) {
      val <- if (length(kv) >= 2) paste(kv[-1], collapse = "=") else ""
      # In query strings "+" encodes a space; URLdecode does not handle it
      val <- gsub("+", " ", val, fixed = TRUE)
      result[[kv[[1]]]] <- tryCatch(
        utils::URLdecode(val),
        error = function(e) val
      )
    }
  }
  result
}

.cvuri_field <- function(qs_list, ...) {
  keys <- c(...)
  for (k in keys) {
    val <- qs_list[[k]]
    if (!is.null(val) && nzchar(val)) return(val)
  }
  NA_character_
}

.parse_ano <- function(txt) {
  stringr::str_extract(txt, "\\b(1[89]|20)\\d{2}\\b")
}

# Split "AUTORES . Titulo ..." at the boundary between the author block and
# what follows. The boundary is the first standalone " . " (Lattes closes the
# author list with a spaced period), a double period left when the last author
# ends in an initial ("NAKANO, T. C.. Titulo"), or, for a single author written
# in full ("PINHEIRO, Francisco Pablo Huascar Aragao. Titulo"), the first
# period that follows a lowercase letter.
.split_autores_titulo <- function(txt) {
  loc <- stringr::str_locate(
    txt,
    "\\s+\\.\\s+|\\.\\.\\s+|(?<=[a-z\u00e1\u00e9\u00ed\u00f3\u00fa\u00e3\u00f5\u00e2\u00ea\u00f4\u00e0\u00e7])\\.\\s+"
  )
  if (is.na(loc[1, 1])) {
    return(c(autores = NA_character_, resto = stringr::str_squish(txt)))
  }
  autores <- stringr::str_sub(txt, 1, loc[1, 1] - 1)
  resto   <- stringr::str_sub(txt, loc[1, 2] + 1)
  autores <- stringr::str_squish(stringr::str_remove(autores, "[.;\\s]+$"))
  if (!nzchar(autores)) autores <- NA_character_
  c(autores = autores, resto = stringr::str_squish(resto))
}

# Edition marker as Lattes prints it: "1. ed.", "1ed.", "1a ed." etc.
.ed_regex <- "(\\d+)\\s*[a\u00aa\u00b0]?\\s*\\.?\\s*[Ee]d\\."

# Parse "CIDADE: EDITORA, ANO" (any part may be empty)
.parse_pub <- function(txt) {
  m <- stringr::str_match(txt, "^\\s*[.,]?\\s*([^:,]*?)\\s*:\\s*([^,]*?)\\s*,\\s*((?:1[89]|20)\\d{2})")
  cidade  <- if (!is.na(m[, 2]) && nzchar(m[, 2])) stringr::str_squish(m[, 2]) else NA_character_
  editora <- if (!is.na(m[, 3]) && nzchar(m[, 3])) stringr::str_squish(m[, 3]) else NA_character_
  ano     <- if (!is.na(m[, 4])) m[, 4] else NA_character_
  c(cidade = cidade, editora = editora, ano = ano)
}

.fix_len <- function(v, n) {
  length(v) <- n
  unname(v)
}

# Extract autores: text before the title (heuristic: autores are UPPERCASE)
.parse_autores_transform <- function(txt) {
  # Authors end at the first ". " followed by a mixed-case word (title start)
  m <- stringr::str_match(txt, "^((?:[A-Z][^.]+\\.\\s*)+?(?:\\([^)]+\\)\\.?\\s*)*)(?=[A-Z][a-z])")
  if (!is.na(m[, 1])) {
    return(stringr::str_squish(m[, 2]))
  }
  # Fallback: everything before the first standalone sentence fragment
  stringr::str_squish(stringr::str_extract(txt, "^[A-Z][^.]{2,100}\\."))
}
