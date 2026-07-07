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

# Strict between-anchors: spans after pre_anchors but before any pos_anchor.
# Uses not(preceding::a[stop]) — correct for OR-list stops.
# Contrast: following::a[A or B] is WRONG because a span past stop A
# still satisfies following::a[B] if B appears later.
.transforms_entre_strict <- function(doc, pre_anchors, pos_anchors) {
  pre_sel <- paste(sprintf("@name='%s'", pre_anchors), collapse = " or ")
  pos_sel <- paste(sprintf("@name='%s'", pos_anchors), collapse = " or ")
  xp <- sprintf(
    "//span[contains(@class,'transform')][preceding::a[%s] and not(preceding::a[%s])]",
    pre_sel, pos_sel
  )
  doc |> rvest::html_elements(xpath = xp) |> rvest::html_text2() |> unique()
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
