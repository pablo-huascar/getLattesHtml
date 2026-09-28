# Internal helpers — not exported

`%||%` <- function(x, y) if (length(x) == 0 || all(is.na(x))) y else x

.nz <- function(x) {
  if (length(x) == 0) return(NA_character_)
  ifelse(is.na(x) | stringr::str_trim(x) == "", NA_character_, x)
}

# Lattes serves ISO-8859-1, but curricula re-saved by browsers or scrapers may
# already be UTF-8 (sometimes with accents replaced by U+FFFD). Reading such a
# file as ISO-8859-1 turns every accent into mojibake ("Ã§", "ï¿½"), so when
# the default encoding is requested and the bytes are valid UTF-8 with
# multibyte sequences, UTF-8 is used instead.
.read_html_lattes <- function(caminho, encoding = "ISO-8859-1") {
  if (identical(toupper(encoding), "ISO-8859-1") && is.character(caminho) &&
      length(caminho) == 1 && file.exists(caminho)) {
    bytes <- readBin(caminho, "raw", file.info(caminho)$size)
    txt <- rawToChar(bytes[bytes != as.raw(0)])
    if (any(bytes > as.raw(0x7f)) && validUTF8(txt)) encoding <- "UTF-8"
  }
  rvest::read_html(caminho, encoding = encoding)
}

# Turn a label written with accents ("Descrição") into a regex that also
# accepts the unaccented letter and U+FFFD, the replacement character left in
# curricula downloaded with a broken encoding ("Descri��o").
.rotulo <- function(x) {
  base <- c(
    "\u00e1" = "a", "\u00e0" = "a", "\u00e2" = "a", "\u00e3" = "a",
    "\u00e9" = "e", "\u00ea" = "e", "\u00ed" = "i", "\u00f3" = "o",
    "\u00f4" = "o", "\u00f5" = "o", "\u00fa" = "u", "\u00e7" = "c",
    "\u00c1" = "A", "\u00c9" = "E", "\u00cd" = "I", "\u00d3" = "O",
    "\u00da" = "U", "\u00c7" = "C"
  )
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  paste(vapply(chars, function(ch) {
    if (ch %in% names(base)) sprintf("[%s%s\ufffd]", ch, base[[ch]]) else ch
  }, character(1)), collapse = "")
}

# Value cell (layout-cell-9) paired with a bold label (layout-cell-3) inside a
# section, matched with .rotulo() so damaged accents still hit.
.valor_rotulado <- function(secao, label) {
  if (inherits(secao, "xml_missing") || length(secao) == 0) return(NA_character_)
  bs <- xml2::xml_find_all(secao,
    ".//div[contains(@class,'layout-cell-3')]//b")
  txt <- stringr::str_squish(rvest::html_text2(bs))
  hit <- which(stringr::str_detect(txt,
    stringr::regex(paste0("^", .rotulo(label), "$"), ignore_case = TRUE)))
  if (length(hit) == 0) return(NA_character_)
  node <- xml2::xml_find_first(bs[[hit[1]]], paste0(
    "ancestor::div[contains(@class,'layout-cell-3')]",
    "/following-sibling::div[contains(@class,'layout-cell-9')][1]",
    "//div[contains(@class,'layout-cell-pad-5')]"
  ))
  if (inherits(node, "xml_missing")) NA_character_ else rvest::html_text2(node)
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

# Walk the document in order and label every span.transform with the section
# anchor it falls under (the last named anchor before it, ignoring the PP_/LP_
# item anchors), the group header (div.inst_back, e.g. "Orientações e
# supervisões concluídas") and the subsection header (div.cita-artigos, e.g.
# "Teses de doutorado"). A section ends at the next anchor with a different
# name, so no stop-anchor list is needed; repeated anchors with the same name
# (LivrosCapitulos, TrabalhosPublicadosAnaisCongresso) stay in one section.
# In production sections the subsection header comes before its anchor, so the
# subsection is only reset by a new group header.
.itens_documento <- function(doc) {
  ns <- xml2::xml_find_all(doc, paste(
    "//a[@name and not(starts-with(@name,'PP_')) and not(starts-with(@name,'LP_'))]",
    "//div[contains(@class,'inst_back') or contains(@class,'cita-artigos')]",
    "//span[contains(@class,'transform')]",
    sep = " | "
  ))
  tag <- xml2::xml_name(ns)
  cls <- xml2::xml_attr(ns, "class")
  n <- length(ns)
  secao <- grupo <- subsecao <- rep(NA_character_, n)
  s <- g <- ss <- NA_character_
  for (i in seq_len(n)) {
    if (tag[i] == "a") {
      s <- xml2::xml_attr(ns[[i]], "name")
    } else if (tag[i] == "div") {
      h <- stringr::str_squish(rvest::html_text2(ns[[i]]))
      if (grepl("inst_back", cls[i], fixed = TRUE)) {
        g <- h
        ss <- NA_character_
      } else {
        ss <- h
      }
    } else {
      secao[i] <- s
      grupo[i] <- g
      subsecao[i] <- ss
    }
  }
  eh_item <- tag == "span"
  list(nos = ns[eh_item], secao = secao[eh_item],
       grupo = grupo[eh_item], subsecao = subsecao[eh_item])
}

# Items (span.transform nodes, their text and headers) of the given sections.
.itens_secao <- function(doc, anchors) {
  it <- .itens_documento(doc)
  keep <- which(it$secao %in% anchors)
  list(
    nos      = it$nos[keep],
    textos   = stringr::str_squish(rvest::html_text2(it$nos[keep])),
    secao    = it$secao[keep],
    grupo    = it$grupo[keep],
    subsecao = it$subsecao[keep]
  )
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
# in full ("PINHEIRO, Francisco Pablo Huascar Aragao. Titulo" or
# "LARANJEIRA, PIRES. Titulo"), the first period that follows a lowercase
# letter or a capitalised word of two or more letters.
.split_autores_titulo <- function(txt) {
  loc <- stringr::str_locate(
    txt,
    paste0(
      "\\s+\\.\\s+|(?<=\\.)\\.\\s+",
      "|(?<=[a-z\u00e1\u00e9\u00ed\u00f3\u00fa\u00e3\u00f5\u00e2\u00ea\u00f4\u00e0\u00e7])\\.\\s+(?![;,])",
      # a single author written in full capitals: "LARANJEIRA, PIRES. Titulo"
      "|(?<=\\p{Lu}{2})\\.\\s+(?![;,])"
    )
  )
  if (is.na(loc[1, 1])) {
    return(c(autores = NA_character_, resto = stringr::str_squish(txt)))
  }
  autores <- stringr::str_sub(txt, 1, loc[1, 1] - 1)
  resto   <- stringr::str_sub(txt, loc[1, 2] + 1)
  # Keep the period of a final initial ("FURTADO, L.A.R.")
  autores <- stringr::str_squish(stringr::str_remove(autores, "[;,\\s]+$"))
  if (!nzchar(autores)) autores <- NA_character_
  c(autores = autores, resto = stringr::str_squish(resto))
}

# Edition marker as Lattes prints it: "1. ed.", "1ed.", "1a ed." etc.; an
# edition left blank is printed as "-ed." (group 1 is then NA).
.ed_regex <- "(?:(\\d+)\\s*[a\u00aa\u00b0]?\\s*\\.?\\s*|(?<![\\w-])-\\s*)[Ee]d\\."

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
