# getLattesHtml 0.2.0

All functions were checked against a set of real Lattes curricula; the fixes
below come from that review.

## Breaking changes

* `get_patentes()` now reads only the "Patentes e registros" sections and
  reports the kind of patent in `tipo` ("Privilégio de Inovação", "Modelo de
  Utilidade"...). "Programas de computador sem registro" and "Processos ou
  técnicas", which it used to return, are technical production in Lattes and
  are now returned by `get_producao_tecnica()`.
* `get_eventos_congressos()` gains an `evento` column (the event name);
  `titulo` is now the title of the work presented, NA when there is none.
* New columns: `autores` in `get_artigos_publicados()` and
  `get_artigos_aceitos()`; `instituicao` and `homepage` in
  `get_endereco_profissional()`.

## Bug fixes

* Section items are now delimited by the section anchor they fall under and
  classified by the Lattes subsection headers ("Teses de doutorado",
  "Supervisão de pós-doutorado", "Resumos expandidos...") instead of lists of
  stop anchors. This stops leaks between sections (e.g. "Prefácio/Posfácio"
  in `get_trabalhos_em_eventos()`, events in `get_trabalhos_anais_congresso()`
  for curricula without an "Apresentações" section).
* `get_endereco_profissional()` returned only NA: it now parses the free-text
  address block used by Lattes (the labelled layout is still supported).
* `get_participacao_projeto()`: `periodo` was always NA, and fields of a
  project without "Descrição" leaked from the next project.
* `get_artigos_publicados()`: `numero` and `pagina_final` are read from the
  citation (the `cvuri` attribute never carries them); articles repeated in
  "Educação e Popularização de C&T" are no longer duplicated.
* `get_trabalhos_anais_congresso()`, `get_trabalhos_em_eventos()` and
  `get_organizacao_eventos()`: authors and titles were cut at the first
  initial; `evento` is now filled for proceedings and `tipo` tells complete
  papers, expanded abstracts and abstracts apart.
* `get_producao_tecnica()` and `get_outras_producoes_tecnicas()`: single
  authors are recognised; `tipo` falls back to the subsection name.
* Formação: titles no longer carry the hidden Oasisbr tooltip text; the
  undergraduate title is no longer cut at the first comma; a sandwich-period
  advisor is no longer taken as the main one; the post-doc `area` column
  returned "ado".
* `get_orientacoes_pos_doutorado()` parses the supervision format
  ("Nome. ANO. Instituição, ..."); nested parentheses in the course no longer
  drop `curso`/`instituicao` in the other orientation functions. Banca and
  orientation levels now follow the subsection headers.
* `get_linha_pesquisa()`: objetivos are attached to their own line and no
  longer include keywords/areas.
* `get_atuacoes_profissionais()` fills `outras_informacoes` from the
  "Outras informações" row.
* Book chapters with a blank edition ("-ed.Curitiba") no longer put the
  edition marker in `cidade`; a single author written in capitals
  ("LARANJEIRA, PIRES. Título") is no longer merged with the title.
* Curricula saved as UTF-8 are detected automatically, and labels tolerate
  accents lost as U+FFFD in damaged downloads.

# getLattesHtml 0.1.0

* First public release on GitHub.
* Fixed `get_capitulos_livros()` and `get_livros_publicados()`: the book title
  was being merged with the author names when authors had single-letter
  initials, and edition/publisher/organizer fields were missed for the `1ed.`
  format used in book chapters.
* Fixed `get_orientacoes_doutorado()`, `get_orientacoes_mestrado()`, and
  `get_orientacoes_pos_doutorado()`: completed advisorships were dropped (and
  in-progress ones sometimes missed) because the section-boundary scan treated
  earlier sections (Bancas, Eventos) as stop points. The boundary is now
  resolved positionally from the section start.
* Implemented `get_artigos_aceitos()`, which was a stub that always returned
  NA. It now parses the "Artigos aceitos para publicação" section, including
  the ISSN carried by the JCR image attribute.
* `get_artigos_publicados()`: when the structured `cvuri` attribute carries an
  empty `titulo`/`nomePeriodico` (seen in older curricula), the title and
  journal are now recovered from the visible citation text instead of
  returning NA.
* `get_atuacoes_profissionais()`: the redundant "Vínculo:" prefix is no longer
  kept in the `vinculo` column, and the labelled fields of the vínculo cell
  are now split into their own columns (`atividade`,
  `enquadramento_funcional`, `carga_horaria`, `regime`); any other labelled
  field is collected into `outras_informacoes`. Label matching is
  case-insensitive and tolerates curricula downloaded with damaged accents
  (U+FFFD), and fields left empty in the source ("Vínculo: ,") come back as NA
  instead of leftover punctuation.
* Fixed the `get_bancas_*()` family: tipo/programa/instituição are now parsed
  from the canonical "ANO. TIPO (PROGRAMA) - INSTITUIÇÃO" tail instead of
  keyword scans that could match words inside the title (e.g. "trabalhadores
  (as)"). Board membership is classified by the program of that tail, so
  qualifying exams count with their level (doutorado/mestrado) and
  "Pós-Graduação" no longer leaks doctoral boards into
  `get_bancas_graduacao()`.
* 30 functions covering all major sections of the Lattes HTML curriculum:
  personal data, education, professional activities, publications (articles,
  books, book chapters), conference papers, advisorships, examination boards,
  technical production, patents, and research projects.
* All functions accept a file path and return a tibble, enabling batch
  processing via `purrr::map()` and `purrr::list_rbind()`.
