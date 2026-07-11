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
* `get_atuacoes_profissionais()`: the redundant "Vínculo:" prefix is no longer
  kept in the `vinculo` column.
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
