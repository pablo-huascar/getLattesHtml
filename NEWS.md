# getLattesHtml 0.1.0

* First public release on GitHub.
* Fixed `get_capitulos_livros()` and `get_livros_publicados()`: the book title
  was being merged with the author names when authors had single-letter
  initials, and edition/publisher/organizer fields were missed for the `1ed.`
  format used in book chapters.
* 30 functions covering all major sections of the Lattes HTML curriculum:
  personal data, education, professional activities, publications (articles,
  books, book chapters), conference papers, advisorships, examination boards,
  technical production, patents, and research projects.
* All functions accept a file path and return a tibble, enabling batch
  processing via `purrr::map()` and `purrr::list_rbind()`.
