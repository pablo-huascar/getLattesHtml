# getLattesHtml 0.1.0

* Initial CRAN release.
* 30 functions covering all major sections of the Lattes HTML curriculum:
  personal data, education, professional activities, publications (articles,
  books, book chapters), conference papers, advisorships, examination boards,
  technical production, patents, and research projects.
* All functions accept a file path and return a tibble, enabling batch
  processing via `purrr::map()` and `purrr::list_rbind()`.
