# CRAN submission comments — getLattesHtml 0.1.0

## Test environments

* macOS aarch64 (local): R 4.5.2
* win-builder (R-devel)
* rhub: ubuntu-latest (R-release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes

* This is a first submission.
* All examples use `system.file("extdata", "exemplo.html", package = "getLattesHtml")`
  so they run without external downloads.
* The package parses HTML files downloaded manually from <http://lattes.cnpq.br/>.
  No internet access is required at run time.
