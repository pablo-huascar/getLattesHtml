# getLattesHtml

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/pablo-huascar/getLattesHtml/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pablo-huascar/getLattesHtml/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Extrai dados estruturados de currículos da Plataforma
[Lattes](http://lattes.cnpq.br/) (CNPq) a partir de arquivos **HTML** baixados
manualmente. Cada função lê um arquivo e devolve um `tibble` organizado, pronto
para processamento em lote com `purrr::map()` e `purrr::list_rbind()`.

> ⚠️ **Versão preliminar.** O pacote está em desenvolvimento ativo e a extração
> se apoia em heurísticas sobre o HTML do Lattes, que não é um formato estável.
> Resultados podem variar entre currículos; relate problemas em
> [issues](https://github.com/pablo-huascar/getLattesHtml/issues).

## Instalação

O pacote está disponível apenas no GitHub. Instale com um dos comandos abaixo:

``` r
# com o pacote remotes
# install.packages("remotes")
remotes::install_github("pablo-huascar/getLattesHtml")

# ou com o pacote pak
# install.packages("pak")
pak::pak("pablo-huascar/getLattesHtml")
```

## Como obter o HTML do currículo

1. Abra o currículo desejado na Plataforma Lattes.
2. No navegador, salve a página como HTML (`Arquivo > Salvar como > Página da Web`).
3. Aponte as funções do pacote para o arquivo salvo.

## Uso

``` r
library(getLattesHtml)

# arquivo de exemplo que acompanha o pacote
html <- system.file("extdata", "exemplo.html", package = "getLattesHtml")

get_dados_gerais(html)
get_formacao_doutorado(html)
get_artigos_publicados(html)
```

Todas as funções retornam um `tibble` com a coluna `id_lattes`, o que facilita
processar vários currículos de uma vez e juntar os resultados:

``` r
library(purrr)

arquivos <- list.files("curriculos", pattern = "\\.html$", full.names = TRUE)

artigos <- map(arquivos, get_artigos_publicados) |> list_rbind()
```

## Funções disponíveis

| Seção | Funções |
|---|---|
| Identificação e dados pessoais | `get_id()`, `get_dados_gerais()`, `get_endereco_profissional()`, `get_idiomas()` |
| Áreas e linhas de pesquisa | `get_areas_atuacao()`, `get_linha_pesquisa()` |
| Formação | `get_formacao_graduacao()`, `get_formacao_mestrado()`, `get_formacao_doutorado()`, `get_formacao_pos_doutorado()`, `get_formacao_complementar()` |
| Atuação profissional | `get_atuacoes_profissionais()` |
| Publicações | `get_artigos_publicados()`, `get_artigos_aceitos()`, `get_livros_publicados()`, `get_capitulos_livros()`, `get_trabalhos_anais_congresso()` |
| Eventos | `get_trabalhos_em_eventos()`, `get_eventos_congressos()`, `get_organizacao_eventos()` |
| Orientações | `get_orientacoes_mestrado()`, `get_orientacoes_doutorado()`, `get_orientacoes_pos_doutorado()` |
| Bancas | `get_bancas_graduacao()`, `get_bancas_mestrado()`, `get_bancas_doutorado()` |
| Produção técnica | `get_producao_tecnica()`, `get_outras_producoes_tecnicas()`, `get_patentes()` |
| Projetos | `get_participacao_projeto()` |

## Licença

MIT © Francisco Pablo Huascar Aragão Pinheiro. Veja o arquivo
[LICENSE.md](LICENSE.md).
