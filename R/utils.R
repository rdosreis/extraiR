library(httr)
library(readr)

# httr::set_config(config(ssl_verifypeer = 0L))

# cria_estrutura <- function(set.wd = "C:/", acronym, ...){
#   mainDir <- Sys.Date()
#   subDir <- toupper(acronym)
#   direxists <- dir.exists(paths = file.path(set.wd, mainDir))
#   if(direxists){
#     dir.create(file.path(set.wd, mainDir, subDir))
#   }else{
#     dir.create(file.path(set.wd, mainDir, subDir), showWarnings = FALSE)
#   }
# }

#' Extrai número de versões
#'
#' @description
#' A função extrai_versao extrai o número de versões (número total)
#'   e os números correspondentes das versões de uma atividade
#'   através do api do Otus ELSA, retornando uma lista.
#'
#' @param project O nome do projeto no Otus.
#' @param url O nome da url de extração de dados para a atividade desejada.
#'   A url default utilizada é
#'   "https://api-otus.elsa.ufrgs.br/enterprises/data-extraction/activity"
#' @param acronym O acrônimo da atividade para a qual se deseja extrair os dados.
#' @param token character o token do usuário.
#'
#' @example
#' # Not run!
#' # antc_versoes <- extrai_versao(acronym = "ANTC",
#' #                                token = "")
#'
extrai_versao <- function(
                          project = "elsa",
                          url = "https://api-otus.elsa.ufrgs.br/enterprises/data-extraction/activity",
                          acronym, token, ...) {
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  url <- paste(url, acronym, "versions", sep = "/")
  get <- GET(
    url = url,
    config = list(
      content_type = "text/csv;charset=UTF-8"
      ),
      add_headers(Authorization = token, Origin = project, Connection = "keep-alive")
  )
  retrieve_content <- content(
    x = get, as = "text",
    encoding = "UTF-8"
  )
  aux <- stringr::str_extract_all(retrieve_content, "\\[[^()]+\\]")[[1]]
  aux <- substring(aux, 2, nchar(aux) - 1)
  versoes <- sort(as.numeric(strsplit(aux, ",")[[1]]))
  total <- length(versoes)
  ultima_versao <- versoes[total]
  versoes_list <- list(versoes = versoes, total = total, ultima_versao = ultima_versao)
  versoes_list
}

#' Extrai uma atividade
#'
#' @description
#' A função extrai_atividade extrai uma versão de um atividade
#'   através do api do Otus ELSA, retornando um data frame/tibble.
#'
#' @param project O nome do projeto no Otus.
#' @param url O nome da url de extração de dados para a atividade desejada.
#'   A url default utilizada é
#'   "https://api-otus.elsa.ufrgs.br/enterprises/data-extraction/activity"
#' @param acronym O acrônimo da atividade para a qual se deseja extrair os dados.
#' @param version O número da versão da atividade para a qual se deseja extrair os dados.
#' @param token character o token do usuário.
#'
#' @example
#' # Not run!
#' # antc_v1 <- extrai_atividade(acronym = "ANTC",
#' #                                version = 1,
#' #                                token = "")
#'
extrai_atividade <- function(
    project = "elsa",
    url = "https://api-otus.elsa.ufrgs.br/enterprises/data-extraction/activity",
    acronym, version, token, ...) {
  httr::set_config(httr::config(ssl_verifypeer = 0L))
  url <- paste(url, acronym, version, sep = "/")
  get <- GET(
    url = url,
    config = list(
      content_type = "text/csv;charset=UTF-8"
    ),
    add_headers(Authorization = token, Origin = project, Connection = "keep-alive")
  )
  if (get$status_code != 500){
    retrieve_content <- content(
      x = get, as = "text",
      encoding = "UTF-8"
    )
    atividade_df <- read_delim(
      file = retrieve_content, delim = ";",
      escape_double = FALSE, col_names = TRUE,
      show_col_types = FALSE
    )
    atividade_df
  }
}

#' Extrai as versões de uma atividade
#'
#' @description
#' A função extrai_atividade_lista extrai todas as versões
#'   correspondentes de uma atividade
#'   através do api do Otus ELSA, retornando uma lista de data frames.
#'
#' @param project O nome do projeto no Otus.
#' @param url O nome da url de extração de dados para a atividade desejada.
#'   A url default utilizada é
#'   "https://api-otus.elsa.ufrgs.br/enterprises/data-extraction/activity"
#' @param acronym O acrônimo da atividade para a qual se deseja extrair os dados.
#' @param token character o token do usuário.
#'
#' @example
#' # Not run!
#' # antc_lista_dfs <- extrai_atividade_lista(acronym = "ANTC",
#' #                                          token = "")
#'
extrai_atividade_lista <- function(
    project = "elsa",
    url = "https://api-otus.elsa.ufrgs.br/enterprises/data-extraction/activity",
    acronym, token, ...) {
  atividade_versoes <- extrai_versao(acronym = acronym, token = token)
  extrai_atividade_aux <- function(x) {
    extrai_atividade(
      acronym = acronym,
      version = x,
      token = token
    )
  }
  atividade_lista_dfs <- lapply(X = atividade_versoes$versoes, FUN = extrai_atividade_aux)
  atividade_lista_dfs
}



