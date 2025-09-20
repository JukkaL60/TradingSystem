# http_helpers.R — yhtenäiset API-kutsut

build_req <- function(path){
  if (!exists(".SAXO_BASE")) stop(".SAXO_BASE ei asetettu (env_setup.R)")
  if (!exists("saxo_token") || is.null(saxo_token$access_token)) stop("saxo_token puuttuu — varmista auth_saxo.R")
  url <- paste0(.SAXO_BASE, path)
  httr2::request(url) |> httr2::req_auth_bearer_token(saxo_token$access_token)
}

perform_req <- function(req){
  req |> httr2::req_timeout(60) |> httr2::req_perform()
}
