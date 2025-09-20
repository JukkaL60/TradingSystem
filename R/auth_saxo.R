# auth_saxo.R — kääre, joka hoitaa autentikoinnin ja varmistaa tokenin

# Lataa OAuth-implementaatio — toimi sekä juuresta että R/-kansiosta ajettaessa
if (file.exists(file.path("R", "saxo_auth_live.R"))) {
  source(file.path("R", "saxo_auth_live.R"), chdir = FALSE)
} else if (file.exists("saxo_auth_live.R")) {
  source("saxo_auth_live.R", chdir = FALSE)
} else {
  stop("saxo_auth_live.R puuttuu. Sijoita se polkuun R/saxo_auth_live.R.")
}

ensure_saxo_auth <- function(){
  env <- Sys.getenv("TRADING_ENV", "sim")
  message("🔑 Auth ympäristö: ", env)
  
  tok <- try(saxo_auth_live(env = env, force = FALSE), silent = TRUE)
  if (inherits(tok, "try-error")) {
    message("⚠️ Tokenin haku epäonnistui: ", conditionMessage(attr(tok, "condition")))
    stop("Saxo-autentikointi epäonnistui.")
  }
  if (!exists("saxo_token")) stop("saxo_token ei asettunut globaaliin ympäristöön.")
  invisible(TRUE)
}

# Suorita heti importin yhteydessä
ensure_saxo_auth()
