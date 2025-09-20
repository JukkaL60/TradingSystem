# =========================
# Saxo OpenAPI – Live/Sim (R)
# OAuth2 (Authorization Code) + perus-endpointit
# =========================

# Paketit
library(httr2)
library(jsonlite)

# -------- Asetukset ympäristömuuttujista --------
SAXO_ENV          <- tolower(Sys.getenv("SAXO_ENV", "sim"))  # "sim" tai "live"
SAXO_APP_KEY      <- Sys.getenv("SAXO_APP_KEY")
SAXO_APP_SECRET   <- Sys.getenv("SAXO_APP_SECRET")
SAXO_REDIRECT_URI <- Sys.getenv("SAXO_REDIRECT_URI")

if (SAXO_ENV == "sim") {
  SAXO_AUTH_URL  <- "https://sim.logonvalidation.net/authorize"
  SAXO_TOKEN_URL <- "https://sim.logonvalidation.net/token"
  SAXO_BASE_URL  <- "https://gateway.saxobank.com/sim/openapi/"
} else {
  SAXO_AUTH_URL  <- "https://live.logonvalidation.net/authorize"
  SAXO_TOKEN_URL <- "https://live.logonvalidation.net/token"
  SAXO_BASE_URL  <- "https://gateway.saxobank.com/openapi/"
}

# Tallennuspolku tokeneille (eri tiedosto sim/live)
.saxo_token_path <- file.path(tempdir(), paste0("saxo_token_", SAXO_ENV, ".rds"))

# ---------------- OAuth & Token-hallinta ----------------

.saxo_save_token <- function(tok) {
  saveRDS(tok, .saxo_token_path)
  invisible(tok)
}

.saxo_load_token <- function() {
  if (file.exists(.saxo_token_path)) {
    readRDS(.saxo_token_path)
  } else {
    NULL
  }
}

.saxo_refresh_token <- function(tok) {
  if (is.null(tok$refresh_token)) stop("Ei refresh_tokenia – aja saxo_auth_live() uudelleen.")
  req <- request(SAXO_TOKEN_URL) |>
    req_body_form(
      grant_type    = "refresh_token",
      refresh_token = tok$refresh_token,
      client_id     = SAXO_APP_KEY,
      client_secret = SAXO_APP_SECRET,
      redirect_uri  = SAXO_REDIRECT_URI
    ) |>
    req_user_agent("SaxoOpenAPI-R/1.0") |>
    req_perform()
  
  new_tok <- resp_body_json(req, simplifyVector = TRUE)
  if (is.null(new_tok$refresh_token)) new_tok$refresh_token <- tok$refresh_token
  .saxo_save_token(new_tok)
}

# ---------------- Julkiset funktiot ----------------

# 1) Auth – Authorization Code (ilman PKCE:tä, koska käytössä on App Secret)
saxo_auth_live <- function() {
  if (!nzchar(SAXO_APP_KEY) || !nzchar(SAXO_APP_SECRET) || !nzchar(SAXO_REDIRECT_URI)) {
    stop(sprintf(
      "Puuttuva asetus (.Renviron): KEY:%s SECRET:%s REDIRECT:%s",
      nzchar(SAXO_APP_KEY), nzchar(SAXO_APP_SECRET), nzchar(SAXO_REDIRECT_URI)
    ))
  }
  message("Auth with redirect_uri = ", SAXO_REDIRECT_URI, " (env: ", SAXO_ENV, ")")
  
  client <- oauth_client(
    id        = SAXO_APP_KEY,
    token_url = SAXO_TOKEN_URL,
    secret    = SAXO_APP_SECRET   # tärkeä Grant Type = Code -flow’ssa
  )
  
  # httr2 1.2.1: selain avataan automaattisesti; ei open_browser-paramia
  tok <- oauth_flow_auth_code(
    client       = client,
    auth_url     = SAXO_AUTH_URL,
    redirect_uri = SAXO_REDIRECT_URI,
    scope        = NULL,    # Saxo ei käytä scopea
    pkce         = FALSE    # pois, koska appissa Grant Type = Code (ei PKCE)
  )
  
  .saxo_save_token(tok)
  message("Saxo auth OK. Token talletettu: ", .saxo_token_path)
  invisible(tok)
}

# Hakee voimassa olevan access_tokenin (uusii tarvittaessa 401:n jälkeen)
.saxo_access_token <- function() {
  tok <- .saxo_load_token()
  if (is.null(tok)) stop("Token puuttuu. Aja saxo_auth_live().")
  tok$access_token
}

# Sisäinen: tee valmiiksi autentikoitu pyyntö
.saxo_req <- function(path) {
  request(paste0(SAXO_BASE_URL, path)) |>
    req_auth_bearer_token(.saxo_access_token()) |>
    req_user_agent("SaxoOpenAPI-R/1.0")
}

# Yleis-kutsufunktio, joka huolehtii 401 -> refresh -> retry
.saxo_perform <- function(req) {
  resp <- req_perform(req)
  if (resp_status(resp) == 401) {
    tok <- .saxo_load_token()
    .saxo_refresh_token(tok)
    req2 <- req |> req_auth_bearer_token(.saxo_access_token())
    resp <- req_perform(req2)
  }
  if (resp_status(resp) >= 300) {
    stop("Saxo API error: ", resp_status_desc(resp), " (", resp_status(resp), ")")
  }
  resp
}

# 2) Pikatesti (sanity check)
saxo_ping <- function() {
  resp <- .saxo_perform(.saxo_req("trade/v2/sessions/capabilities"))
  list(status = resp_status(resp),
       head   = utils::head(resp_body_json(resp, simplifyVector = TRUE)))
}

# 3) Tilit & avaimet
saxo_get_accounts <- function() {
  resp <- .saxo_perform(.saxo_req("port/v1/accounts/me"))
  resp_body_json(resp, simplifyVector = TRUE)
}

# 4) Omat positiot
saxo_get_positions <- function() {
  resp <- .saxo_perform(.saxo_req("port/v1/positions/me"))
  resp_body_json(resp, simplifyVector = TRUE)
}

# 5) Instrumentin haku (UIC + AssetType)
#    Keywords, AssetTypes = esim. "Stock","Bond","FxSpot","CfdOnIndex","Index" jne.
saxo_find_uic <- function(keywords, asset_types = NULL, top = 10) {
  req <- .saxo_req("ref/v1/instruments/") |>
    req_url_query(Keywords = keywords, `$top` = top)
  if (!is.null(asset_types)) req <- req |> req_url_query(AssetTypes = asset_types)
  resp <- .saxo_perform(req)
  out  <- resp_body_json(resp, simplifyVector = TRUE)
  out$Data
}

# 6) Historialliset hinnat (chart/v1/charts)
#    Horizon: esim. 1,5,10,60,1440 (minuuttijänteet; 1440 = 1D)
#    TimeRange esim. "Year","Quarter","Month" TAI From/To (ISO8601)
saxo_get_history <- function(uic, asset_type, horizon = 1440, timerange = "Year",
                             from = NULL, to = NULL) {
  req <- .saxo_req("chart/v1/charts") |>
    req_url_query(
      Uic       = uic,
      AssetType = asset_type,
      Horizon   = horizon
    )
  if (!is.null(from) && !is.null(to)) {
    req <- req |> req_url_query(From = from, To = to)
  } else if (!is.null(timerange)) {
    req <- req |> req_url_query(TimeRange = timerange)
  }
  resp <- .saxo_perform(req)
  dat  <- resp_body_json(resp, simplifyVector = TRUE)
  dat$Data
}

# ---------------- Käyttöesimerkki ----------------
# 1) Kirjaudu sisään (avaa selaimen authia varten):
#    saxo_auth_live()
#
# 2) Pikatesti:
#    saxo_ping()
#
# 3) Tilit:
#    acc <- saxo_get_accounts(); str(acc, max.level = 1)
#
# 4) Positiot:
#    pos <- saxo_get_positions(); str(pos, max.level = 1)
#
# 5) UIC-haku (esim. EuroStoxx 50 Index "SX5E"):
#    ins <- saxo_find_uic("SX5E", asset_types = "Index", top = 5); View(ins)
#    uic <- ins$Identifier[1]; asset <- ins$AssetType[1]
#
# 6) Historia 1D:
#    h <- saxo_get_history(uic, asset_type = asset, horizon = 1440, timerange = "Year"); head(h)
