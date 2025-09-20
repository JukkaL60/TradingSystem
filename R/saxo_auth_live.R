# ─────────────────────────────────────────────────────────────────────────────
# saxo_auth_live.R — Saxo OpenAPI OAuth2 (PKCE), SIM/LIVE tuki
# Vaatii .Renviron:
#   TRADING_ENV=sim|live
#   SAXO_CLIENT_ID=...
#   SAXO_CLIENT_SECRET=...
#   SAXO_REDIRECT_URI=http://localhost:1410/saxoapp
# ─────────────────────────────────────────────────────────────────────────────

.saxo_log <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")

# Endpointit (kuvasi mukaan /authorize ja /token)
.saxo_endpoints <- function(env = c("sim","live")) {
  env <- match.arg(env)
  base <- if (env == "sim") "https://sim.logonvalidation.net" else "https://www.logonvalidation.net"
  list(
    authorize = paste0(base, "/authorize"),
    token     = paste0(base, "/token"),
    base      = base
  )
}

.saxo_cache_path <- function(env) {
  dir.create(".auth", showWarnings = FALSE)
  file.path(".auth", sprintf("saxo_token_%s.rds", env))
}

.saxo_token_valid <- function(tok) {
  if (is.null(tok)) return(FALSE)
  exp <- tryCatch(tok$expires_at, error = function(e) NA)
  if (is.na(exp)) return(TRUE) # jos ei tietoa, yritetään käyttää
  as.numeric(difftime(exp, Sys.time(), units = "secs")) > 60
}

.saxo_try_refresh <- function(tok, client, token_url) {
  if (is.null(tok$refresh_token) || is.na(tok$refresh_token) || tok$refresh_token == "") return(NULL)
  .saxo_log("🔄 Yritetään refresh-tokenia...")
  out <- try(httr2::oauth_refresh(token = tok, client = client, token_url = token_url), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL)
  out
}

saxo_auth_live <- function(env = Sys.getenv("TRADING_ENV", "sim"), force = FALSE) {
  ep  <- .saxo_endpoints(env)
  cid <- Sys.getenv("SAXO_CLIENT_ID")
  csc <- Sys.getenv("SAXO_CLIENT_SECRET")
  red <- Sys.getenv("SAXO_REDIRECT_URI", unset = "http://localhost:1410/saxoapp")
  
  if (cid == "" || csc == "") stop("SAXO_CLIENT_ID tai SAXO_CLIENT_SECRET puuttuu .Renvironista.")
  
  cache_file <- .saxo_cache_path(env)
  if (!force && file.exists(cache_file)) {
    tok <- try(readRDS(cache_file), silent = TRUE)
    if (!inherits(tok, "try-error") && .saxo_token_valid(tok)) {
      .saxo_log("🔐 Käytetään välimuistin Saxo-tokenia (", cache_file, ").")
      assign("saxo_token", tok, envir = .GlobalEnv)
      return(tok)
    }
  }
  
  client <- httr2::oauth_client(id = cid, token_url = ep$token, secret = csc)
  
  # Refresh jos löytyy vanha
  if (!force && exists("tok") && !is.null(tok)) {
    ref <- .saxo_try_refresh(tok, client, ep$token)
    if (!is.null(ref) && .saxo_token_valid(ref)) {
      .saxo_log("✅ Refresh onnistui — tallennetaan uusi token.")
      saveRDS(ref, cache_file); assign("saxo_token", ref, envir = .GlobalEnv); return(ref)
    }
  }
  
  .saxo_log("🌐 Avataan selaimeen Saxo-kirjautuminen (", env, ").")
  # HUOM: httr2 käyttää parametriNIMEÄ 'auth_url'
  tok <- httr2::oauth_flow_auth_code(
    client       = client,
    auth_url     = ep$authorize,
    scope        = "openid offline_access trade read",
    redirect_uri = red
  )
  if (!inherits(tok, "httr2_token")) stop("OAuth-vaihto ei palauttanut httr2_token-objektia.")
  
  .saxo_log("✅ Saxo-token haettu. Tallennetaan välimuistiin: ", cache_file)
  saveRDS(tok, cache_file)
  assign("saxo_token", tok, envir = .GlobalEnv)
  tok
}
