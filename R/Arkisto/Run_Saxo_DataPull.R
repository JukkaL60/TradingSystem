# =========================================================
# Run_Saxo_DataPull.R
# Kokonaisprosessi: Auth -> Accounts -> Positions/NetPositions -> Histories
# - Kestää katkokset (401/5xx/429), yrittää uudelleen ja tekee re-authin tarvittaessa
# - Tallentaa CSV:t päiväleimattuun kansioon: Data/Saxo/YYYYMMDD/
# Vaatii: R/saxo_openapi_live.R (funktiot)
# =========================================================

# ---------- 1) alustukset ----------
message("==> Ladataan funktiot...")
source(file.path("R","saxo_openapi_live.R"))

# varmista ympäristömuuttujat
needed <- c("SAXO_ENV","SAXO_APP_KEY","SAXO_APP_SECRET","SAXO_REDIRECT_URI")
env_ok <- all(nzchar(Sys.getenv(needed)))
if (!env_ok) {
  stop("Ympäristömuuttujia puuttuu. Tarkista .Renviron: ",
       paste(needed, collapse=", "))
}

# tuloste- ja välihakuasetukset
.run_ts        <- format(Sys.time(), "%Y%m%d_%H%M%S")
.out_dir       <- file.path("Data","Saxo", format(Sys.Date(), "%Y%m%d"))
.dir_ok        <- if (!dir.exists(.out_dir)) dir.create(.out_dir, recursive = TRUE) else TRUE
.history_limit <- 30      # max historian instrumenttien määrä (turvallinen raja alkuun)
.sleep_between <- 0.25    # viive kutsujen väliin (sekuntia), vähentää rate limit -riskiä

# ---------- 2) apurit (retry + reauth) ----------
# yleiswrap, joka yrittää re-authin jos 401 ja backoffaa 5xx/429
.sxo_try <- function(expr, label = "call", tries = 3) {
  delay <- 0.8
  for (i in seq_len(tries)) {
    ok <- TRUE
    res <- tryCatch(
      force(expr),
      error = function(e) { ok <<- FALSE; e }
    )
    if (ok) return(res)
    
    msg <- conditionMessage(res)
    # heuristiikka: 401 -> reauth
    if (grepl("401|Unauthorized", msg, ignore.case = TRUE)) {
      message("   [", label, "] 401 havaittu → yritetään re-auth...")
      try(saxo_auth_live(), silent = TRUE)
    } else if (grepl("429|Too Many Requests|5\\d\\d", msg)) {
      message("   [", label, "] palvelin/rate rajoitus → odotetaan ", round(delay,2), "s ja yritetään uudelleen...")
      Sys.sleep(delay)
      delay <- min(delay * 1.6, 6)
    } else {
      message("   [", label, "] virhe: ", msg)
    }
  }
  stop("Epäonnistui usean yrityksen jälkeen: ", label)
}

# litistys, jos Saxo palauttaa ryhmiteltyjä sarakkeita (PositionBase.*, DisplayAndFormat.*)
.flatten_if_needed <- function(df) {
  if (is.data.frame(df) && any(sapply(df, is.list))) {
    return(jsonlite::flatten(df))
  }
  df
}

# ---------- 3) keep-alive ja auth varmennus ----------
.saxo_ensure_auth <- function() {
  message("==> Varmistetaan yhteys (ping/auth)...")
  # nopea ping: jos kaatuu, tee auth
  ping_ok <- TRUE
  tryCatch({
    .sxo_try(saxo_ping(), "ping", tries = 2)
  }, error = function(e) {
    ping_ok <<- FALSE
  })
  if (!ping_ok) {
    message("   Ping epäonnistui → tehdään auth...")
    saxo_auth_live()
    .sxo_try(saxo_ping(), "ping_jälkeen_auth", tries = 2)
  }
  invisible(TRUE)
}

# ---------- 4) pääaskeleet ----------
.fetch_accounts <- function() {
  message("==> Haetaan tilit (accounts)...")
  acc <- .sxo_try(saxo_get_accounts(), "accounts")
  dat <- acc$Data
  dat <- .flatten_if_needed(dat)
  out <- file.path(.out_dir, paste0("accounts_", .run_ts, ".csv"))
  utils::write.csv(dat, out, row.names = FALSE)
  message("   OK: ", nrow(dat), " rivi(ä) → ", out)
  dat
}

.fetch_positions <- function() {
  message("==> Haetaan positiot (positions)...")
  pos <- .sxo_try({
    # varmista että saamme rikkaat kentät + flatten
    if ("saxo_get_positions" %in% ls()) {
      saxo_get_positions(field_groups = "PositionBase,PositionView,DisplayAndFormat")
    } else {
      # fallback jos funktio ei ole päivitetty
      resp <- .sxo_try(.saxo_perform(.saxo_req("port/v1/positions/me") |>
                                       req_url_query(FieldGroups = "PositionBase,PositionView,DisplayAndFormat")),
                       "positions_raw")
      list(Data = resp_body_json(resp, simplifyVector = TRUE)$Data)
    }
  }, "positions")
  dat <- .flatten_if_needed(pos$Data)
  out <- file.path(.out_dir, paste0("positions_", .run_ts, ".csv"))
  utils::write.csv(dat, out, row.names = FALSE)
  message("   OK: ", nrow(dat), " rivi(ä) → ", out)
  dat
}

.fetch_netpositions <- function() {
  message("==> Haetaan nettopositiot (netpositions)...")
  npos <- .sxo_try({
    if ("saxo_get_netpositions" %in% ls()) {
      saxo_get_netpositions(field_groups = "NetPositionBase,DisplayAndFormat")
    } else {
      resp <- .sxo_try(.saxo_perform(.saxo_req("port/v1/netpositions/me") |>
                                       req_url_query(FieldGroups = "NetPositionBase,DisplayAndFormat")),
                       "netpositions_raw")
      list(Data = resp_body_json(resp, simplifyVector = TRUE)$Data)
    }
  }, "netpositions")
  dat <- .flatten_if_needed(npos$Data)
  out <- file.path(.out_dir, paste0("netpositions_", .run_ts, ".csv"))
  utils::write.csv(dat, out, row.names = FALSE)
  message("   OK: ", nrow(dat), " rivi(ä) → ", out)
  dat
}

.fetch_histories <- function(positions_df, max_instruments = .history_limit) {
  # poimi UIC + AssetType (PositionBase-prefiksit tai jo siistittynä)
  message("==> Haetaan historiat (chart/v1/charts)...")
  cols <- names(positions_df)
  uic_col <- if ("PositionBase.Uic" %in% cols) "PositionBase.Uic" else if ("Uic" %in% cols) "Uic" else NA
  ast_col <- if ("PositionBase.AssetType" %in% cols) "PositionBase.AssetType" else if ("AssetType" %in% cols) "AssetType" else NA
  if (is.na(uic_col) || is.na(ast_col)) {
    message("   Ei löydy Uic/AssetType sarakkeita → ohitetaan history-haku.")
    return(invisible(NULL))
  }
  
  # uniikit instrumentit
  instr <- unique(positions_df[, c(uic_col, ast_col)])
  names(instr) <- c("Uic","AssetType")
  # siivoa mahdolliset tyhjät/NA
  instr <- instr[stats::complete.cases(instr), ]
  n_total <- nrow(instr)
  if (n_total == 0) {
    message("   Ei instrumentteja → skip.")
    return(invisible(NULL))
  }
  if (n_total > max_instruments) {
    message("   Instrumentteja ", n_total, " → rajoitetaan ", max_instruments, " kpl (muuta .history_limit jos haluat kaikki).")
    instr <- instr[seq_len(max_instruments), ]
  }
  
  out_all <- list()
  for (i in seq_len(nrow(instr))) {
    u <- instr$Uic[i]
    a <- instr$AssetType[i]
    lab <- paste0("history_", u, "_", a)
    message(sprintf("   [%d/%d] %s (%s) – 1D, Year", i, nrow(instr), u, a))
    h <- tryCatch(
      .sxo_try(saxo_get_history(uic = u, asset_type = a, horizon = 1440, timerange = "Year"), lab),
      error = function(e) { message("     -> VIRHE: ", conditionMessage(e)); NULL }
    )
    if (!is.null(h) && length(h)) {
      df <- as.data.frame(h)
      # normaali sarakevalinta jos olemassa
      keep <- intersect(c("Time","Open","High","Low","Close","Volume"), names(df))
      df   <- df[, keep, drop = FALSE]
      out_file <- file.path(.out_dir, paste0("history_", u, "_", a, "_", .run_ts, ".csv"))
      utils::write.csv(df, out_file, row.names = FALSE)
      out_all[[paste0(u,"_",a)]] <- out_file
    }
    Sys.sleep(.sleep_between)
  }
  message("   Historia-exportit: ", length(out_all), " tiedostoa.")
  invisible(out_all)
}

# ---------- 5) pääproseduuri ----------
saxo_run_pipeline <- function() {
  message("=================================================")
  message("Saxo Data Pull – käynnistyy (ENV = ", Sys.getenv("SAXO_ENV","?"), ")")
  message("Tulostekansio: ", normalizePath(.out_dir, winslash = "/"))
  message("=================================================")
  
  .saxo_ensure_auth()
  
  acc_df  <- .fetch_accounts()
  pos_df  <- .fetch_positions()
  npos_df <- .fetch_netpositions()
  
  # historiat vain, jos on instrumentteja
  .fetch_histories(pos_df, max_instruments = .history_limit)
  
  message("==> Valmis. Päädatatiedostot kansiossa: ", normalizePath(.out_dir, winslash = "/"))
  invisible(list(
    accounts_path     = list.files(.out_dir, pattern = "^accounts_.*\\.csv$", full.names = TRUE),
    positions_path    = list.files(.out_dir, pattern = "^positions_.*\\.csv$", full.names = TRUE),
    netpositions_path = list.files(.out_dir, pattern = "^netpositions_.*\\.csv$", full.names = TRUE),
    histories_paths   = list.files(.out_dir, pattern = "^history_.*\\.csv$", full.names = TRUE)
  ))
}

# --------- (valinnainen) aja heti kun tiedosto sourcataan ----------
# saxo_run_pipeline()
