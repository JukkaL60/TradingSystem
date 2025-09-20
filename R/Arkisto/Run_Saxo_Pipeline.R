# ============================================
# Run_Saxo_Pipeline.R
# Master pipeline Saxo OpenAPI -datan hakuun
# ============================================

# 1) Lataa Saxo OpenAPI -funktiot
source(file.path("R", "saxo_openapi_live.R"))

# pikkutools
.clean_names <- function(x) {
  x <- sub("^PositionBase\\.", "", x)
  x <- sub("^PositionView\\.", "", x)
  x <- sub("^DisplayAndFormat\\.", "", x)
  x
}
.flatten_if_needed <- function(df) {
  if (is.data.frame(df) && any(vapply(df, is.list, logical(1)))) {
    jsonlite::flatten(df)
  } else df
}
.supports_arg <- function(fun, arg) {
  arg %in% names(formals(fun))
}

# 2) Funktio, joka hoitaa koko prosessin
saxo_run_pipeline <- function() {
  message("🔑 Vaihe 1: Autentikoidaan Saxo OpenAPI...")
  saxo_auth_live()   # avaa selaimen tarvittaessa
  message("✅ Auth OK, token haettu.")
  
  # 3) Tilit
  message("📂 Vaihe 2: Haetaan tilitiedot...")
  acc <- saxo_get_accounts()
  accounts_df <- .flatten_if_needed(acc$Data)
  message("   Tilirivejä: ", nrow(accounts_df))
  
  # 4) Positiot – toimii riippumatta saxo_get_positions()-signatuurista
  message("📂 Vaihe 3: Haetaan positiot...")
  
  positions_df <- NULL
  if (exists("saxo_get_positions")) {
    if (.supports_arg(saxo_get_positions, "field_groups")) {
      # Uudempi versio tukee field_groups-parametria
      pos <- saxo_get_positions(field_groups = "PositionBase,PositionView,DisplayAndFormat")
      positions_df <- .flatten_if_needed(pos$Data)
    } else {
      # Vanhempi versio: hae suoraan API:sta rikkaat kentät
      req <- .saxo_req("port/v1/positions/me") |>
        req_url_query(FieldGroups = "PositionBase,PositionView,DisplayAndFormat")
      resp <- .saxo_perform(req)
      body <- resp_body_json(resp, simplifyVector = TRUE)
      positions_df <- .flatten_if_needed(body$Data)
    }
  } else {
    stop("saxo_get_positions() ei löydy. Varmista että R/saxo_openapi_live.R on ladattu.")
  }
  
  # Valitse hyödylliset sarakkeet turvallisesti
  want <- c(
    "NetPositionId",
    "PositionBase.AccountId","PositionBase.AccountKey",
    "PositionBase.Uic","PositionBase.AssetType","PositionBase.Amount",
    "PositionBase.OpenPrice","PositionBase.OpenPriceIncludingCosts",
    "PositionBase.ExecutionTimeOpen","PositionBase.Status",
    "PositionView.CalculationReliability",
    "DisplayAndFormat.Symbol","DisplayAndFormat.Description","DisplayAndFormat.Currency"
  )
  keep <- intersect(want, names(positions_df))
  positions_view <- if (length(keep)) positions_df[, keep, drop = FALSE] else positions_df
  names(positions_view) <- .clean_names(names(positions_view))
  
  if (nrow(positions_view)) {
    message("   Positioita: ", nrow(positions_view))
    print(utils::head(positions_view, 5))
  } else {
    message("   ⚠️ Ei aktiivisia positioita / sarakkeita.")
  }
  
  message("🎉 Saxo pipeline valmis.")
  invisible(list(
    accounts  = accounts_df,
    positions = positions_view
  ))
}
