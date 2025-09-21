# ============================================
# R/fetch_positions.R — hae positiot ja lisää UnderlyingUic/UnderlyingAssetType
# ============================================

# Apurit
.clean_names <- function(x) {
  x <- sub("^PositionBase\\.", "", x)
  x <- sub("^PositionView\\.", "", x)
  x <- sub("^DisplayAndFormat\\.", "", x)
  x
}

.flatten_if_needed <- function(df) {
  if (is.data.frame(df) && any(vapply(df, is.list, logical(1)))) {
    jsonlite::flatten(df)
  } else {
    df
  }
}

# Hae instrumenttien detaljit (Underlying-tiedot) batchina
.get_instrument_details <- function(uics) {
  uics <- unique(na.omit(as.integer(uics)))
  if (!length(uics)) return(tibble::tibble())
  resp <- perform_req(
    build_req("ref/v1/instruments/details") |>
      httr2::req_url_query(Uics = paste(uics, collapse = ","), FieldGroups = "All")
  )
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  tib <- .flatten_if_needed(body$Data)
  if (!is.data.frame(tib) || !nrow(tib)) tibble::tibble() else tib
}

# Pääfunktio
fetch_positions <- function() {
  # HUOM: pidä FieldGroups tässä minimissä (muuten Saxo voi antaa 400 tälle endpointille)
  resp <- perform_req(
    build_req("port/v1/positions/me") |>
      httr2::req_url_query(FieldGroups = "PositionBase,PositionView,DisplayAndFormat")
  )
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  df   <- .flatten_if_needed(body$Data)
  
  # Jos ei dataa, palauta tyhjä kehys
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(data.frame(
      Uic = integer(),
      NetPositionId = character(),
      Amount = double(),
      AverageOpenPrice = double(),
      stringsAsFactors = FALSE
    ))
  }
  
  want <- c(
    "NetPositionId","PositionBase.AccountId","PositionBase.AccountKey",
    "PositionBase.Uic","PositionBase.AssetType","PositionBase.Amount",
    "PositionBase.OpenPrice","PositionBase.OpenPriceIncludingCosts",
    "PositionBase.ExecutionTimeOpen","PositionBase.Status",
    "PositionView.CalculationReliability",
    "DisplayAndFormat.Symbol","DisplayAndFormat.Description","DisplayAndFormat.Currency"
  )
  keep <- intersect(want, names(df))
  out  <- if (length(keep)) df[, keep, drop = FALSE] else df
  names(out) <- .clean_names(names(out))
  
  # Yhteensopivuus: AverageOpenPrice jos puuttuu
  if (!"AverageOpenPrice" %in% names(out) && "OpenPrice" %in% names(out)) {
    out$AverageOpenPrice <- out$OpenPrice
  }
  
  # Tyypit
  if ("Uic" %in% names(out)) out$Uic <- suppressWarnings(as.integer(out$Uic))
  
  # Hae Underlying-tiedot detailsista ja liitä
  det <- try(.get_instrument_details(out$Uic), silent = TRUE)
  if (!inherits(det, "try-error") && is.data.frame(det) && nrow(det) > 0) {
    # Poimi mahdolliset kentät eri nimillä (Saxo voi palauttaa molempia muotoja)
    det_min <- det |>
      dplyr::transmute(
        Uic = suppressWarnings(as.integer(.data$Uic)),
        Det_UnderlyingUic = suppressWarnings(as.integer(
          dplyr::coalesce(.data$`UnderlyingAsset.Uic`, .data$UnderlyingUic)
        )),
        Det_UnderlyingAssetType = dplyr::coalesce(
          .data$`UnderlyingAsset.AssetType`, .data$UnderlyingAssetType
        )
      )
    out <- out |>
      dplyr::left_join(det_min, by = "Uic")
  } else {
    out$Det_UnderlyingUic <- NA_integer_
    out$Det_UnderlyingAssetType <- NA_character_
  }
  
  # Lopulliset UnderlyingUic/UnderlyingAssetType (jos details puuttuu, käytä omaa UIC/AssetType)
  out$UnderlyingUic <- dplyr::coalesce(out$Det_UnderlyingUic, out$Uic)
  out$UnderlyingAssetType <- dplyr::coalesce(out$Det_UnderlyingAssetType, out$AssetType)
  out <- dplyr::select(out, -dplyr::starts_with("Det_"))
  
  out
}

# Julkinen objekti, jota Run_Trading_System.R odottaa
positions_df <- tryCatch(
  fetch_positions(),
  error = function(e) {
    message("⚠️ fetch_positions: ", conditionMessage(e))
    data.frame(
      Uic = integer(),
      NetPositionId = character(),
      Amount = double(),
      AverageOpenPrice = double(),
      stringsAsFactors = FALSE
    )
  }
)
