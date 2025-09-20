# compute_metrics.R — laskee perusmittarit vikasietoisesti

if (!exists("positions_df")) stop("positions_df puuttuu (compute_metrics)")

if (nrow(positions_df) == 0) {
  message("ℹ️ Ei positioita — metrics_df jätetään tyhjäksi.")
  metrics_df <- positions_df
} else {
  # quotes_df valinnainen; jos puuttuu, tee runko oikeilla sarakkeilla
  if (!exists("quotes_df")) {
    quotes_df <- data.frame(
      Uic         = unique(positions_df$Uic),
      LastTraded  = NA_real_,
      Bid         = NA_real_,
      Ask         = NA_real_
    )
  }
  suppressWarnings({
    metrics_df <- dplyr::left_join(positions_df, quotes_df, by = "Uic") |>
      dplyr::mutate(
        LastPrice    = dplyr::coalesce(LastTraded, (Bid + Ask) / 2),
        MarketValue  = Amount * LastPrice,
        CostValue    = Amount * AverageOpenPrice,
        UnrealizedPL = MarketValue - CostValue,
        PL_pct       = dplyr::if_else(CostValue != 0, UnrealizedPL / CostValue, NA_real_)
      )
  })
}
