# ============================================
# R/compute_metrics.R
# Laskee tunnusluvut suoraan positions_df:stä, joka on jo rikastettu hinnoilla
# (CurrentPrice, Mid, Bid, Ask, LastTraded). Ei käytä enää quotes_df:ää.
# ============================================

suppressPackageStartupMessages({
  library(dplyr)
})

# NA-turvallinen numeerinen coalesce
coalesce_num <- function(...) suppressWarnings(dplyr::coalesce(...))

# Varmista, että sarake on olemassa; jos ei, lisää NA-arvoilla
.ensure_col <- function(df, name, value = NA) {
  if (!name %in% names(df)) df[[name]] <- value
  df
}

compute_metrics_from_positions <- function(pos_df) {
  stopifnot(is.data.frame(pos_df))
  
  # Varmista minimi-sarakkeet (osa voi puuttua SIM-ympäristössä)
  need <- c("Uic","AssetType","Amount","CurrentPrice","Mid","Bid","Ask","LastTraded","AverageOpenPrice","OpenPrice")
  for (nm in need) pos_df <- .ensure_col(pos_df, nm, NA)
  
  # Muunna numeeriset turvallisesti
  numify <- function(x) suppressWarnings(as.numeric(x))
  
  out <- pos_df %>%
    mutate(
      Amount        = numify(Amount),
      CurrentPrice  = numify(CurrentPrice),
      Mid           = numify(Mid),
      Bid           = numify(Bid),
      Ask           = numify(Ask),
      LastTraded    = numify(LastTraded),
      AverageOpenPrice = numify(AverageOpenPrice),
      OpenPrice         = numify(OpenPrice),
      
      # Fallback-hinta, jos CurrentPrice puuttuu:
      AvgBA         = ifelse(rowSums(is.finite(cbind(Bid, Ask))) > 0,
                             rowMeans(cbind(Bid, Ask), na.rm = TRUE),
                             NA_real_),
      LastPriceFallback = coalesce_num(Mid, LastTraded, AvgBA),
      
      # Lopullinen käytettävä hinta
      PriceUsed     = coalesce_num(CurrentPrice, LastPriceFallback),
      
      # Keski-avauksen hinta (fallback OpenPriceen)
      AvgOpenUsed   = coalesce_num(AverageOpenPrice, OpenPrice),
      
      # Markkina-arvo ja realisoitumaton P/L (yksinkertainen arvotus)
      MarketValue   = ifelse(is.finite(Amount) & is.finite(PriceUsed),
                             Amount * PriceUsed, NA_real_),
      UnrealizedPnL = ifelse(is.finite(Amount) & is.finite(AvgOpenUsed) & is.finite(PriceUsed),
                             (PriceUsed - AvgOpenUsed) * Amount, NA_real_)
    ) %>%
    # Valitse järkevä perusnäkymä metricsiin. Lisää/poista kenttiä tarpeen mukaan.
    select(
      Uic, AssetType, Amount,
      PriceUsed, CurrentPrice, Mid, Bid, Ask, LastTraded,
      AvgOpenUsed, MarketValue, UnrealizedPnL,
      dplyr::any_of(c("Symbol","Currency","Expiry","Strike","PutCall","NetPositionId"))
    )
  
  out
}

# Suorita laskenta ja laita tulos metrics_df-muuttujaan
metrics_df <- compute_metrics_from_positions(
  if (exists("positions_df") && is.data.frame(positions_df)) positions_df else {
    stop("positions_df puuttuu ympäristöstä.")
  }
)
