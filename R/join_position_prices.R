# ============================================
# R/join_position_prices.R
# In-memory join: positions_df + position_prices_df  -> positions_df (rikastettuna)
# Ei lue eikä kirjoita CSV:itä (paitsi jos write_csv=TRUE erikseen pyydetään)
# ============================================

suppressPackageStartupMessages({
  library(dplyr)
})

# NA-turvallinen numeerinen coalesce
coalesce_num <- function(...) suppressWarnings(dplyr::coalesce(...))

# Luo CurrentPrice seuraavalla prioriteetilla:
# 1) Mid  2) LastTraded  3) Avg(Bid,Ask)
# Lisäksi: jos Bid=0 JA Ask=0 -> tulkitaan "ei hintaa" (NA), EI arvosteta nollaan.
.compute_current_price <- function(pr_df) {
  pr_df %>%
    mutate(
      Mid        = suppressWarnings(as.numeric(Mid)),
      Bid        = suppressWarnings(as.numeric(Bid)),
      Ask        = suppressWarnings(as.numeric(Ask)),
      LastTraded = suppressWarnings(as.numeric(LastTraded)),
      
      # Jos molemmat Bid ja Ask ovat 0 -> treat as NA (ei hintaa)
      Bid_clean  = ifelse(is.finite(Bid) & is.finite(Ask) & Bid == 0 & Ask == 0, NA_real_, Bid),
      Ask_clean  = ifelse(is.finite(Bid) & is.finite(Ask) & Bid == 0 & Ask == 0, NA_real_, Ask),
      
      AvgBA      = ifelse(rowSums(is.finite(cbind(Bid_clean, Ask_clean))) > 0,
                          rowMeans(cbind(Bid_clean, Ask_clean), na.rm = TRUE),
                          NA_real_),
      
      CurrentPrice = coalesce_num(Mid, LastTraded, AvgBA)
    ) %>%
    select(-AvgBA, -Bid_clean, -Ask_clean)
}

# --------------------------------------------
# PUBLIC: join_position_prices_mem
# --------------------------------------------
# positions_df         : data.frame, sisältää ainakin Uic-sarakkeen
# position_prices_df   : data.frame, sisältää ainakin Uic + (Mid/Bid/Ask/LastTraded)
# write_csv            : FALSE (ei outputteja oletuksena)
# verbose              : TRUE -> konsoliin diagnostiikka
#
# Palauttaa: positions_df rikastettuna hinnankentillä
# --------------------------------------------
join_position_prices_mem <- function(positions_df,
                                     position_prices_df,
                                     write_csv = FALSE,
                                     verbose = TRUE) {
  stopifnot(is.data.frame(positions_df),
            is.data.frame(position_prices_df))
  
  if (!"Uic" %in% names(positions_df)) {
    stop("positions_df: Uic-sarake puuttuu.")
  }
  if (!"Uic" %in% names(position_prices_df)) {
    stop("position_prices_df: Uic-sarake puuttuu.")
  }
  
  # Varmista minimi-kolumnit price-tauluun
  need_cols <- c("Uic","Mid","Bid","Ask","LastTraded","LastUpdated","Currency","Symbol")
  miss <- setdiff(need_cols, names(position_prices_df))
  if (length(miss)) {
    for (m in miss) position_prices_df[[m]] <- NA
  }
  
  # Laske CurrentPrice ja putsaa 0/0-quotet
  prices_ready <- .compute_current_price(position_prices_df)
  
  # Diagnostiikka
  if (isTRUE(verbose)) {
    n_all <- nrow(prices_ready)
    mid_ok <- sum(is.finite(prices_ready$Mid), na.rm = TRUE)
    lt_ok  <- sum(is.finite(prices_ready$LastTraded), na.rm = TRUE)
    ba_ok  <- sum(is.finite((prices_ready$Bid + prices_ready$Ask)/2) & !(prices_ready$Bid == 0 & prices_ready$Ask == 0), na.rm = TRUE)
    cp_ok  <- sum(is.finite(prices_ready$CurrentPrice), na.rm = TRUE)
    message(sprintf("ℹ️ Hintahavaintoja: %d | Mid=%d | LastTraded=%d | Avg(BA)~=%d | CurrentPrice=%d",
                    n_all, mid_ok, lt_ok, ba_ok, cp_ok))
  }
  
  # Yhdistä Uic:lla positioihin (ei pudoteta position rivejä)
  out <- positions_df %>%
    left_join(
      prices_ready %>%
        select(Uic, CurrentPrice, Mid, Bid, Ask, LastTraded, LastUpdated, Currency, Symbol),
      by = "Uic"
    )
  
  # Haluttaessa kirjoita tarkistustiedosto (ei oletuksena)
  if (isTRUE(write_csv)) {
    if (!dir.exists("Output")) dir.create("Output", showWarnings = FALSE)
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    path  <- file.path("Output", paste0("Positions_With_Prices_", stamp, ".csv"))
    readr::write_csv(out, path)
    message("✅ Positions + prices tallennettu: ", path)
  }
  
  out
}
