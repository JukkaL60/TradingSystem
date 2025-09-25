# ============================================
# R/fetch_underlying_prices_job.R
# Hakee underlying-hinnat erillisenä jobina ja tallettaa CSV:ksi
# Käyttää /trade/v1/infoprices/list + AccountKey + Amount + AssetType
# ============================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(jsonlite); library(httr2); library(readr); library(stringr)
})

`%||%` <- function(a,b) if (!is.null(a)) a else b

# -- AssetType-mapping: option -> underlying assettype ------------
map_underlying_assettype <- function(at) {
  at <- as.character(at %||% NA_character_)
  dplyr::case_when(
    at %in% c("StockIndexOption","IndexOption") ~ "StockIndex",
    at %in% c("FuturesOption")                  ~ "ContractFutures",  # KORJAUS
    at %in% c("StockOption")                    ~ "Stock",
    at %in% c("FxOption","FxVanillaOption")     ~ "FxSpot",
    TRUE                                        ~ at
  )
}


# -- Oletus-Amount per AssetType (infoprices vaatii tämän usein) --
default_amount <- function(asset_type) {
  dplyr::case_when(
    asset_type %in% c("FxSpot")      ~ 100000,
    asset_type %in% c("StockIndex")  ~ 1,
    asset_type %in% c("Futures")     ~ 1,
    asset_type %in% c("Stock")       ~ 1,
    TRUE                             ~ 1
  )
}

# -- Nouda AccountKey muistista tai Output/Accounts_*.csv:stä -----
get_account_key <- function() {
  if (exists("accounts_df", inherits = TRUE) &&
      is.data.frame(accounts_df) && "AccountKey" %in% names(accounts_df) &&
      nrow(accounts_df) > 0) {
    return(accounts_df$AccountKey[[1]])
  }
  files <- list.files("Output", pattern = "^Accounts_\\d{8}_\\d{6}\\.csv$", full.names = TRUE)
  if (length(files)) {
    acc <- try(readr::read_csv(tail(sort(files), 1), show_col_types = FALSE), silent = TRUE)
    if (!inherits(acc, "try-error") && "AccountKey" %in% names(acc) && nrow(acc) > 0) {
      return(acc$AccountKey[[1]])
    }
  }
  stop("AccountKey ei löytynyt (muistista eikä Output/Accounts_*.csv:stä).")
}

# -- /trade/v1/infoprices/list pyyntö ------------------------------
rows <- purrr::map(items, function(it) {
  f <- try(jsonlite::flatten(it), silent = TRUE)
  if (inherits(f, "try-error")) return(NULL)
  
  mid  <- suppressWarnings(as.numeric(f$Quote.Mid))
  last <- suppressWarnings(as.numeric(f$PriceInfo.LastTraded))
  bid  <- suppressWarnings(as.numeric(f$Quote.Bid))
  ask  <- suppressWarnings(as.numeric(f$Quote.Ask))
  
  # Jos molemmat Bid ja Ask ovat 0 -> tulkitaan "ei hintaa"
  if (is.finite(bid) && is.finite(ask) && bid == 0 && ask == 0) {
    bid <- NA_real_; ask <- NA_real_
  }
  
  ab   <- if (is.finite(ask) || is.finite(bid)) mean(c(ask, bid), na.rm = TRUE) else NA_real_
  px   <- dplyr::coalesce(mid, last, ab)
  
  tibble::tibble(
    UnderlyingUic   = suppressWarnings(as.integer(f$Uic)),
    UnderlyingPrice = suppressWarnings(as.numeric(px)),
    QuoteTime       = as.character(f$LastUpdated %||% f$Quote.Updated %||% NA_character_),
    Source          = if (!is.na(mid)) "Mid" else if (!is.na(last)) "LastTraded" else if (!is.na(ab)) "Avg(Ask,Bid)" else NA_character_
  )
})

# --- Julkinen: jobi ---------------------------------------------

fetch_underlying_prices_job <- function(positions_input = NULL,
                                        write_csv = TRUE) {
  
  # 0) Lähde-positions
  if (is.null(positions_input)) {
    files <- list.files("Output", pattern = "^Positions_\\d{8}_\\d{6}\\.csv$", full.names = TRUE)
    if (!length(files)) {
      message("⚠️ Ei löydy Output/Positions_*.csv — anna positions_input parametrina.")
      return(invisible(tibble()))
    }
    positions_df <- readr::read_csv(tail(sort(files), 1), show_col_types = FALSE)
  } else {
    positions_df <- positions_input
  }
  
  if (!is.data.frame(positions_df) || !nrow(positions_df)) {
    message("ℹ️ Ei positioita — ei haettavia hintoja.")
    return(invisible(tibble()))
  }
  
  # 1) Varmista UnderlyingUic ja mapita underlyingin AssetType
  pos <- positions_df %>%
    mutate(
      UnderlyingUic       = if (!"UnderlyingUic" %in% names(.)) Uic else UnderlyingUic,
      UnderlyingAssetType = if (!"UnderlyingAssetType" %in% names(.)) AssetType else UnderlyingAssetType,
      UnderlyingAssetType = map_underlying_assettype(UnderlyingAssetType)
    )
  
  u_tbl <- pos %>%
    distinct(UnderlyingUic, UnderlyingAssetType) %>%
    filter(!is.na(UnderlyingUic), nzchar(UnderlyingAssetType))
  
  if (!nrow(u_tbl)) {
    message("ℹ️ Ei löydy unique UnderlyingUic -rivejä.")
    return(invisible(tibble()))
  }
  
  # 2) AccountKey ja Amount per asset type
  account_key <- get_account_key()
  
  # 3) Hae per AssetType ryhmissä /list -kutsulla
  results <- u_tbl %>%
    group_by(UnderlyingAssetType) %>%
    group_map(~{
      at <- .y$UnderlyingAssetType[[1]]
      amt <- default_amount(at)
      message("🔎 Haetaan hinnat AssetType=", at, " (", nrow(.x), " UICs, Amount=", amt, ")")
      sx_infoprices_list(
        uics        = .x$UnderlyingUic,
        asset_type  = at,
        account_key = account_key,
        amount      = amt
      )
    }) %>%
    bind_rows()
  
  if (!nrow(results)) {
    message("⚠️ Underlying-hintoja ei saatu yhdellekään UnderlyingUicille.")
    return(invisible(tibble()))
  }
  
  # 4) Dedup ja talletus
  out <- results %>%
    filter(is.finite(UnderlyingUic), is.finite(UnderlyingPrice)) %>%
    group_by(UnderlyingUic) %>% slice_head(n = 1) %>% ungroup()
  
  if (write_csv && nrow(out)) {
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    path  <- file.path("Output", paste0("Underlying_Prices_", stamp, ".csv"))
    readr::write_csv(out, path)
    message("✅ Underlying-hinnat talletettu: ", path)
  } else if (!nrow(out)) {
    message("⚠️ Underlying-hintoja ei saatu (kaikki hinnat puuttuivat).")
  }
  
  out
}
