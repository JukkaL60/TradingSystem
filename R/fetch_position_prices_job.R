# ============================================
# R/fetch_position_prices_job.R
# Hakee POSITIOIDEN (instrumentin) hinnat /trade/v1/infoprices/list:llä
# ja tallettaa CSV:ksi Output/Position_Prices_*.csv
# ============================================
# https://gateway.saxobank.com/sim/openapi/trade/v1/infoprices/list

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(tibble)
  library(jsonlite); library(httr2); library(readr)
})

`%||%` <- function(a,b) if (!is.null(a)) a else b

# Oletus-Amount AssetTypen mukaan (Saxo usein vaatii Amount-parametrin)
# Huom: käytetään Saxon "Extended AssetTypeja"
default_amount <- function(asset_type) {
  dplyr::case_when(
    asset_type %in% c("FxSpot") ~ 100000,
    asset_type %in% c("Stock","Etf") ~ 1,
    asset_type %in% c("ContractFutures","Futures","CfdOnFutures") ~ 1,
    asset_type %in% c("StockIndex") ~ 1,
    asset_type %in% c("StockOption","StockIndexOption","FuturesOption","FxOption","FxVanillaOption") ~ 1, # optiolle 1 kontrakti
    TRUE ~ 1
  )
}

# Palauttaa aina yhden numeron: jos NULL/tyhjä, NA_real_, muuten as.numeric(first)
num1 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x[[1]]))
}

# Hae AccountKey muistista tai viimeisimmästä Accounts_*.csv:stä
get_account_key <- function() {
  if (exists("accounts_df", inherits = TRUE) &&
      is.data.frame(accounts_df) && "AccountKey" %in% names(accounts_df) &&
      nrow(accounts_df) > 0) {
    return(accounts_df$AccountKey[[1]])
  }
  files <- list.files("Output", pattern="^Accounts_\\d{8}_\\d{6}\\.csv$", full.names = TRUE)
  if (length(files)) {
    acc <- try(readr::read_csv(tail(sort(files), 1), show_col_types = FALSE), silent = TRUE)
    if (!inherits(acc, "try-error") && "AccountKey" %in% names(acc) && nrow(acc) > 0) {
      return(acc$AccountKey[[1]])
    }
  }
  stop("AccountKey ei löytynyt (muistista eikä Output/Accounts_*.csv:stä).")
}

# /trade/v1/infoprices/list yhdelle AssetTypelle (chunkkaa UIC:t)
sx_infoprices_list <- function(uics, asset_type, account_key, amount,
                               field_groups = "DisplayAndFormat,Quote,PriceInfo") {
  uics <- unique(as.integer(na.omit(uics)))
  if (!length(uics)) return(tibble())
  
  fetch_chunk <- function(chunk) {
    req <- build_req("trade/v1/infoprices/list") |>
      httr2::req_url_query(
        AccountKey = account_key,
        Uics       = paste(chunk, collapse = ","),
        AssetType  = asset_type,
        Amount     = amount,
        FieldGroups = field_groups
      ) |>
      httr2::req_error(is_error = function(resp) FALSE)
    
    resp <- try(perform_req(req), silent = TRUE)
    if (inherits(resp, "try-error") || is.null(resp)) {
      message("❌ infoprices/list request failed (AT=", asset_type, ", nUics=", length(chunk), ")")
      return(tibble())
    }
    status <- httr2::resp_status(resp)
    if (status >= 400) {
      body_txt <- try(httr2::resp_body_string(resp), silent = TRUE)
      message("❌ infoprices/list ", status, " (AT=", asset_type, "): ", body_txt)
      return(tibble())
    }
    
    body <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(body, "try-error") || is.null(body$Data)) {
      message("⚠️ infoprices/list empty body (AT=", asset_type, ", nUics=", length(chunk), ")")
      return(tibble())
    }
    
    items <- body$Data
    # Jos Data on yhden instrumentin objekti (sisältää Uic-kentän), kääri listaksi
    if (is.list(items) && !is.null(items$Uic)) items <- list(items)
    if (length(items) == 0) return(tibble())
    
    rows <- purrr::map(items, function(it) {
      f <- try(jsonlite::flatten(it), silent = TRUE)
      if (inherits(f, "try-error")) return(NULL)
      
      # Turvalliset poiminnat (aina pituus 1)
      mid  <- num1(f$Quote.Mid)
      last <- num1(f$PriceInfo.LastTraded)
      bid  <- num1(f$Quote.Bid)
      ask  <- num1(f$Quote.Ask)
      
      # Jos molemmat Bid ja Ask ovat 0 -> tulkitaan "ei hintaa" (NA)
      if (is.finite(bid) && is.finite(ask) && bid == 0 && ask == 0) {
        bid <- NA_real_
        ask <- NA_real_
      }
      
      tibble(
        Uic         = suppressWarnings(as.integer(num1(f$Uic))),
        AssetType   = asset_type,
        Mid         = mid,
        Bid         = bid,
        Ask         = ask,
        LastTraded  = last,
        LastUpdated = as.character(
          if (!is.null(f$LastUpdated) && length(f$LastUpdated)) f$LastUpdated[[1]]
          else if (!is.null(f$Quote.Updated) && length(f$Quote.Updated)) f$Quote.Updated[[1]]
          else NA_character_
        ),
        Currency    = as.character(
          if (!is.null(f$DisplayAndFormat.Currency) && length(f$DisplayAndFormat.Currency)) f$DisplayAndFormat.Currency[[1]]
          else NA_character_
        ),
        Symbol      = as.character(
          if (!is.null(f$DisplayAndFormat.Symbol) && length(f$DisplayAndFormat.Symbol)) f$DisplayAndFormat.Symbol[[1]]
          else NA_character_
        ),
        PriceSource = as.character(
          if (!is.null(f$PriceSource) && length(f$PriceSource)) f$PriceSource[[1]]
          else NA_character_
        )
      )
    })
    
    dplyr::bind_rows(purrr::compact(rows))
  }
  
  chunks <- split(uics, ceiling(seq_along(uics) / 50))  # 50 kpl / kutsu
  dplyr::bind_rows(purrr::map(chunks, fetch_chunk))
}

# --- Julkinen: jobi ------------------------------------------------
# positions_input:
#   - data.frame (positions_df) TAI
#   - NULL -> luetaan uusin Output/Positions_*.csv
fetch_position_prices_job <- function(positions_input = NULL,
                                      write_csv = TRUE) {
  
  # 0) Lataa positions
  positions_df <-
    if (is.null(positions_input)) {
      files <- list.files("Output", pattern="^Positions_\\d{8}_\\d{6}\\.csv$", full.names = TRUE)
      if (!length(files)) {
        message("⚠️ Ei löydy Output/Positions_*.csv — anna positions_input parametrina.")
        return(invisible(tibble()))
      }
      readr::read_csv(tail(sort(files), 1), show_col_types = FALSE)
    } else {
      positions_input
    }
  
  if (!is.data.frame(positions_df) || !nrow(positions_df)) {
    message("ℹ️ Ei positioita — ei haettavia hintoja.")
    return(invisible(tibble()))
  }
  
  # 1) Ryhmittele position instrumentin mukaan (EI underlying!)
  pos_keys <- positions_df %>%
    distinct(Uic, AssetType) %>%
    filter(!is.na(Uic), nzchar(AssetType))
  
  if (!nrow(pos_keys)) {
    message("ℹ️ Ei löydy (Uic, AssetType) -pareja.")
    return(invisible(tibble()))
  }
  
  # 2) AccountKey + Amount
  account_key <- get_account_key()
  
  # 3) Hae per AssetType /list -kutsulla
  results <- pos_keys %>%
    group_by(AssetType) %>%
    group_map(~{
      at  <- .y$AssetType[[1]]
      amt <- default_amount(at)
      message("🔎 Haetaan POSITION-hinnat AssetType=", at, " (", nrow(.x), " UICs, Amount=", amt, ")")
      sx_infoprices_list(
        uics        = .x$Uic,
        asset_type  = at,
        account_key = account_key,
        amount      = amt
      )
    }) %>%
    bind_rows()
  
  if (!nrow(results)) {
    message("⚠️ Position-hintoja ei saatu yhdellekään UIC:lle.")
    return(invisible(tibble()))
  }
  
  # 4) Dedup ja talletus
  out <- results %>%
    filter(is.finite(Uic)) %>%
    group_by(Uic) %>% slice_head(n = 1) %>% ungroup()
  
  if (write_csv && nrow(out)) {
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    path  <- file.path("Output", paste0("Position_Prices_", stamp, ".csv"))
    readr::write_csv(out, path)
    message("✅ Position-hinnat talletettu: ", path)
  } else if (!nrow(out)) {
    message("⚠️ Position-hintoja ei saatu (tyhjä tulos).")
  }
  
  out
}
