# ============================================
# R/fetch_underlying_prices.R
# Hakee UnderlyingPrice per UnderlyingUic varman päälle (GET /trade/v1/infoprices)
# ============================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(jsonlite); library(httr2)
})

# Pieni coalesce-apuri listarakenteille
`%||%` <- function(a,b) if (!is.null(a)) a else b

fetch_underlying_prices <- function(positions_df, throttle_ms = 0) {
  # 0) Guardit
  if (!is.data.frame(positions_df) || nrow(positions_df) == 0) {
    return(tibble(UnderlyingUic = integer(), UnderlyingPrice = numeric()))
  }
  
  # 1) Varmista avainsarakkeet (johda UnderlyingUic/UnderlyingAssetType jos puuttuu)
  pos <- positions_df %>%
    mutate(
      UnderlyingUic = if (!"UnderlyingUic" %in% names(.)) Uic else UnderlyingUic,
      UnderlyingAssetType = if (!"UnderlyingAssetType" %in% names(.)) AssetType else UnderlyingAssetType
    )
  
  u_tbl <- pos %>%
    distinct(UnderlyingUic, UnderlyingAssetType) %>%
    filter(!is.na(UnderlyingUic))
  
  if (nrow(u_tbl) == 0) {
    return(tibble(UnderlyingUic = integer(), UnderlyingPrice = numeric()))
  }
  
  # 2) Hae yksi kerrallaan GET /trade/v1/infoprices
  fetch_one <- function(uic, atype) {
    q <- list(
      Uic = as.integer(uic),
      FieldGroups = "Quote,PriceInfo,DisplayAndFormat"
    )
    # AssetType ei ole aina pakollinen; jos tiedossa, lähetetään mukaan
    if (!is.null(atype) && !is.na(atype) && nzchar(atype)) {
      q$AssetType <- atype
    }
    
    # Tee pyyntö käyttäen teidän http-helperia
    resp <- try(
      perform_req(
        build_req("trade/v1/infoprices") |>
          req_url_query(!!!q)
      ),
      silent = TRUE
    )
    if (inherits(resp, "try-error")) return(NULL)
    
    body <- try(resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(body, "try-error") || is.null(body$Data)) return(NULL)
    
    dat <- try(jsonlite::flatten(body$Data), silent = TRUE)
    if (inherits(dat, "try-error") || !is.list(dat)) return(NULL)
    
    # Poimi UIC ja hinta usealla fallbackilla
    # polut voivat olla sekä list- että flat-muotoisia → käytä %||%
    instr <- dat$Instrument %||% list()
    quote <- dat$Quote %||% list()
    pinfo <- dat$PriceInfo %||% list()
    
    this_uic <- as.integer(instr$Uic %||% dat$Uic %||% uic)
    
    # hinta: Mid → LastTraded → (Ask+Bid)/2
    mid  <- suppressWarnings(as.numeric(quote$Mid %||% dat$Quote.Mid))
    last <- suppressWarnings(as.numeric(pinfo$LastTraded %||% dat$PriceInfo.LastTraded))
    ask  <- suppressWarnings(as.numeric(quote$Ask %||% dat$Quote.Ask))
    bid  <- suppressWarnings(as.numeric(quote$Bid %||% dat$Quote.Bid))
    ab   <- suppressWarnings(as.numeric(mean(c(ask, bid), na.rm = TRUE)))
    
    price <- dplyr::coalesce(mid, last, if (is.finite(ab)) ab else NA_real_)
    if (!is.finite(price)) return(NULL)
    
    tibble(UnderlyingUic = this_uic, UnderlyingPrice = price)
  }
  
  # 3) Iteroi ja kerää
  res <- map2(u_tbl$UnderlyingUic, u_tbl$UnderlyingAssetType, fetch_one)
  # throttling (tarvittaessa Saxon rajoitusten vuoksi)
  if (throttle_ms > 0L) Sys.sleep(throttle_ms / 1000)
  
  out <- bind_rows(compact(res)) %>%
    distinct(UnderlyingUic, .keep_all = TRUE)
  
  # Varmuuden vuoksi numeeriseksi
  out$UnderlyingUic   <- suppressWarnings(as.integer(out$UnderlyingUic))
  out$UnderlyingPrice <- suppressWarnings(as.numeric(out$UnderlyingPrice))
  
  out
}
