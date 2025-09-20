# fetch_positions.R — hae positiot

.clean_names <- function(x) {
  x <- sub("^PositionBase\\.", "", x)
  x <- sub("^PositionView\\.", "", x)
  x <- sub("^DisplayAndFormat\\.", "", x)
  x
}
.flatten_if_needed <- function(df) {
  if (is.data.frame(df) && any(vapply(df, is.list, logical(1)))) jsonlite::flatten(df) else df
}

fetch_positions <- function(){
  resp <- perform_req(
    build_req("port/v1/positions/me") |>
      httr2::req_url_query(FieldGroups="PositionBase,PositionView,DisplayAndFormat")
  )
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  df   <- .flatten_if_needed(body$Data)
  
  want <- c(
    "NetPositionId","PositionBase.AccountId","PositionBase.AccountKey",
    "PositionBase.Uic","PositionBase.AssetType","PositionBase.Amount",
    "PositionBase.OpenPrice","PositionBase.OpenPriceIncludingCosts",
    "PositionBase.ExecutionTimeOpen","PositionBase.Status",
    "PositionView.CalculationReliability",
    "DisplayAndFormat.Symbol","DisplayAndFormat.Description","DisplayAndFormat.Currency"
  )
  keep <- intersect(want, names(df))
  out  <- if (length(keep)) df[, keep, drop=FALSE] else df
  names(out) <- .clean_names(names(out))
  
  if (!"AverageOpenPrice" %in% names(out) && "OpenPrice" %in% names(out)) {
    out$AverageOpenPrice <- out$OpenPrice
  }
  out
}

positions_df <- tryCatch(fetch_positions(), error=function(e){
  message("⚠️ fetch_positions: ", conditionMessage(e))
  data.frame(Uic=integer(), NetPositionId=character(), Amount=double(),
             AverageOpenPrice=double(), stringsAsFactors=FALSE)
})
