# fetch_accounts.R — hae tilit

.flatten_if_needed <- function(df) {
  if (is.data.frame(df) && any(vapply(df, is.list, logical(1)))) jsonlite::flatten(df) else df
}

fetch_accounts <- function(){
  # Suora REST kutsu
  resp <- perform_req(build_req("port/v1/accounts/me"))
  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  .flatten_if_needed(body$Data)
}

accounts_df <- tryCatch(fetch_accounts(), error=function(e){
  message("⚠️ fetch_accounts: ", conditionMessage(e))
  data.frame(AccountId=character(), AccountKey=character(), stringsAsFactors=FALSE)
})
