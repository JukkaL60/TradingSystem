# ============================================
# test_infoprices.R
# Testaa Saxo OpenAPI infoprices EURUSD spot
# ============================================

library(httr2)
library(jsonlite)

# 1) Ladataan token välimuistista
token_path <- ".auth/saxo_token_sim.rds"
if (!file.exists(token_path)) stop("Token file not found: ", token_path)
token <- readRDS(token_path)

# 2) Määritellään endpoint
base_url <- "https://gateway.saxobank.com/sim/openapi"
endpoint <- "/trade/v1/infoprices"

# EURUSD spot: Uic = 21, AssetType = FxSpot
uic <- 21
asset_type <- "FxSpot"

url <- paste0(base_url, endpoint,
              "?Uic=", uic,
              "&AssetType=", asset_type,
              "&Amount=100000")  # Amount voi olla mikä vain positiivinen luku

# 3) Tee request
resp <- request(url) |>
  req_headers(
    Authorization = paste("Bearer", token$access_token)
  ) |>
  req_perform()

# 4) Parsitaan JSON
data <- resp |>
  resp_body_string() |>
  fromJSON(flatten = TRUE)

print(data)

# Pieni yhteenveto
if (!is.null(data$Quote)) {
  cat("\n✅ EURUSD infoprices OK\n")
  cat("Bid:", data$Quote$Bid, " Ask:", data$Quote$Ask, " Mid:", data$Quote$Mid, "\n")
} else {
  cat("\n⚠️ Ei saatu hintaa EURUSD:lle\n")
}
