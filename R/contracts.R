# ============================================
# R/contracts.R — joustava skeemavalidointi
# ============================================

suppressPackageStartupMessages(library(checkmate))

# Vähimmäisskeema, joka on realistinen heti fetch_positions() jälkeen
.positions_required <- c(
  "Uic", "AssetType", "Amount", "AverageOpenPrice", "Currency", "UnderlyingUic"
)

# Valinnaiset (tarkistetaan jos löytyvät; ei kaadeta jos puuttuvat)
.positions_optional <- c(
  "NetPositionId", "PositionId", "CurrentPrice", "UnderlyingPrice",
  "Strike", "PutCall", "Expiry",
  "Delta", "Gamma", "Theta", "Vega"
)

positions_schema <- function(df) {
  assert_data_frame(df, min.rows = 0)
  
  # 1) Vaadi vähintään nämä
  assert_names(names(df), must.include = .positions_required)
  
  # 2) Jos optiot: voi tulla Strike/PutCall/Expiry myöhemmin (InstrumentData/Details)
  # 3) Greeks/UnderlyingPrice voivat puuttua ennen rikastusta => vain jos läsnä, tarkista tyyppi
  for (nm in intersect(.positions_optional, names(df))) {
    # kevyt tyyppitsekki jos haluat:
    if (nm %in% c("Amount","AverageOpenPrice","CurrentPrice","UnderlyingPrice",
                  "Delta","Gamma","Theta","Vega","Strike")) {
      assert_numeric(df[[nm]], null.ok = TRUE)
    }
  }
  
  invisible(df)
}

