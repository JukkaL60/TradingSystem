# ============================================
# Run_Position_Prices.R
# Ajaa vain position-hintojen jobin ja tallentaa CSV:ksi
# ============================================

options(stringsAsFactors = FALSE)

log <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
need <- function(path) { if (!file.exists(path)) stop("Puuttuu skripti: ", path); path }
safesrc <- function(path) source(need(path), chdir = FALSE)

dir.create("Logs", showWarnings = FALSE)
dir.create("Output", showWarnings = FALSE)

safesrc("R/env_setup.R")
safesrc("R/auth_saxo.R")
safesrc("R/http_helpers.R")
safesrc("R/fetch_position_prices_job.R")

log("🔧 Ajetaan position-hintojen jobi...")
if (exists("positions_df") && is.data.frame(positions_df) && nrow(positions_df) > 0) {
  fetch_position_prices_job(positions_input = positions_df, write_csv = TRUE)
} else {
  fetch_position_prices_job(positions_input = NULL, write_csv = TRUE)
}
log("✅ Position-hintojen jobi valmis.")
