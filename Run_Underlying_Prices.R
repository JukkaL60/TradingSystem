# ============================================
# Run_Underlying_Prices.R
# Ajaa vain underlying-hintojen jobin ja tallentaa CSV:ksi
# ============================================

options(stringsAsFactors = FALSE)

log <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
need <- function(path) { if (!file.exists(path)) stop("Puuttuu skripti: ", path); path }
safesrc <- function(path) source(need(path), chdir = FALSE)

dir.create("Logs", showWarnings = FALSE)
dir.create("Output", showWarnings = FALSE)

log("🔧 Ajetaan Underlying-hintojen jobi...")

safesrc("R/env_setup.R")
safesrc("R/auth_saxo.R")
safesrc("R/http_helpers.R")
safesrc("R/fetch_positions.R")              # jos haluat käyttää tuoreita positioita
safesrc("R/fetch_underlying_prices_job.R")

# Käyttö 1: jos positions_df on muistissa (esim. äsken ajettu pää-run)
if (exists("positions_df") && is.data.frame(positions_df) && nrow(positions_df) > 0) {
  fetch_underlying_prices_job(positions_input = positions_df, write_csv = TRUE)
} else {
  # Käyttö 2: lue viimeisin Positions_*.csv Outputista
  fetch_underlying_prices_job(positions_input = NULL, write_csv = TRUE)
}

log("✅ Underlying-hintojen jobi valmis.")
