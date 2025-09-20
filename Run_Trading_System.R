# ============================================
# Run_Trading_System.R
# Käynnistää Saxo OpenAPI -pipeline-prosessin (modulaarinen)
# ============================================

options(stringsAsFactors = FALSE)

# ── Apu-funktiot ────────────────────────────────────────────────
log <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
need <- function(path) { if (!file.exists(path)) stop("Puuttuu skripti: ", path); path }
safesrc <- function(path) source(need(path), chdir = FALSE)  # <-- nyt ei vaihda hakemistoa

start_time <- Sys.time()
dir.create("Logs", showWarnings = FALSE)
dir.create("Output", showWarnings = FALSE)

# ── Pakolliset moduulit ─────────────────────────────────────────
must_have <- c(
  "env_setup.R",
  "auth_saxo.R",
  "fetch_accounts.R",
  "fetch_positions.R",
  "compute_metrics.R",
  "save_output_csv.R"
)

missing <- must_have[!file.exists(file.path("R", must_have))]
if (length(missing)) {
  stop("❌ Puuttuvat moduulit R/-kansiosta: ", paste(missing, collapse = ", "))
}

log("🚀 Käynnistetään Trading System...")

tryCatch({
  # 1) Ympäristö & auth
  safesrc("R/env_setup.R")
  safesrc("R/auth_saxo.R")          
  safesrc("R/http_helpers.R")       
  
  # 2) Legacy-tuki (jos vanha pipeline löytyy)
  if (file.exists("R/Run_Saxo_Pipeline.R")) {
    log("ℹ️ Legacy-pipeline havaittu (R/Run_Saxo_Pipeline.R) — ajetaan ja jatketaan sen tuloksilla.")
    safesrc("R/Run_Saxo_Pipeline.R")   
    legacy_res <- try(saxo_run_pipeline(), silent = TRUE)
    if (!inherits(legacy_res, "try-error") && is.list(legacy_res)) {
      if (is.null(legacy_res$accounts)) {
        log("⚠️ Legacy pipeline ei palauttanut accounts-dataa; haetaan moduulilla.")
        safesrc("R/fetch_accounts.R")   
      } else {
        accounts_df <- legacy_res$accounts
      }
      if (is.null(legacy_res$positions)) {
        log("⚠️ Legacy pipeline ei palauttanut positions-dataa; haetaan moduulilla.")
        safesrc("R/fetch_positions.R")  
      } else {
        positions_df <- legacy_res$positions
      }
    } else {
      log("⚠️ Legacy-pipeline epäonnistui — haetaan tiedot moduuleilla.")
      safesrc("R/fetch_accounts.R")
      safesrc("R/fetch_positions.R")
    }
  } else {
    # 3) Uusi modulaarinen tapa
    safesrc("R/fetch_accounts.R")     
    safesrc("R/fetch_positions.R")    
  }
  
  # 4) (Valinnainen) quotes/greeks myöhemmin
  # if (file.exists("R/get_quotes.R")) safesrc("R/get_quotes.R")   
  # if (file.exists("R/get_greeks.R")) safesrc("R/get_greeks.R")   
  
  # 5) Tunnusluvut
  safesrc("R/compute_metrics.R")      
  
  # 6) Tallennus CSV
  safesrc("R/save_output_csv.R")
  
  # 7) Yhteenvedot (jos käytössä)
  if (file.exists("R/reporting_summary.R")) safesrc("R/reporting_summary.R")
  
  # Pieni konsoliyhteenveto
  if (exists("accounts_df")) log("📂 Tilirivejä: ", nrow(accounts_df))
  if (exists("positions_df")) log("📂 Positioita: ", nrow(positions_df))
  if (exists("metrics_df"))   log("📊 Metrics-rivejä: ", nrow(metrics_df))
  
  log("✅ Valmis")
}, error = function(e){
  msg <- paste("❌ VIRHE:", conditionMessage(e))
  log(msg)
  writeLines(paste(format(Sys.time()), msg), file.path("Logs","error_last.log"))
  stop(e)
}, finally = {
  dur <- difftime(Sys.time(), start_time, units = "secs")
  log(sprintf("⏱️ Kesto: %.1f s", as.numeric(dur)))
})






