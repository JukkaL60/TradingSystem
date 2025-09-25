# ============================================
# Run_Trading_System.R (päivitetty)
# Käynnistää Saxo OpenAPI -pipeline-prosessin (modulaarinen)
# - Hakee positions_df
# - Hakee POSITIOIDEN HINNAT muistissa (ei sivupolkuja csv:iin)
# - Liittää hinnat positions_df:ään ja laskee CurrentPrice
# - Laskee metrics
# - Tallentaa outputit vain tarkistusta varten
# ============================================

options(stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(dplyr))

# ── Apu-funktiot ────────────────────────────────────────────────
log <- function(...) cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
need <- function(path) { if (!file.exists(path)) stop("Puuttuu skripti: ", path); path }
safesrc <- function(path) source(need(path), chdir = FALSE)  # ei vaihda hakemistoa

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
  # 1) Ympäristö & auth & HTTP-helperit
  safesrc("R/env_setup.R")
  safesrc("R/auth_saxo.R")
  safesrc("R/http_helpers.R")
  
  # 2) Legacy-tuki (jos vanha pipeline löytyy)
  if (file.exists("R/Run_Saxo_Pipeline.R")) {
    log("ℹ️ Legacy-pipeline havaittu (R/Run_Saxo_Pipeline.R) — ajetaan ja jatketaan sen tuloksilla.")
    safesrc("R/Run_Saxo_Pipeline.R")
    legacy_res <- try(saxo_run_pipeline(), silent = TRUE)
    if (!inherits(legacy_res, "try-error") && is.list(legacy_res)) {
      if (is.null(legacy_res$accounts)) { log("⚠️ Legacy pipeline ei palauttanut accounts-dataa; haetaan moduulilla."); safesrc("R/fetch_accounts.R") } else { accounts_df <- legacy_res$accounts }
      if (is.null(legacy_res$positions)) { log("⚠️ Legacy pipeline ei palauttanut positions-dataa; haetaan moduulilla."); safesrc("R/fetch_positions.R") } else { positions_df <- legacy_res$positions }
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
  
  # 4) POSITIOIDEN HINNAT muistissa (ei csv-sivupolkuja)
  #    - hakee snapshotit /trade/v1/infoprices/list kautta
  #    - säilyttää Extended AssetType -kirjoitusasut (Etf, ContractFutures, StockIndexOption, FuturesOption, ...)
  safesrc("R/fetch_position_prices_job.R")
  safesrc("R/join_position_prices.R")
  
  if (exists("positions_df") && is.data.frame(positions_df) && nrow(positions_df) > 0) {
    log("💹 Haetaan positioiden hinnat muistissa...")
    position_prices_df <- try(fetch_position_prices_job(positions_input = positions_df, write_csv = FALSE), silent = TRUE)
    
    if (!inherits(position_prices_df, "try-error") && is.data.frame(position_prices_df) && nrow(position_prices_df) > 0) {
      positions_priced <- try(join_position_prices_mem(positions_df, position_prices_df, write_csv = FALSE, verbose = TRUE), silent = TRUE)
      if (!inherits(positions_priced, "try-error") && is.data.frame(positions_priced) && nrow(positions_priced) > 0) {
        # Pidä rajapinta muun koodin kanssa: korvaa positions_df rikastetulla versiolla
        positions_df <- positions_priced
        log("✅ Hinnat liitetty: CurrentPrice käytettävissä.")
      } else {
        log("⚠️ Hinta-liitos epäonnistui — jatketaan alkuperäisellä positions_df:llä.")
      }
    } else {
      log("⚠️ Positioiden hintoja ei saatu — jatketaan ilman CurrentPricea.")
    }
  } else {
    log("ℹ️ Ei positioita — ohitetaan hintaosio.")
  }
  
  # 5) Tunnusluvut (compute_metrics käyttää nyt positions_df, jossa CurrentPrice jos saatavilla)
  safesrc("R/compute_metrics.R")
  
  # 6) Tallennus CSV vain auditointiin/tarkistuksiin
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

# ── Underlying-hinnat (VAPAAEHTOINEN, vain jos tarvitset myöhemmin graafeihin/Black-76) ──
#  Jos haluat hakea myös underlying-hinnat muistissa, lisää myöhemmin erillinen ajo:
#  safesrc("R/fetch_underlying_prices_job.R")
#  underlying_prices_df <- fetch_underlying_prices_job(positions_input = positions_df, write_csv = FALSE)
#  # tarvittaessa liitä positions_df:ään erillisellä joinilla (in-memory), esim. UnderlyingUic:lla
