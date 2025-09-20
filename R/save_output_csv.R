# save_output_csv.R — tallettaa tulokset CSV:ksi juuren Output/-kansioon

# Luo juuritason Output/-kansio varmasti
outdir <- file.path(getwd(), "Output")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Tallenna accounts
if (exists("accounts_df")) {
  fn_acc <- file.path(outdir, paste0("Accounts_", timestamp, ".csv"))
  readr::write_csv(accounts_df, fn_acc)
  message("📄 Tilit talletettu: ", fn_acc)
} else {
  message("ℹ️ accounts_df puuttuu — ei tilitiedostoja")
}

# Tallenna positions
if (exists("positions_df")) {
  fn_pos <- file.path(outdir, paste0("Positions_", timestamp, ".csv"))
  readr::write_csv(positions_df, fn_pos)
  message("📄 Positiot talletettu: ", fn_pos)
} else {
  message("ℹ️ positions_df puuttuu — ei positioita")
}

# Tallenna metrics
if (exists("metrics_df")) {
  fn_met <- file.path(outdir, paste0("Positions_Metrics_", timestamp, ".csv"))
  readr::write_csv(metrics_df, fn_met)
  message("✅ Metrics talletettu: ", fn_met)
} else {
  message("ℹ️ metrics_df puuttuu — ei metrics-tiedostoa")
}
