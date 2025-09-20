# env_setup.R — paketit ja ympäristö

# 1) Varmista paketit
quiet_lib <- function(p){ 
  if(!requireNamespace(p, quietly=TRUE)) {
    message("ℹ️ Paketti puuttuu: ", p, " — asenna install.packages('", p, "')")
  }
}
invisible(lapply(c("httr2","jsonlite","dplyr","readr","lubridate","stringr"), quiet_lib))

options(stringsAsFactors = FALSE)

# 2) Lue oma .Renviron OneDrivesta (ei mene GitHubiin)
my_envfile <- "C:/Users/jukka/OneDrive/Tiedostot/.Renviron"
if (file.exists(my_envfile)) {
  message("🔐 Ladataan ympäristömuuttujat tiedostosta: ", my_envfile)
  readRenviron(my_envfile)
} else {
  message("ℹ️ OneDrive .Renviron-tiedostoa ei löytynyt, käytetään oletuksia / muuta .Renvironia")
}

# 3) Aseta Saxo-ympäristö
TRADING_ENV <- Sys.getenv("TRADING_ENV", unset = "sim")

saxo_env <- list(
  sim  = list(base = "https://gateway.saxobank.com/sim/openapi/"),
  live = list(base = "https://gateway.saxobank.com/openapi/")
)

.SAXO_BASE <- saxo_env[[TRADING_ENV]][["base"]]
message("🌐 Saxo ympäristö: ", TRADING_ENV, " (base = ", .SAXO_BASE, ")")

# 4) Varmista että kriittiset muuttujat löytyvät
mask <- function(x) {
  if (nchar(x) <= 8) return(x)
  paste0(substr(x,1,6), "...", substr(x,nchar(x)-3, nchar(x)))
}

cid <- Sys.getenv("SAXO_CLIENT_ID")
csc <- Sys.getenv("SAXO_CLIENT_SECRET")
red <- Sys.getenv("SAXO_REDIRECT_URI")

if (cid == "" || csc == "" || red == "") {
  warning("⚠️ Yksi tai useampi Saxo-auth muuttuja puuttuu! (SAXO_CLIENT_ID / SAXO_CLIENT_SECRET / SAXO_REDIRECT_URI)")
} else {
  message("✅ Auth-muuttujat ladattu:")
  message("   SAXO_CLIENT_ID     = ", mask(cid))
  message("   SAXO_CLIENT_SECRET = ", mask(csc))
  message("   SAXO_REDIRECT_URI  = ", red)
}
