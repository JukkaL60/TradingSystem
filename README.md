# TradingSystem

TradingSystem on henkilökohtainen R-pohjainen järjestelmä Saxo OpenAPI:n kautta tapahtuvaan kaupankäynnin ja positioiden seurantaan.\
Projektia ei ole tarkoitettu jaettavaksi, vaan se toimii vain omassa käytössä.

------------------------------------------------------------------------

## Kansiorakenne
TradingSystem/
├─ R/ # Moduulit (R-skriptit)
│ ├─ env_setup.R
│ ├─ auth_saxo.R
│ ├─ saxo_auth_live.R
│ ├─ http_helpers.R
│ ├─ fetch_accounts.R
│ ├─ fetch_positions.R
│ ├─ compute_metrics.R
│ ├─ save_output_csv.R
│ └─ (mahd. lisämoduulit: get_quotes.R, reporting_summary.R)
│
├─ Output/ # Ajon tulosteet (CSV, metrics, raportit)
├─ Logs/ # Virhelokit ja ajohistoria
├─ Run_Trading_System.R # Pääajuri (käynnistää koko pipeline-prosessin)
├─ TradingSystem.Rproj # RStudio-projektitiedosto
└─ README.md # Tämä tiedosto


---

## Käyttö

1. **Varmista ympäristömuuttujat**  
   `.Renviron` sijaitsee OneDrivessa:
C:/Users/jukka/OneDrive/Tiedostot/.Renviron

Siellä määritetään Saxo-rajapinnan asetukset:
```ini
SAXO_ENV=sim
SAXO_CLIENT_ID=...
SAXO_CLIENT_SECRET=...
SAXO_REDIRECT_URI=http://localhost:1410/saxoapp

Aja järjestelmä
RStudiossa:

source("Run_Trading_System.R")


→ avaa selaimen kirjautumista varten (ensimmäisellä kerralla).
→ hakee tilit ja positiot Saxo OpenAPI:sta.
→ laskee tunnusluvut ja tallentaa tulokset Output/-kansioon.

Tulosteet

Tilit: Accounts_YYYYMMDD_HHMMSS.csv

Positiot: Positions_YYYYMMDD_HHMMSS.csv

Metrics: Positions_Metrics_YYYYMMDD_HHMMSS.csv

Virheet: Logs/error_last.log

Huomioita

.auth/ sisältää väliaikaiset tokenit (ei jaettavaksi).

Output/ ja Logs/ sisältävät vain ajon tuloksia → voidaan tyhjentää turvallisesti.

Projektia ei käytetä GitHubissa versionhallintaan (vain paikalliseen käyttöön + OneDrive varmuuskopiointiin).

Seuraavat askeleet

Mahdollinen moduuli: get_quotes.R (markkinahintojen haku)

Mahdollinen moduuli: reporting_summary.R (yhteenvedot, esim. graafit)

Power BI / Excel -kytkentä, jos raportointia halutaan automatisoida
