source(file.path("R","saxo_openapi_live.R"))
saxo_auth_live()   # selain sanoo “Authentication complete…”
acc <- saxo_get_accounts()
str(acc, max.level = 1)
