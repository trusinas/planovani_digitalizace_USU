# deploy app
tmp.enc <- options()$encoding
options(encoding = "UTF-8")
wd <- getwd()
setwd(paste0(wd, "/shiny_plan"))
rsconnect::deployApp(appFileManifest = "data/fileManifest.txt", appName = "plan_digitalizace", account = "egov", forceUpdate = TRUE)
options(encoding = tmp.enc)
setwd(wd)
