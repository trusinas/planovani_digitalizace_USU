library(tidyverse)
library(jsonlite)
library(glue)
library(openxlsx)

# požadované agendy
vyber <- c("A117", "A121")

# TODO: výběr ÚSÚ
# TODO: výběr agend se službami

# stažení dat
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/sluzby.json", "data/sluzby.json")
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/agendy.json", "data/agendy.json")
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/ovm.json", "data/ovm.json")
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/kategorie_ovm.json", "data/kategorie_ovm.json")

# zpracování dat
source("R/etl.R", encoding = "UTF-8")
# sluzby.bak <- sluzby
# sluzby <- sluzby.bak

# vytvoření listů v souboru
wb <- createWorkbook()
addWorksheet(wb, "info")
addWorksheet(wb, "seznam úkonů")
addWorksheet(wb, "návratnost")

# TODO: info
info <- tibble(x1 = c("Informace, jak používat tabulku",
                      "Postup"))

# základ pro vzorec
l <- nrow(sluzby)
l <- l + 1
sluzby <- sluzby %>% 
  mutate(
    'přínosy úředník' = "",
    'přínosy klient' = ""
  )
# text vzorce
vzorec <- tibble(x1 = c("návratnost (let)", "iniciální investiční výdaj (Kč)", "roční přínosy (Kč)", 
                   "roční provozní výdaje (Kč)", "", "roční přínosy pro VS (Kč)", 
                   "průměrná mzda ve VS (Kč)", "průměrná mzda ve VS na minutu (Kč)", 
                   "", "roční přínosy pro klienty (Kč)", "hrubý domácí produkt na obyvatele (Kč)", 
                   "HDP na obyvatele na minutu (Kč)"),
                 x2 = c("", 30000000, "", 6000000, "", "", 40572, "", "", "", 524867, ""))
vzorec$x2 <- as.integer(vzorec$x2)

# vyplnění dat do specifických listů
writeData(wb, "info", x = info, colNames = FALSE)
writeData(wb, "seznam úkonů", x = sluzby, colNames = TRUE, withFilter = T) # na první řádek přidán autom. filtr
writeData(wb, "návratnost", x = vzorec, colNames = FALSE)

# přínosy úředník
writeFormula(wb, sheet = 2, x = glue("=O{2:l}*(P{2:l}*'návratnost'!B8+R{2:l})"), startCol = 20, startRow = 2)
# přínosy klient
writeFormula(wb, sheet = 2, x = glue("=O{2:l}*(P{2:l}*'návratnost'!B12+S{2:l})"), startCol = 21, startRow = 2)

# návratnost
writeFormula(wb, sheet = 3, x = '=B2/(B6+B10-B4)', startCol = 2, startRow = 1)
writeFormula(wb, sheet = 3, x = '=B6+B10', startCol = 2, startRow = 3)
writeFormula(wb, sheet = 3, x = "=SUM('seznam úkonů'!T:T)", startCol = 2, startRow = 6)
writeFormula(wb, sheet = 3, x = "=2*B7/173.92/60", startCol = 2, startRow = 8)
writeFormula(wb, sheet = 3, x = "=SUM('seznam úkonů'!U:U)", startCol = 2, startRow = 10)
writeFormula(wb, sheet = 3, x = "=B11/2008/60", startCol = 2, startRow = 12)

# nadefinování stylů buněk pro list seznam úkonů
header <- createStyle(textDecoration = "bold")
header.navratnost <- createStyle(textDecoration = "bold", fontSize = 18, border="bottom", borderStyle = "medium")
vyplnit <- createStyle(fgFill="yellow1")
nevyplnovat <- createStyle(fgFill = "gray")

# přidání stylů, formátování - úkony
addStyle(wb,2, header, rows=1, cols=1:21)
addStyle(wb, 2, vyplnit, rows=2:l, col=15:19, gridExpand = T)
addStyle(wb, 2, nevyplnovat, rows = 2:l, cols = 20:21, gridExpand = T)
setColWidths(wb, 2, cols=c(2,4,5,8,10,11,14), widths = 35) # nadefinování šířky sloupců - široké
setColWidths(wb, 2, cols=c(1,3,6,7,9,12,13), widths = 9) # nadefinování šířky sloupců - úzké
setColWidths(wb, 2, cols=c(15, 20:21), widths = 15) # nadefinování šířky sloupců - mezi
setColWidths(wb, 2, cols=c(16:19), widths = 20) # nadefinování šířky sloupců - mezi
freezePane(wb, 2, firstRow = T) # ukotvení 1. řádku 

# přidání stylů, formátování - info
addStyle(wb, 1, header.navratnost, rows=1, cols = 1, gridExpand = F)
setColWidths(wb, 1, cols = 1, widths = 50) # nadefinování šířky sloupců - široké

# přidání stylů, formátování - návratnost
addStyle(wb, 3, vyplnit, rows=c(2, 4), col=2, gridExpand = F)
addStyle(wb, 3, nevyplnovat, rows = c(1, 3, 6:8, 10:12), cols = 2, gridExpand = F)
addStyle(wb, 3, header.navratnost, rows=1, cols = 1:2, gridExpand = T)
setColWidths(wb, 3, cols = 1, widths = 34)

#zamknutí buněk - list 1
protectWorksheet(wb, 1, lockInsertingColumns = TRUE, lockInsertingRows = TRUE, lockDeletingColumns = TRUE, lockDeletingRows = TRUE,
                 lockFormattingCells = FALSE, lockFormattingColumns = FALSE, lockFormattingRows = FALSE)

#zamknutí buněk - list 2
protectWorksheet(wb, 2, lockInsertingColumns = TRUE, lockInsertingRows = TRUE, lockDeletingColumns = TRUE, lockDeletingRows = TRUE,
                 lockFormattingCells = FALSE, lockFormattingColumns = FALSE, lockFormattingRows = FALSE,
                 lockAutoFilter = FALSE, lockSorting = FALSE)
addStyle(wb, 2, style = createStyle(locked = F, fgFill = "yellow1"), rows = 2:9, cols = 15:19, gridExpand = T)

#zamknutí buněk - list 3
protectWorksheet(wb, 3, lockInsertingColumns = TRUE, lockInsertingRows = TRUE, lockDeletingColumns = TRUE, lockDeletingRows = TRUE,
                 lockFormattingCells = FALSE, lockFormattingColumns = FALSE, lockFormattingRows = FALSE)
addStyle(wb, 3, style = createStyle(locked = F, fgFill = "yellow1"), rows = c(2,4), cols = 2, gridExpand = F)

# TODO: formátování čísel
  # úkony
  # návratnost

# TODO: přejmenování sloupců - ke službám přidat názvy

# TODO: přidání seznamu problematických úkonů
  # bez datovky
  # bez el. podpisu
  # bez portálu

# QUESTION: vypustit úkony již na portále?
  # nevhodné k digitalizaci

# uložení
saveWorkbook(wb, file = paste0("output/ukony_", paste(vyber, collapse = "_"), ".xlsx"), overwrite = T)

