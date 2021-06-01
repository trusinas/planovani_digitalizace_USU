# definice zámku s možností filtrování
zamek.s.filtrem <- function(workbook, sheet) {
  protectWorksheet(workbook, sheet, lockInsertingColumns = TRUE, lockInsertingRows = TRUE, lockDeletingColumns = TRUE, lockDeletingRows = TRUE,
                   lockFormattingCells = FALSE, lockFormattingColumns = FALSE, lockFormattingRows = FALSE,
                   lockAutoFilter = FALSE, lockSorting = FALSE)
}

# definice formátu pro list se seznamem úkonů nedostupných v jednotlivých digitálních kanálech
format.u.chybi <- function(workbook, sheet) {
  # definice stylu pro použití
  header <- createStyle(textDecoration = "bold")

  # formátování
  addStyle(workbook, sheet, header, rows=1, cols=1:8)
  setColWidths(workbook, sheet, cols=c(2,4,6,8), widths = 35) # nadefinování šířky sloupců - široké
  setColWidths(workbook, sheet, cols=c(1,3,5), widths = 9) # nadefinování šířky sloupců - úzké
  setColWidths(workbook, sheet, cols=c(7), widths = 15) # nadefinování šířky sloupců - mezi
  freezePane(workbook, sheet, firstRow = T) # ukotvení 1. řádku 
}