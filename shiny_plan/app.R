library(shiny)
library(dplyr)
library(readr)
library(jsonlite)
library(glue)
library(openxlsx)
library(tidyr)
library(stringr)

# ETL
    # vytvoří DIR data a output
    # stáhnout JSONy
    # zpracovat a uložit jako RDS
# APP
    # výběr ÚSÚ a agendy (v obou př. lze více)
    # aktualizace dat - GitHub action
    # tlačítko pro vygenerování a stažení XLSX

source("R/func.R", encoding = "UTF-8")

# načtení dat
sluzby <- read_rds("output/sluzby.rds")
ovm <- read_rds("output/ovm.rds")
agendy <- read_rds("output/agendy.rds")
kanaly <- read_rds("output/kanaly.rds")

# info - postup
info <- read.delim("data/navod.txt", header = F, encoding = "UTF-8")

# seznam agend se službami
agendy <- agendy %>% 
    filter(kod.agendy %in% unique(sluzby$kod.agendy)) %>% 
    mutate(vyber = paste(kod.agendy, nazev.agendy, sep = " - "))

# seznam ÚSÚ
agendy <- agendy %>% 
    left_join(ovm, by = c("usu" = "id.ovm"))

# názvy agendy
names(agendy$kod.agendy) <- agendy$vyber

sluzby <- sluzby %>% 
    mutate(
        pocet.rocne = "",
        'uspora.minut - urednik' = "",
        'uspora.minut - klient' = "",
        'uspora.material - urednik' = "",
        'uspora.material - klient' = ""
    )
# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Aplikace pro vytvoření podkladů pro plánování digitalizace"),
    
    # Main panel for displaying outputs ----
    mainPanel(
        uiOutput("select.usu"),
        uiOutput("select.agenda"),
        
        # Output: Download a file ----
        downloadButton('downloadFile', 'Zprocesuj & stáhni', class= "action"),
        
        
        h3("Vládou schválený plán"),
        tags$a(href="https://pma3.gov.cz/uploads/doc/Aktualizovana-podoba-planu-digitalizace-.xlsx", "Plán digitalizace schválený vládou 20.9.2021 usnesením č. 826"),
        h4("Novinky"),
        p("Aktualizace parametrů za rok 2021"),
        p("Přidán plán digitalizace pro agendu (agendy)"),
        p("Do exportovaného excelu přidán seznam úkonů, které nejsou dostupné v jednotlivých digitálních kanálech.")
    )
    
)

# Define server logic
server <- function(input, output) {
    # Input: Výběr ÚSÚ ----
    output$select.usu <- renderUI({
        selectInput("usu", "1) Vyber ohlašovatele (ÚSÚ)", unique(agendy$nazev.ovm), selected = "Ministerstvo vnitra",
                    multiple = FALSE, selectize = TRUE, width = "600px", size = NULL)
    })
    # Input: Výběr agend ----
    output$select.agenda <- renderUI({
        req(input$usu)
        selectInput("agenda", "2) Vyber agendu (lze více)", agendy %>% filter(nazev.ovm == input$usu) %>% pull(kod.agendy), selected = NULL,
                    multiple = TRUE, selectize = TRUE, width = "600px", size = NULL)
    })
    
    # zpracování tabulky
    vybrane.sluzby <-reactive({
        req(input$usu)
        sluzby <- sluzby %>% 
            filter(kod.agendy %in% input$agenda)
    })
    processed <-reactive({
        
        # vytvoření listů v souboru
        wb <- createWorkbook()
        addWorksheet(wb, "info")
        addWorksheet(wb, "seznam úkonů")
        addWorksheet(wb, "návratnost")
        addWorksheet(wb, "chybí datová schránka")
        addWorksheet(wb, "chybí uz. el. podpis")
        addWorksheet(wb, "chybí portál")
        addWorksheet(wb, "plán digitalizace")
        
       
        # základ pro vzorec
        l <- nrow(vybrane.sluzby())
        l <- l + 1
        sluzby <- vybrane.sluzby() %>% 
            mutate(
                'přínosy úředník' = "",
                'přínosy klient' = ""
            )
        # realizované kanály
        realiz.kanaly <- kanaly %>% 
            filter(realizovany == TRUE) %>% 
            select(id.ukonu, kanal) %>% 
            group_by(id.ukonu) %>% 
            summarise(kanal = paste(kanal, collapse = "; "))
        sluzby <- sluzby %>% 
            left_join(realiz.kanaly, by = "id.ukonu") %>% 
            relocate(kanal, .after = digi.vhodny)
        
        # text vzorce
        vzorec <- tibble(x1 = c("návratnost (let)", "iniciální investiční výdaj (Kč)", "roční přínosy (Kč)", 
                                "roční provozní výdaje (Kč)", "", "roční přínosy pro VS (Kč)", 
                                "průměrná mzda ve VS (Kč)", "průměrná mzda ve VS na minutu (Kč)", 
                                "", "roční přínosy pro klienty (Kč)", "hrubý domácí produkt na obyvatele (Kč)", 
                                "HDP na obyvatele na minutu (Kč)"),
                         x2 = c("", 30000000, "", 6000000, "", "", 43232, "", "", "", 559534, ""))
        vzorec$x2 <- as.integer(vzorec$x2)
        
        # vyplnění dat do specifických listů
        writeData(wb, "info", x = info, colNames = FALSE)
        writeData(wb, "seznam úkonů", x = sluzby, colNames = TRUE, withFilter = T) # na první řádek přidán autom. filtr
        writeData(wb, "návratnost", x = vzorec, colNames = FALSE)
        
        # přínosy úředník
        writeFormula(wb, sheet = 2, x = glue("=IFERROR(O{2:l}*(P{2:l}*'návratnost'!B8+R{2:l}),0)"), startCol = 20, startRow = 2)
        # přínosy klient
        writeFormula(wb, sheet = 2, x = glue("=IFERROR(O{2:l}*(Q{2:l}*'návratnost'!B12+S{2:l}),0)"), startCol = 21, startRow = 2)
        
        # návratnost
        writeFormula(wb, sheet = 3, x = '=B2/(B6+B10-B4)', startCol = 2, startRow = 1)
        writeFormula(wb, sheet = 3, x = '=B6+B10', startCol = 2, startRow = 3)
        writeFormula(wb, sheet = 3, x = "=SUBTOTAL(9, 'seznam úkonů'!T:T)", startCol = 2, startRow = 6)
        writeFormula(wb, sheet = 3, x = "=2*B7/173.92/60", startCol = 2, startRow = 8)
        writeFormula(wb, sheet = 3, x = "=SUBTOTAL(9, 'seznam úkonů'!U:U)", startCol = 2, startRow = 10)
        writeFormula(wb, sheet = 3, x = "=B11/2008/60", startCol = 2, startRow = 12)
        
        # nadefinování stylů buněk pro list info
        navod.text <- createStyle(fgFill = "white")
        zalamovat <- createStyle(wrapText = T)
        
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
        setColWidths(wb, 1, cols = 1, widths = 117) # nadefinování šířky sloupců - široké
        addStyle(wb, 1, navod.text, rows = 2:15, cols = 1, stack = TRUE) # pozadí textu
        addStyle(wb, 1, zalamovat, rows = 2:15, cols = 1, stack = TRUE) # zalamování textu
        addStyle(wb, 1, header, rows = c(5, 12, 15), cols = 1, gridExpand = FALSE, stack = TRUE)
        
        # přidání stylů, formátování - návratnost
        addStyle(wb, 3, vyplnit, rows=c(2, 4), col=2, gridExpand = F)
        addStyle(wb, 3, nevyplnovat, rows = c(1, 3, 6:8, 10:12), cols = 2, gridExpand = F)
        addStyle(wb, 3, header.navratnost, rows=1, cols = 1:2, gridExpand = T)
        setColWidths(wb, 3, cols = 1, widths = 34)
        
        #zamknutí buněk - list 1
        protectWorksheet(wb, 1, lockInsertingColumns = TRUE, lockInsertingRows = TRUE, lockDeletingColumns = TRUE, lockDeletingRows = TRUE,
                         lockFormattingCells = FALSE, lockFormattingColumns = FALSE, lockFormattingRows = FALSE)
        
        #zamknutí buněk - list 2
        zamek.s.filtrem(wb, 2)
        addStyle(wb, 2, style = createStyle(locked = F, fgFill = "yellow1"), rows = 2:l, cols = 15:19, gridExpand = T)
        
        #zamknutí buněk - list 3
        protectWorksheet(wb, 3, lockInsertingColumns = TRUE, lockInsertingRows = TRUE, lockDeletingColumns = TRUE, lockDeletingRows = TRUE,
                         lockFormattingCells = FALSE, lockFormattingColumns = FALSE, lockFormattingRows = FALSE)
        addStyle(wb, 3, style = createStyle(locked = F, fgFill = "yellow1"), rows = c(2,4), cols = 2, gridExpand = F)
        
        # TODO: formátování čísel
        # úkony
        # návratnost
        
        # TODO: přejmenování sloupců - ke službám přidat názvy
        
        # přidání seznamu úkonů - chybějící kanály
        # zohlednění i plánovaných kanálů
        vsechny.kanaly <- kanaly %>% 
            select(id.ukonu, kanal) %>% 
            group_by(id.ukonu) %>% 
            summarise(kanal = paste(kanal, collapse = "; "))
        sluzby.i.plan <- vybrane.sluzby() %>% 
            left_join(vsechny.kanaly, by = "id.ukonu")
        # bez DS
        bez.ds <- sluzby.i.plan %>% 
            filter(!str_detect(kanal, "DATOVA_SCHRANKA")) %>% 
            select(kod.agendy, nazev.agendy, id.sluzby, nazev.sluzby, id.ukonu, nazev.ukonu, digi.vhodny, kanal)
        
        # bez podpisu
        bez.uep <- sluzby.i.plan %>% 
            filter(!str_detect(kanal, "EL_PODPIS")) %>% 
            select(kod.agendy, nazev.agendy, id.sluzby, nazev.sluzby, id.ukonu, nazev.ukonu, digi.vhodny, kanal)
        
        # bez portálu
        bez.portalu <- sluzby.i.plan %>% 
            filter(!str_detect(kanal, "PORTAL")) %>% 
            select(kod.agendy, nazev.agendy, id.sluzby, nazev.sluzby, id.ukonu, nazev.ukonu, digi.vhodny, kanal)
        
        # bez datovky
        writeData(wb, "chybí datová schránka", x = bez.ds, colNames = TRUE, withFilter = T)
        
        # bez el. podpisu
        writeData(wb, "chybí uz. el. podpis", x = bez.uep, colNames = TRUE, withFilter = T)
       
         # bez portálu
        writeData(wb, "chybí portál", x = bez.portalu, colNames = TRUE, withFilter = T)
        
        # přidání stylů, formátování - chybějící kanály
        format.u.chybi(wb, 4)
        format.u.chybi(wb, 5)
        format.u.chybi(wb, 6)
        
        #zamknutí buněk - listy s chybějícími kanály
        zamek.s.filtrem(wb, 4)
        zamek.s.filtrem(wb, 5)
        zamek.s.filtrem(wb, 6)
        
        # plán za agendu
        # plánované kanály
        planovane.kanaly <- kanaly %>% 
            filter(realizovany == FALSE) %>% 
            select(id.ukonu, kanal, planovany.od)
        planovane <- vybrane.sluzby() %>% 
            left_join(planovane.kanaly, by = "id.ukonu") %>% 
            select(-c(ovm, nazev.ovm, pocet.rocne, 'uspora.minut - urednik', 'uspora.minut - klient', 'uspora.material - urednik', 'uspora.material - klient')) %>% 
            filter(!is.na(kanal) | !is.na(planovany.od))
        writeData(wb, "plán digitalizace", x = planovane, colNames = TRUE, withFilter = T)
        
        # formátování
        setColWidths(wb, 7, cols=c(2,4,5,8,9), widths = 35) # nadefinování šířky sloupců - široké
        setColWidths(wb, 7, cols=c(1,3,6,7,10,11), widths = 9) # nadefinování šířky sloupců - úzké
        setColWidths(wb, 7, cols=c(12), widths = 17) # nadefinování šířky sloupců - mezi
        freezePane(wb, 7, firstRow = T) # ukotvení 1. řádku
        addStyle(wb,7, header, rows=1, cols=1:13)
        
        # QUESTION: jaká data v tabulce?
            # typ služby
            # vykonavatel úkonu
            # úkony již na portále
        # QUESTION: jaké sloupce v tabulce?
        
        # nevhodné k digitalizaci
        return(wb)
        
    })    
    # Download handler in Server
    output$downloadFile <- downloadHandler(
        filename = function() paste0("ukony_", paste(input$agenda, collapse = "_"), ".xlsx"),
        content = function(file) {
            
            # uložení
            saveWorkbook(processed(), file, overwrite = T)
            output$done <- renderText({
                ifelse(!is.null(processed),"Hotovo","Nastala chyba :(")
            })
        }
    )
 
}

# Run the application 
shinyApp(ui = ui, server = server)
