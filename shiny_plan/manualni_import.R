library(tidyverse)
library(readxl)
# export z AISP, vybrat:
  # Popis služby
  # Typ služby
  # Identifikátor úkonu
  # Název úkonu
  # Popis úkonu
  # Fáze úkonu
  # Vykonavatel úkonu
  # Vhodnost digitalizace
  # Typ kanálu
  # Kanál je realizovaný
  # Kanál plánován od
  # Kanál plánován do
  # Kód agendy
  # Název agendy

path <- choose.files()

# služby
df <- read_excel(path)
nazvy <- c("kod.agendy", "nazev.agendy", "id.sluzby", "nazev.sluzby", 
           "popis.sluzby", "typ.sluzby", "ovm", "nazev.ovm", "id.ukonu", 
           "nazev.ukonu", "popis.ukonu", "faze", "digi.vhodny")
sluzby <- df %>% 
  filter(`Úkon - vykonavatel` == "klient") %>% 
  select(- c(`Úkon - vykonavatel`, `Obslužný kanál`, `Existující/plánovaný`, `Plánovaný od`, `Plánovaný do`)) %>%
  set_names("kod.agendy", "nazev.agendy", "id.sluzby", "nazev.sluzby", 
            "typ.sluzby", "popis.sluzby", "id.ukonu", 
            "nazev.ukonu", "popis.ukonu", "digi.vhodny", "faze") %>% 
  mutate(ovm = "", nazev.ovm = "") %>% 
  select(c(kod.agendy, nazev.agendy, id.sluzby, nazev.sluzby, popis.sluzby, typ.sluzby, ovm, nazev.ovm, id.ukonu, nazev.ukonu, popis.ukonu, faze, digi.vhodny)) %>% 
  group_by(kod.agendy, nazev.agendy, id.sluzby, nazev.sluzby, popis.sluzby, typ.sluzby, ovm, nazev.ovm, id.ukonu, nazev.ukonu, popis.ukonu, digi.vhodny) %>% 
  summarise(faze = paste(faze, collapse = "; ")) %>% 
  ungroup()

kanaly <- df %>% 
  filter(`Úkon - vykonavatel` == "klient") %>% 
  select(c(`Úkon - kód`, `Obslužný kanál`, `Existující/plánovaný`, `Plánovaný od`)) %>% 
  set_names(c("id.ukonu", "kanal", "realizovany", "planovany.od")) %>% 
  mutate(
    realizovany = if_else(realizovany == "existující", TRUE, FALSE),
    kanal = case_when(
      kanal == "datová schránka" ~ "DATOVA_SCHRANKA",
      kanal == "dokument opatřený uznávaným elektronickým podpisem" ~ "EL_PODPIS",
      kanal == "osobně" ~ "OSOBNE",
      kanal == "pošta" ~ "POSTA",
      kanal == "ostatní formy dálkového přístupu (např. veřejná datová síť, telefonicky atd.)." ~ "DALKOVY_PRISTUP",
      kanal == "Czech POINT" ~ "CZECH_POINT",
      kanal == "jiný způsob, pokud tak stanoví právní předpis" ~ "JINY_ZPUSOB",
      kanal == "samoobslužný portál (AIS)" ~ "PORTAL",
      )
    )

write_rds(sluzby, "shiny_plan/output/sluzby.rds")
write_rds(kanaly, "shiny_plan/output/kanaly.rds")
