library(tidyverse)
library(jsonlite)

# vytvoření adresářů
if(!dir.exists("output")) dir.create("output")
if(!dir.exists("data")) dir.create("data")

# stažení dat
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/sluzby.json", "data/sluzby.json")
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/agendy.json", "data/agendy.json")
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/ovm.json", "data/ovm.json")
download.file("https://rpp-opendata.egon.gov.cz/odrpp/datovasada/kategorie_ovm.json", "data/kategorie_ovm.json")

# agendy - zpracování
agendy <- read_json("data/agendy.json")
agendy <- agendy$položky
agendy <- tibble(agendy = agendy)
agendy <- agendy %>% 
  hoist(agendy, kod.agendy = "kód",
        nazev.agendy = c("název", "cs"),
        usu = "ohlašovatel"
  ) %>% 
  select(-agendy)
agendy <- agendy %>% 
  mutate(
    usu = str_remove(usu, "orgán-veřejné-moci/")
  )

# agendy - uložení
write_rds(agendy, "output/agendy.rds")

# názvy OVM a KO - zpracování
ovm <- read_json("data/ovm.json")
ovm <- ovm$položky
ovm <- tibble(ovm = ovm)
ovm <- ovm %>% 
  hoist(ovm, id.ovm = "identifikátor",
        nazev.ovm = c("název", "cs")) %>% 
  select(-ovm)

ko <- read_json("data/kategorie_ovm.json")
ko <- ko$položky
ko <- tibble(ko = ko)
ko <- ko %>% 
  hoist(ko, id.ovm = "identifikátor",
        nazev.ovm = c("název", "cs")) %>% 
  select(-ko)
ovm <- ovm %>% 
  rbind(ko)

# názvy OVM a KO - uložení
write_rds(ovm, "output/ovm.rds")

# zpracování služeb
sluzby <- read_json("data/sluzby.json")
sluzby <- sluzby$položky
sluzby <- tibble(sluzby = sluzby)
sluzby <- sluzby %>% 
  hoist(sluzby, id.sluzby = "identifikátor",
        nazev.sluzby = c("název", "cs"),
        popis.sluzby = c("popis", "cs"),
        typ.sluzby = "typ-služby",
        kod.agendy = "agenda",
        ukony = "úkony",
        mistni.prislusnost = "místní-příslušnost"
  ) %>% 
  select(-sluzby)

sluzby <- sluzby %>% 
  unnest_longer(mistni.prislusnost) %>% 
  unnest_wider(mistni.prislusnost)
sluzby <- sluzby %>% 
  rename(subjekt = 'typ-subjektu', mistni.prislusnost = 'typ-místní-příslušnosti', ovm = 'poskytovatel-k-místní-příslušnosti',
         pusobnost = 'typ-působnosti') %>% 
  select(-c(subjekt, mistni.prislusnost, pusobnost)) %>% 
  distinct()

sluzby <- sluzby %>% 
  mutate(
    typ.sluzby = str_remove(typ.sluzby, "typ-služby/"),
    kod.agendy = str_remove(kod.agendy, "agenda/"),
    ovm = str_remove(ovm, "kategorie-ovm/|orgán-veřejné-moci/")
  ) 
sluzby.ovm <- sluzby %>% 
  select(id.sluzby, ovm) %>% 
  distinct()
  
sluzby <- sluzby %>% 
  select(-ovm) %>% 
  distinct() %>% 
  unnest_longer(ukony) %>% 
  hoist(ukony,
        id.ukonu = "identifikátor-úkonu",
        nazev.ukonu = c("název-úkonu", "cs"),
        popis.ukonu = c("popis-úkonu", "cs"),
        vykonavatel = "typ-vykonavatele-úkonu",
        faze = "fáze",
        digi.vhodny = "vhodný-k-digitalizaci",
        kanaly = "kanály"
        ) %>% 
  select(-ukony) %>% 
  filter(vykonavatel == "typ-vykonavatele-úkonu/KLIENT")
ukony.faze <- sluzby %>% 
  select(id.ukonu, faze) %>% 
  distinct() %>% 
  unnest_longer(faze) %>% 
  mutate(faze = str_remove(faze, "fáze-úkonu/")) %>% 
  group_by(id.ukonu) %>% 
  summarise(faze = paste(faze, collapse = "; ")) %>% 
  ungroup()
sluzby <- sluzby %>% 
  select(-faze) %>% 
  left_join(ukony.faze, by = "id.ukonu")
rm(ukony.faze)

# Obslužný kanál - všechny dohromady
kanaly <- sluzby %>% 
  select(id.ukonu, kanaly) %>% 
  distinct() %>% 
  unnest_longer(kanaly) %>% 
  unnest_wider(kanaly) %>% 
  select(id.ukonu, kanal = 'typ-kanálu') %>% 
  mutate(kanal = str_remove(kanal, "typ-obslužného-kanálu/")) %>% 
  group_by(id.ukonu) %>% 
  summarise(kanal = paste(kanal, collapse = "; "))
sluzby <- sluzby %>% 
  select(-kanaly) %>% 
  left_join(kanaly, by = "id.ukonu")
rm(kanaly)

sluzby <- sluzby %>% 
  left_join(agendy, by = "kod.agendy")

sluzby.ovm <- sluzby.ovm %>% 
  left_join(ovm, by = c("ovm" = "id.ovm")) %>% 
  group_by(id.sluzby) %>% 
  summarise(
    nazev.ovm = paste(nazev.ovm, collapse = "; "),
    ovm = paste(ovm, collapse = "; ")
  )
sluzby <- sluzby %>% 
  left_join(sluzby.ovm, by = "id.sluzby")
rm(agendy, ko, ovm, sluzby.ovm)

# TODO: Požadované sloupce
  ## Služba VS - kód
  ## Služba VS - název
  ## Služba VS - typ služby
  ## Služba VS - popis
  # Služba VS - poskytovatel OVM - všichni dohromady
  # Úkon - kód
  # Úkon - název
  # Úkon - popis
  # Úkon - fáze
  # Obslužný kanál - všechny dohromady
  # počet ročně
  # úspora minut - úředník
  # úspora minut - klient
  # úspora materiál - úředník
  # úspora materiál - klient
sluzby <- sluzby %>% 
  select(kod.agendy, nazev.agendy, id.sluzby, nazev.sluzby, popis.sluzby, typ.sluzby, ovm, nazev.ovm,
         id.ukonu, nazev.ukonu, popis.ukonu, faze, digi.vhodny, kanal)
write_rds(sluzby, "output/sluzby.rds")
