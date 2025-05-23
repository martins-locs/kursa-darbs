# 1. Bibliotēku ielāde ----
library(readxl)
library(plyr)
library(dplyr)
library(openxlsx)
library(XML)
library(stringr)
library(tidyr)
library(psych)



# 2. Datu iegūšana no direktīvas XML ----

xml_data <- xmlParse("./IevadesDati/LV_birds_reports_20191030-151740.xml")  
xml_list <- xmlToList(xml_data)
xml_list <- xml_list[names(xml_list) == "bird_report"]
xml_dati <- ldply(xml_list, function(x) as.data.frame(t(unlist(x)), stringsAsFactors = FALSE))

xml_dati <- xml_dati %>%
  mutate(speciesname = trimws(speciesname),
         speciesname = str_replace(speciesname, " all others$", ""),
         speciesname = str_replace(speciesname, " s\\. str\\.$", ""),
         speciesname = sapply(str_split(speciesname, " "), function(x) {
           if (length(x) == 3) paste(x[1], x[2]) else paste(x, collapse = " ")
         }))

write.xlsx(xml_dati, "./IevadesDati/LV_birds_reports_2019.xlsx")




# 3. Datu apvienošana ----

## 3.1. Datu ielāde ----
putni_direktiva <- read_excel("./IevadesDati/LV_birds_reports_2019.xlsx")
putni_dabasdati <- read_excel("./IevadesDati/putni_SkaitiPazimes_kops2016.xlsx", .name_repair = "universal")


putni_dabasdati <- putni_dabasdati %>%
  mutate(sugaZIN = trimws(sugaZIN),
         sugaZIN = str_replace(sugaZIN, " f. domestica", ""),
         sugaZIN = sapply(str_split(sugaZIN, " "), function(x) {
           if (length(x) == 3) paste(x[1], x[2]) else paste(x, collapse = " ")
         }))

putni_dabasdati <- putni_dabasdati %>%
  filter(!str_detect(sugaLV, " sp\\.") & !str_detect(sugaLV, "/") &
           !str_detect(sugaZIN, " sp\\.") & !str_detect(sugaZIN, "/"))


## 3.2. Populācijas vērtību apstrāde un aprēķins
putni_direktiva$population_size_min <- as.numeric(putni_direktiva$population_size_min)
putni_direktiva$population_size_max <- as.numeric(putni_direktiva$population_size_max)
putni_direktiva$population_size <- as.numeric(putni_direktiva$population_size)

putni_direktiva <- putni_direktiva %>%
  mutate(
    population_size_min = ifelse(population_size_min == 0, 0.1, population_size_min),
    population_size_max = ifelse(population_size_max == 0, 0.1, population_size_max)
  )

putni_direktiva$population_size <- ifelse(
  is.na(putni_direktiva$population_size) | putni_direktiva$population_size == 0,
  round(
    mapply(function(x, y) geometric.mean(c(x, y), na.rm = TRUE),
           putni_direktiva$population_size_min,
           putni_direktiva$population_size_max),
    0
  ),
  putni_direktiva$population_size
)


## 3.3. Pārsauc kolonnu nosaukumus un izrēķina dziedātāju attiecību ----
putni_dabasdati <- putni_dabasdati %>%
  rename_with(~ c("common_speciesname", "speciesname")[match(.x, c("sugaLV", "sugaZIN"))], 
              .cols = c("sugaLV", "sugaZIN"))

putni_dabasdati <- putni_dabasdati %>%
  mutate(NoverojumaVeids = case_when(
    SkaitsBiotopa == 0 & SkaitsDzied == 0 ~ NA_character_,
    SkaitsBiotopa > 0 & (SkaitsDzied / SkaitsBiotopa) > 1 ~ "Pārsvarā audiāli",
    SkaitsBiotopa > 0 & (SkaitsDzied / SkaitsBiotopa) > 0 ~ "Vizuāli un audiāli",
    SkaitsBiotopa > 0 & SkaitsDzied == 0 ~ "Tikai vizuāli",
    SkaitsBiotopa == 0 & SkaitsDzied > 0 ~ "Tikai audiāli",
    TRUE ~ NA_character_
  ))

putni_dabasdati$Dzied.Biotopa.attieciba <- ifelse(
  putni_dabasdati$SkaitsBiotopa == 0, NA, 
  putni_dabasdati$SkaitsDzied / putni_dabasdati$SkaitsBiotopa
)

putni_dabasdati$Dzied.Biotopa.attieciba <- gsub(",", ".", putni_dabasdati$Dzied.Biotopa.attieciba)
putni_dabasdati$Dzied.Biotopa.attieciba <- as.numeric(putni_dabasdati$Dzied.Biotopa.attieciba)


## 3.4. Pārbauda un apvieno duplikātus, kā prioritāti ņemot ligzdotājus ----
cat("Dublikātu skaits putni_direktiva (common_speciesname):", sum(duplicated(putni_direktiva$common_speciesname)), "\n")
cat("Dublikātu skaits putni_dabasdati (common_speciesname):", sum(duplicated(putni_dabasdati$common_speciesname)), "\n")

cat("Dublikātu skaits putni_direktiva (speciesname):", sum(duplicated(putni_direktiva$speciesname)), "\n")
cat("Dublikātu skaits putni_dabasdati (speciesname):", sum(duplicated(putni_dabasdati$speciesname)), "\n")

putni_direktiva_clean <- putni_direktiva %>%
  group_by(common_speciesname, speciesname) %>%  # Grupējam pēc sugas nosaukumiem
  filter(season.text == "B" | !any(season.text == "B")) %>%  
  slice(1) %>%  # Ja joprojām paliek vairākas rindas, izvēlamies pirmo
  ungroup()


## 3.5. Monitoringa nepilnību atlase ----
putni_direktiva_clean <- putni_direktiva_clean %>%
  mutate(nepilnigie_monitoringi = ifelse(population_method.text %in% c("estimateExpert", "estimatePartial"), 1, 0)) %>%
  relocate(nepilnigie_monitoringi, .after = population_method.text)


## 3.6. Datu apvienošana ----
apvienots_1 <- putni_dabasdati %>%
  left_join(putni_direktiva_clean, by = c("common_speciesname", "speciesname"))

apvienots_1 <- apvienots_1 %>%
  select(everything(), SkaitsVisasPazimes, SkaitsDzied, SkaitsBiotopa, NoverojumaVeids)




# 4. Ielādē preferences un pārsauc kolonnu nosaukumus ----
preferences <- read_excel("./IevadesDati/preferences.xlsx", .name_repair = "universal")

preferences <- preferences %>%
  mutate(sugaZIN = trimws(sugaZIN),
         sugaZIN = str_replace(sugaZIN, " f. domestica", ""),
         sugaZIN = sapply(str_split(sugaZIN, " "), function(x) {
           if (length(x) == 3) paste(x[1], x[2]) else paste(x, collapse = " ")
         }))

preferences <- preferences %>%
  filter(!str_detect(sugaLV, " sp\\.") & !str_detect(sugaLV, "/") &
           !str_detect(sugaZIN, " sp\\.") & !str_detect(sugaZIN, "/"))


## 4.1. Kolonu nosaukumu pārsaukšana ----
preferences <- preferences %>%
  rename_with(~ c("common_speciesname", "speciesname")[match(.x, c("sugaLV", "sugaZIN"))], 
              .cols = c("sugaLV", "sugaZIN"))

cols_to_duplicate <- setdiff(names(preferences), c("common_speciesname", "speciesname"))


## 4.2. Preferenču apvienošana ar esošo tabulu ----
apvienots_2 <- apvienots_1 %>%
  full_join(preferences, by = c("common_speciesname", "speciesname"))

apvienots_2 <- apvienots_2 %>%
  filter(!str_detect(speciesname, "sp\\.|/"))




# 5. Rezultātu saglabāšana ----
#write.xlsx(apvienots_2, "./IevadesDati/putni.xlsx", overwrite = TRUE)