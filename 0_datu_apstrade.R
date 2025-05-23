# 1. Bibliotēku ielāde ----
library(tidyverse)
library(terra)
library(sf)
library(dplyr)
library(writexl)
library(openxlsx)




# 2. Ainavas un novērojumu datu ielāde un apstrāde ----

## 2.1. Ainavas rastra ielāde ----
Sys.setlocale("LC_NUMERIC", "C")
ainava=terra::rast("../IevadesDati/Ainava_vienk_mask.tif")


## 2.2. Novērojumu (GPKG) datu ielāde un pārveidošana ----
st_layers("../IevadesDati/martinam_dabasdati.gpkg")
noverojumi0=read_sf("../IevadesDati/martinam_dabasdati.gpkg",
                    layer="putni_dabasdati_2023")
noverojumi0=st_transform(noverojumi0,crs=st_crs(ainava))




# 3. Novērojumu piesaiste ainavas klasēm ----

## 3.1. Rindu ID pievienošana ----
noverojumi0=noverojumi0 %>% 
  mutate(rinda_ID=rownames(.))


## 3.2. Pikseļu klasifikācijas pievienošana novērojumiem ----
klases=terra::extract(ainava,noverojumi0,method="simple")
klases=klases %>% 
  mutate(rinda_ID=as.character(ID),
         klase=layer) %>% 
  dplyr::select(-ID,-layer)

klases <- klases %>% 
  mutate(klase = case_when(
    klase %in% c(100, 500) ~ 100,
    klase %in% c(310, 320, 330, 410, 420) ~ 310,
    klase %in% c(610, 620, 630, 640) ~ 610,
    klase %in% c(720, 730) ~ 720,
    TRUE ~ klase
  ))

noverojumi0=noverojumi0 %>% 
  left_join(klases,"rinda_ID")


## 3.3. Ainavas klases – pikseļu biežums un relatīvā platība ----
klasu_platibas=terra::freq(ainava)

klasu_platibas <- klasu_platibas %>%
  mutate(value = as.character(value)) %>%
  select(-layer) %>%
  mutate(value = case_when(
    value %in% c("100", "500") ~ "100",
    value %in% c("310", "320", "330", "410", "420") ~ "310",
    value %in% c("610", "620", "630", "640") ~ "610",
    value %in% c("720", "730") ~ "720",
    TRUE ~ value
  )) %>%
  group_by(value) %>%
  summarise(count = sum(count), .groups = "drop")

klasu_platibas=klasu_platibas %>% 
  mutate(kopeja_platiba=sum(count),
         klases_ipatsvars=count/kopeja_platiba)

#klasu_platibas=klasu_platibas %>% dplyr::select(-layer)




# 4. Vides ielāde vai saglabāšana ----
#write.xlsx(klasu_platibas, "klasu_platibas.xlsx", overwrite = TRUE)
#save.image(file = "../IevadesDati/my_environment.RData")
load("../IevadesDati/my_environment.RData")




# 5. Putnu novērojumu datu sagatavošana un preferenču analīze ainavu klasēs ----

## 5.1. “Klases vērtību apvienošana līdzīgās kategorijās ----
noverojumi <- noverojumi0 %>%
  mutate(klase = case_when(
    klase == 500 ~ 100,
    klase == 320 ~ 310,
    klase == 330 ~ 310,
    klase == 410 ~ 310,
    klase == 420 ~ 310,
    klase == 620 ~ 610,
    klase == 630 ~ 610,
    klase == 640 ~ 610,
    klase == 730 ~ 720,
    TRUE ~ klase
  ))


## 5.2. Novērojumu skaita aprēķins dažādos līmeņos ----
noverojumi2=noverojumi %>% 
  group_by(sugaLV,sugaZIN) %>% 
  mutate(novsk_Suga=n()) %>% 
  ungroup() %>% 
  group_by(sugaLV,sugaZIN,klase) %>% 
  mutate(novsk_SugaKlase=n()) %>% 
  ungroup() %>% 
  mutate(novsk_Putni=n()) %>% 
  group_by(klase) %>% 
  mutate(novsk_PutniKlase=n()) %>% 
  ungroup()


## 5.3. Novērojumu apvienošana ar klasēm un klasifikācija ----
klasu_platibas <- klasu_platibas %>%
  mutate(value = as.numeric(value))

noverojumi3 <- noverojumi2 %>%
  left_join(klasu_platibas, by = c("klase" = "value"))

klasu_skaits <- noverojumi3 %>%
  group_by(klase) %>%
  summarise(skaits = n())


## 5.4. Novērojumu statistikas apkopošana un sugu preferenču aprēķins ----
noverojumi_apkopots=data.frame(noverojumi3) %>% 
  group_by(sugaLV,sugaZIN,klase) %>% 
  summarise(klases_platiba=mean(count),
            kopeja_platiba=mean(kopeja_platiba),
            klases_ipatsvars=mean(klases_ipatsvars),
            novsk_SugaKlase=mean(novsk_SugaKlase),
            novsk_Suga=mean(novsk_Suga),
            novsk_PutniKlase=mean(novsk_PutniKlase),
            novsk_Putni=mean(novsk_Putni)) %>% 
  mutate(pref_SugasIetvaros=(novsk_SugaKlase/novsk_Suga)/klases_ipatsvars) %>% 
  mutate(pref_PutnuIetvaros=(novsk_SugaKlase/novsk_Suga)/(novsk_PutniKlase/novsk_Putni)) %>% 
  ungroup() %>% 
  mutate(tips="Visi novērojumi")


## 5.5. Putnu preferenču aprēķins ainavas klasēm kopumā ----
ainavklasem=data.frame(noverojumi3) %>% 
  group_by(klase) %>% 
  summarise(klases_ipatsvars=mean(klases_ipatsvars),
            novsk_PutniKlase=mean(novsk_PutniKlase),
            novsk_Putni=mean(novsk_Putni)) %>% 
  mutate(pref_Putniem=(novsk_PutniKlase/novsk_Putni)/klases_ipatsvars) %>% 
  ungroup() %>% 
  dplyr::select(klase,pref_Putniem) %>% 
  ungroup()
pievienot=data.frame(klase=NA,
                     pref_Putniem=NA)
ainavklasem=rbind(ainavklasem,pievienot)




# 6. Ligzdojošo putnu filtrēšana pēc statusa ----
print(unique(noverojumi3$statuss))

pazimes=c(
  "Ligzda ar mazuļiem (LM)",
  "Ligzda ar olām (LO)",
  "Baro mazuļus (JB)",
  "Apdzīvota ligzda vai dobums (AL)",
  "Lietota ligzda (LL)",
  "Tikko šķīlušies mazuļi vai vecāki ar mazuļiem (RM)",
  "Aizvilinoša vai uzbrūkoša uzvedība (AU)",
  "Gatavo ligzdu (nes ligzdas materiālu) (G)",
  "Uztraukuma uzvedība vai saucieni (U)",
  "Apmeklē iespējamu ligzdas vietu (V)",
  "PāRojas (ligzdotāji) (R)",
  "Pāris ligzdošanai piemērotā biotopā (P)",
  "Teritoriāla uzvedība (ligzdotājam) (T)",
  "Uzturas ligzdošanai piemērotā biotopā (B)",
  "Dzied ligzdošanai piemērotā biotopā (D)",
  "Perēšanas laukums pieaugušam putnam (L)"
)
ligzdotaji = noverojumi3 %>% 
  filter(statuss %in% pazimes)



## 6.1. Vokalizējošo ligzdotāju proporcija starp iespējamajiem ligzdotājiem ----
print(unique(noverojumi3$statuss))
pazimes=c(
  "Uzturas ligzdošanai piemērotā biotopā (B)",
  "Dzied ligzdošanai piemērotā biotopā (D)"
)
ligzdotaji_BD = noverojumi3 %>% 
  filter(statuss %in% pazimes)

pazimes=c(
  "Dzied ligzdošanai piemērotā biotopā (D)"
)
ligzdotaji_D = noverojumi3 %>% 
  filter(statuss %in% pazimes)

ipatsvars = nrow(ligzdotaji_D) / nrow(ligzdotaji_BD)
print(ipatsvars)




## 6.2. Ligzdotāju novērojumu skaita aprēķins dažādos līmeņos ----
ligzdotaji2 = ligzdotaji %>% 
  group_by(sugaLV, sugaZIN) %>% 
  mutate(novsk_Suga = n()) %>% 
  ungroup() %>% 
  group_by(sugaLV, sugaZIN, klase) %>% 
  mutate(novsk_SugaKlase = n()) %>% 
  ungroup() %>% 
  mutate(novsk_Putni = n()) %>% 
  group_by(klase) %>% 
  mutate(novsk_PutniKlase = n()) %>% 
  ungroup()





## 6.3. Vokalizējošo ligzdotāju apkopošana un preferenču aprēķins ----
ligzdotaji2_D = ligzdotaji_D %>% 
  group_by(sugaLV, sugaZIN) %>% 
  mutate(novsk_Suga = n()) %>% 
  ungroup() %>% 
  group_by(sugaLV, sugaZIN, klase) %>% 
  mutate(novsk_SugaKlase = n()) %>% 
  ungroup() %>% 
  mutate(novsk_Putni = n()) %>% 
  group_by(klase) %>% 
  mutate(novsk_PutniKlase = n()) %>% 
  ungroup()


ligzdotaji_apkopots_D=data.frame(ligzdotaji2_D) %>% 
  group_by(sugaLV,sugaZIN,klase) %>% 
  summarise(klases_platiba=mean(count),
            kopeja_platiba=mean(kopeja_platiba),
            klases_ipatsvars=mean(klases_ipatsvars),
            novsk_SugaKlase=mean(novsk_SugaKlase),
            novsk_Suga=mean(novsk_Suga),
            novsk_PutniKlase=mean(novsk_PutniKlase),
            novsk_Putni=mean(novsk_Putni)) %>% 
  mutate(pref_SugasIetvaros=(novsk_SugaKlase/novsk_Suga)/klases_ipatsvars) %>% 
  mutate(pref_PutnuIetvaros=(novsk_SugaKlase/novsk_Suga)/(novsk_PutniKlase/novsk_Putni)) %>% 
  ungroup() %>% 
  mutate(tips="Ligzdotāji") %>% 
  ungroup()




## 6.4. Visiem ligzdotājiem preferenču aprēķins ----
ligzdotaji_apkopots=data.frame(ligzdotaji2) %>% 
  group_by(sugaLV,sugaZIN,klase) %>% 
  summarise(klases_platiba=mean(count),
            kopeja_platiba=mean(kopeja_platiba),
            klases_ipatsvars=mean(klases_ipatsvars),
            novsk_SugaKlase=mean(novsk_SugaKlase),
            novsk_Suga=mean(novsk_Suga),
            novsk_PutniKlase=mean(novsk_PutniKlase),
            novsk_Putni=mean(novsk_Putni)) %>% 
  mutate(pref_SugasIetvaros=(novsk_SugaKlase/novsk_Suga)/klases_ipatsvars) %>% 
  mutate(pref_PutnuIetvaros=(novsk_SugaKlase/novsk_Suga)/(novsk_PutniKlase/novsk_Putni)) %>% 
  ungroup() %>% 
  mutate(tips="Ligzdotāji") %>% 
  ungroup()




## 6.5. Ainavklases ligzdotajiem apvienotas ----
ainavklasem_ligzdotaji = data.frame(ligzdotaji2) %>% 
  #mutate(klase = klasu_aizvietojums(klase)) %>%
  group_by(klase) %>% 
  summarise(klases_ipatsvars = mean(klases_ipatsvars),
            novsk_PutniKlase = mean(novsk_PutniKlase),
            novsk_Putni = mean(novsk_Putni)) %>% 
  mutate(pref_Putniem = (novsk_PutniKlase / novsk_Putni) / klases_ipatsvars) %>% 
  ungroup() %>% 
  dplyr::select(klase, pref_Putniem) %>% 
  ungroup()
pievienot = data.frame(klase = NA,
                       pref_Putniem = NA)
ainavklasem_ligzdotaji = rbind(ainavklasem_ligzdotaji, pievienot)


#grr_visi=ainavklasem %>% 
#  left_join(noverojumi_apkopots,by="klase")

#grr_ligzdotaji=ainavklasem_ligzdotaji %>% 
#  left_join(ligzdotaji_apkopots,"klase")



#Izeksportet preferences
#write.csv(noverojumi_apkopots, "./atlase/noverojumi_apkopots.csv", row.names = FALSE)
#write.csv(ligzdotaji_apkopots, "./atlase/ligzdotaji_apkopots.csv", row.names = FALSE)




## 6.6. Eksportē preferneces ----
# Pārveidojam datus no 'noverojumi_apkopots'
df_sugas <- noverojumi_apkopots %>%
  dplyr::select(sugaLV, sugaZIN, klase, pref_SugasIetvaros) %>%
  pivot_wider(names_from = klase, values_from = pref_SugasIetvaros, values_fill = 0, names_prefix = "Sugas_")

df_putni <- noverojumi_apkopots %>%
  dplyr::select(sugaLV, sugaZIN, klase, pref_PutnuIetvaros) %>%
  pivot_wider(names_from = klase, values_from = pref_PutnuIetvaros, values_fill = 0, names_prefix = "Putnu_")

# Pārveidojam datus no 'ligzdotaji_apkopots'
df_ligzd_sugas <- ligzdotaji_apkopots %>%
  dplyr::select(sugaLV, sugaZIN, klase, pref_SugasIetvaros) %>%
  pivot_wider(names_from = klase, values_from = pref_SugasIetvaros, values_fill = 0, names_prefix = "Ligzd_Sugas_")

df_ligzd_putni <- ligzdotaji_apkopots %>%
  dplyr::select(sugaLV, sugaZIN, klase, pref_PutnuIetvaros) %>%
  pivot_wider(names_from = klase, values_from = pref_PutnuIetvaros, values_fill = 0, names_prefix = "Ligzd_Putnu_")

# Pakāpeniska apvienošana
df_final <- df_sugas %>%
  left_join(df_putni, by = c("sugaLV", "sugaZIN")) %>%
  left_join(df_ligzd_sugas, by = c("sugaLV", "sugaZIN")) %>%
  left_join(df_ligzd_putni, by = c("sugaLV", "sugaZIN"))

# Saglabājam rezultātu kā Excel failu
write_xlsx(df_final, "./IevadesDati/preferences.xlsx")



# 7. Attēlu automatizācija ----
zinatniskie = levels(factor(noverojumi_apkopots$sugaZIN))
dir.create("./Atteli/", showWarnings = FALSE)

for(i in seq_along(zinatniskie)){
  print(i)
  zinatniskais = zinatniskie[i]
  print(zinatniskais)
  
  print("datu atlase all")
  dati_all = noverojumi_apkopots %>% 
    ungroup() %>% 
    filter(sugaZIN == zinatniskais) %>% 
    dplyr::select(sugaLV, sugaZIN, klase, pref_SugasIetvaros, pref_PutnuIetvaros) %>% 
    ungroup()
  
  print("datu atlase breed")
  dati_ligzdotaji = ligzdotaji_apkopots %>% 
    ungroup() %>% 
    filter(sugaZIN == zinatniskais) %>% 
    dplyr::select(sugaLV, sugaZIN, klase, pref_SugasIetvaros, pref_PutnuIetvaros) %>% 
    ungroup()
  
  print("join all")
  grr_visi2=ainavklasem %>% 
    left_join(dati_all,by="klase") %>% 
    ungroup()
  
  print("join breed")
  grr_ligzdotaji2=ainavklasem_ligzdotaji %>% 
    left_join(dati_ligzdotaji,by="klase") %>% 
    ungroup()
  
  
  print("ainavklases")
  dati_all2 = grr_visi2 %>% 
    dplyr::select(sugaLV, sugaZIN, klase, pref_SugasIetvaros, pref_PutnuIetvaros,pref_Putniem) %>% 
    pivot_longer(cols = pref_SugasIetvaros:pref_Putniem,
                 names_to = "veids", values_to = "vertiba") %>% 
    mutate(tips="Visi novērojumi\nAll observations")
  
  dati_ligzdotaji2 = grr_ligzdotaji2 %>% 
    dplyr::select(sugaLV, sugaZIN, klase, pref_SugasIetvaros, pref_PutnuIetvaros,pref_Putniem) %>% 
    pivot_longer(cols = pref_SugasIetvaros:pref_Putniem,
                 names_to = "veids", values_to = "vertiba") %>% 
    mutate(tips="Ligzdotāji\nAmong breeders\n")
  
  print("apvienosana attēlam")
  dati_attelam=rbind(dati_all2,dati_ligzdotaji2)
  pievienot_ligzdotajus=data.frame(sugaLV=NA,
                                   sugaZIN=NA,
                                   klase=NA,
                                   veids="pref_Putniem",
                                   vertiba=NA,
                                   tips="Ligzdotāji\nAmong breeders\n")
  dati_attelam2=rbind(dati_attelam,pievienot_ligzdotajus)
  
  dati_attelam2$klase <- recode(dati_attelam2$klase,
                                "100" = "Mākslīgās virsmas\nBuilt-up",
                                "200" = "Ūdeņi\nWaters",
                                "310" = "Lauksaimniecības\nzemes\nFarmlands\n",
                                "610" = "Meži\nForests",
                                "710" = "Purvi\nBogs",
                                "720" = "Niedrāji\nReedbeds",
                                "800" = "Pārējie\nOthers"
                                )
  
  dati_attelam2$klase <- factor(dati_attelam2$klase,
                                levels = c("Mākslīgās virsmas\nBuilt-up", "Ūdeņi\nWaters", "Lauksaimniecības\nzemes\nFarmlands\n", "Meži\nForests", "Purvi\nBogs", "Niedrāji\nReedbeds", "Pārējie\nOthers"))
  
  
  unique(dati_attelam2$klase)
  
  
  print("virsrakstam")
  lielais_n = max(noverojumi_apkopots$novsk_Suga[noverojumi_apkopots$sugaZIN == zinatniskais])
  lielais_n=ifelse(is.infinite(lielais_n),0,lielais_n)
  mazais_n=max(ligzdotaji_apkopots$novsk_Suga[ligzdotaji_apkopots$sugaZIN == zinatniskais])
  mazais_n=ifelse(is.infinite(mazais_n),0,mazais_n)
  sugaLV_text = levels(factor(dati_attelam$sugaLV))
  sugaZIN_text = levels(factor(dati_attelam$sugaZIN))
  
  virsraksts = substitute(expr = paste(sugaLV_text, " ", italic(sugaZIN_text), " (N=", skaits1, ", n=", skaits2,")"),
                          env = base::list(sugaLV_text = sugaLV_text,
                                           sugaZIN_text = sugaZIN_text,
                                           skaits1 = lielais_n,
                                           skaits2=mazais_n))
  
  print("attēls")
  attels = ggplot(dati_attelam2, aes(factor(klase), vertiba, col = veids,shape=tips)) +
    geom_hline(yintercept = 1, lty = 3) +
    geom_point(position = position_jitterdodge(jitter.width = 0.01, jitter.height = 0), size = 2) +
    theme_classic() +
    scale_y_log10() +
    scale_shape_manual("Grupa / Group", values = c("Visi novērojumi\nAll observations" = 16, "Ligzdotāji\nAmong breeders\n" = 17),drop=FALSE) + 
    scale_color_manual("Preference",
                       values = c("pref_Putniem" = "grey",
                                  "pref_SugasIetvaros" = "#C80000",
                                  "pref_PutnuIetvaros" = "#0045C8"),
                       labels = c("Visi putnu novērojumi\nAll bird sightings\n",
                                  "Visu sugu ietvaros\nWithin all species\n",
                                  "Savas sugas ietvaros\nWithin one's own species"
                       )) +
    theme(panel.grid.major.x = element_line(color = "grey", linewidth = 0.1)) +
    labs(x = "Ainavas klase / Landscape class",
         y = "Preference",
         title = virsraksts)
  
  print(attels)
  
  print("saglabāšana")
  zinatniskais_clean = gsub("/", "_", zinatniskais)
  
  faila_nosaukums = paste0("./Atteli/", zinatniskais_clean, ".png")
  ggsave(attels, filename = faila_nosaukums, height = 1800, width = 2700, dpi = 300, units = "px", device = "png")
}
