library(readxl)
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(carData)
library(sjPlot)
library(effects)
library(forcats)
library(ggthemes)

putni <- read_excel("./IevadesDati/putni.xlsx")

ainavas <- c(100, 200, 310, 610, 710, 720, 800)

putni_dzied_ainava <- data.frame()

# Cikls
for (skaitlis in ainavas) {
  putni_kopa <- putni %>%
    select(speciesname,
           Dzied.Biotopa.attieciba,
           population_method.text,
           paste0("Sugas_", skaitlis), 
           paste0("Putnu_", skaitlis), 
           paste0("Ligzd_Sugas_", skaitlis), 
           paste0("Ligzd_Putnu_", skaitlis)) %>%
    rename(
      Sugas = paste0("Sugas_", skaitlis),
      Putnu = paste0("Putnu_", skaitlis),
      Ligzd_Sugas = paste0("Ligzd_Sugas_", skaitlis),
      Ligzd_Putnu = paste0("Ligzd_Putnu_", skaitlis)
    ) %>%
    mutate(Ainava = skaitlis)
  putni_dzied_ainava <- bind_rows(putni_dzied_ainava, putni_kopa)
}



putni_dzied_ainava$Ainava <- as.factor(putni_dzied_ainava$Ainava)
putni_dzied_ainava$Ainava <- fct_recode(putni_dzied_ainava$Ainava,
                                        "Ceļi / Roads" = "100",
                                        "Ūdeņi / Waters" = "200",
                                        "Aramzeme / Agricultural land" = "310",
                                        "Meži / Forests" = "610",
                                        "Purvi / Marshes" = "710",
                                        "Niedrāji / Reedbeds" = "720",
                                        "Smiltāji / Sandbanks" = "800"
)

ggplot(data = putni_dzied_ainava) +
  #geom_point(aes(x = Sugas, y = Dzied.Biotopa.attieciba, color = "Sugas"), shape = 1, size = 1, alpha = 1, na.rm = TRUE) +
  #geom_point(aes(x = Putnu, y = Dzied.Biotopa.attieciba, color = "Putnu"), shape = 1, size = 1, alpha = 1, na.rm = TRUE) +
  #geom_point(aes(x = Ligzd_Sugas, y = Dzied.Biotopa.attieciba, color = "Ligzd_Sugas"), shape = 2, size = 1, alpha = 1, na.rm = TRUE) +
  geom_point(aes(x = Ligzd_Putnu, y = Dzied.Biotopa.attieciba, color = "Ligzd_Putnu"), shape = 1, size = 1, na.rm = TRUE) +
  
  #geom_smooth(aes(x = Sugas, y = Dzied.Biotopa.attieciba, color = "Sugas tendence"), method = "loess", se = FALSE) +
  #geom_smooth(aes(x = Putnu, y = Dzied.Biotopa.attieciba, color = "Putnu tendence"), method = "loess", se = FALSE) +
  #geom_smooth(aes(x = Ligzd_Sugas, y = Dzied.Biotopa.attieciba, color = "Ligzd_Sugas tendence"), method = "loess", se = FALSE) +
  geom_smooth(aes(x = Ligzd_Putnu, y = Dzied.Biotopa.attieciba, color = "Ligzd_Putnu tendence"), size = 0.5, method = "loess", se = FALSE) +
  
  scale_x_log10(labels = scales::label_log()) +
  scale_y_log10(labels = scales::label_log()) +
  
  facet_wrap(~ Ainava, scales = "free_y", nrow = 2) +  
  labs(x = "Preference ainavas klasē / Preference in landscape class", y = "D/B pazīmju attiecība / D/B trait ratio") +
  
  ggthemes::scale_color_colorblind() +
  
  theme_classic() +
  geom_hline(yintercept = 1, lty = 3) +
  geom_hline(yintercept = 0.1, lty = 3) +
  geom_vline(xintercept = 1, lty = 3) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )




putni_dzied_ainava <- putni_dzied_ainava %>%
  mutate(
    x_statuss = case_when(
      Ligzd_Putnu > 1 ~ "right",
      TRUE ~ "left"
    ),
    y_statuss = case_when(
      Dzied.Biotopa.attieciba > 1 ~ "top",
      Dzied.Biotopa.attieciba < 0.1 ~ "bottom",
      TRUE ~ "middle"
    )
  )

# Skaitām punktus katrā kvadrātā
punktu_skaits_kvadratos <- putni_dzied_ainava %>%
  group_by(Ainava, x_statuss, y_statuss) %>%
  count(name = "punktu_skaits")

# Pievienojam koordinātas, lai novietotu tekstu pareizajos kvadrātos
punktu_skaits_kvadratos <- punktu_skaits_kvadratos %>%
  mutate(
    x_text = case_when(
      x_statuss == "left" ~ 0.025,
      x_statuss == "right" ~ 130,
      TRUE ~ 1
    ),
    y_text = case_when(
      y_statuss == "top" ~ 80,     
      y_statuss == "middle" ~ 0.35,     
      y_statuss == "bottom" ~ 0.01,
      TRUE ~ 1
    )
  )

# Attēlojam datus ar punktu skaitu katrā kvadrātā
ggplot(data = putni_dzied_ainava) +
  geom_point(aes(x = Ligzd_Putnu, y = Dzied.Biotopa.attieciba, 
                 color = "Ligzd_Putnu", shape = population_method.text), 
             size = 1, alpha = 1, na.rm = TRUE) +
  geom_smooth(aes(x = Ligzd_Putnu, y = Dzied.Biotopa.attieciba, color = "Ligzd_Putnu tendence"), 
              size = 0.5, method = "loess", se = FALSE) +
  
  scale_x_log10(labels = scales::label_log()) +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100), labels = scales::label_log()) +
  
  facet_wrap(~ Ainava, scales = "free_y", nrow = 2) +  
  labs(x = "Preference ainavas klasē / Preference in landscape class", 
       y = "D-B pazīmju attiecība / D-B trait ratio") +
  
  ggthemes::scale_color_colorblind() +
  
  scale_shape_manual(values = c("estimateExpert" = 1, 
                                "estimatePartial" = 2,
                                "completeSurvey" = 3),
                     breaks = c("estimateExpert", "estimatePartial", "completeSurvey"), 
                     labels = c("Ekspertu novērtējums\nExpert estimate", "Daļējs novērtējums\nPartial estimate", "Pilnīgs novērtējums\nComplete Survey")) +  # Maina formu nosaukumus
  
  theme_classic() +
  geom_hline(yintercept = 1, lty = 3) +
  geom_hline(yintercept = 0.1, lty = 3) +
  geom_vline(xintercept = 1, lty = 3) +
  
  geom_text(data = punktu_skaits_kvadratos, 
            aes(x = x_text, y = y_text, label = paste("n =", punktu_skaits)), 
            color = "#56B4E9", size = 3, fontface = "bold", inherit.aes = FALSE) +
  
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.background = element_rect(color = "black", fill = "white", size = 0.25) 
  ) +
  guides(color = "none", shape = guide_legend(title = "Datu kvalitātes klase"))


test_results <- list()
for (ainava_group in unique(putni_dzied_ainava$Ainava)) {
  ainava_data <- putni_dzied_ainava %>% filter(Ainava == ainava_group)
  test_results[[as.character(ainava_group)]] <- kruskal.test(Dzied.Biotopa.attieciba ~ Sugas, data = ainava_data)
}

test_results

ggsave(filename = "./Rezultati/Putnu_sugu_akustiska_ainava.jpg", 
       plot = last_plot(),
       height = 1800, 
       width = 3150, 
       dpi = 300, 
       units = "px", 
       device = "jpg")





####




table(putni$population_method.text,useNA = "always")
putni=putni %>% 
  mutate(metode=ifelse(population_method.text=="completeSurvey",0,1)) %>% 
  mutate(akustiski.grupa=case_when(Dzied.Biotopa.attieciba<0.1~1,
                                   Dzied.Biotopa.attieciba<1~2,
                                   Dzied.Biotopa.attieciba>1~3)) %>% 
  mutate(akustiski.grupa=as.factor(akustiski.grupa))
table(putni$metode)
table(putni$akustiski.grupa)






datu_saraksts <- list()

for (skaitlis in ainavas) {
  kol_LP <- paste0("Ligzd_Putnu_", skaitlis)
  
  atlasamie <- c("metode", "akustiski.grupa", kol_LP)
  
  putni_kopa <- putni %>%
    select(all_of(atlasamie))
  
  datu_saraksts[[paste0("dati_", skaitlis)]] <- putni_kopa
  
  formula_str <- as.formula(paste0("metode ~ akustiski.grupa * ", kol_LP))
  modelis <- glm(formula_str, data = putni_kopa, family = binomial(link = "logit"))
  
  assign(paste0("modelis_", skaitlis), modelis, envir = .GlobalEnv)
  
  print(paste("Modelis ainavai:", skaitlis))
  print(summary(modelis))
  
  sjPlot::tab_model(modelis, title = paste("Modelis", skaitlis))
  
  plot(effects::allEffects(modelis), main = paste("Prognoze ainavai", skaitlis))
}



#modelis720=glm(metode~akustiski.grupa*Ligzd_Putnu_720,data=putni,family=binomial(link="logit"))
#summary(modelis720)
#sjPlot::tab_model(modelis720)
#plot(effects::allEffects(modelis720))

#modelis610=glm(metode~akustiski.grupa*Ligzd_Putnu_610,data=putni,family=binomial(link="logit"))
#summary(modelis610)
#sjPlot::tab_model(modelis610)
#plot(effects::allEffects(modelis610))




vektors=c(0.01,0.1,0.5,1,2,10)
prognoze_100=as.data.frame(ggeffects::ggpredict(modelis_100,terms=c("Ligzd_Putnu_100[vektors]","akustiski.grupa")))
prognoze_100$veids="Ceļi / Roads"
prognoze_200=as.data.frame(ggeffects::ggpredict(modelis_200,terms=c("Ligzd_Putnu_200[vektors]","akustiski.grupa")))
prognoze_200$veids="Ūdeņi / Waters"
prognoze_310=as.data.frame(ggeffects::ggpredict(modelis_310,terms=c("Ligzd_Putnu_310[vektors]","akustiski.grupa")))
prognoze_310$veids="Aramzeme / Agricultural land"
prognoze_610=as.data.frame(ggeffects::ggpredict(modelis_610,terms=c("Ligzd_Putnu_610[vektors]","akustiski.grupa")))
prognoze_610$veids="Meži / Forests"
prognoze_710=as.data.frame(ggeffects::ggpredict(modelis_710,terms=c("Ligzd_Putnu_710[vektors]","akustiski.grupa")))
prognoze_710$veids="Purvi / Marshes"
prognoze_720=as.data.frame(ggeffects::ggpredict(modelis_720,terms=c("Ligzd_Putnu_720[vektors]","akustiski.grupa")))
prognoze_720$veids="Niedrāji / Reedbeds"
prognoze_800=as.data.frame(ggeffects::ggpredict(modelis_800,terms=c("Ligzd_Putnu_800[vektors]","akustiski.grupa")))
prognoze_800$veids="Smiltāji / Sandbanks"


visi_prognoze=rbind(prognoze_100,prognoze_200,prognoze_310,prognoze_610,prognoze_710,prognoze_720,prognoze_800)

#ggplot(visi_prognoze,aes(factor(x),predicted,ymin=conf.low,ymax=conf.high,shape=group,col=veids))+
#geom_pointrange(position=position_jitterdodge(jitter.width=0.1,jitter.height = 0))+
#ggthemes::scale_color_colorblind()+
#theme_classic()


visi_prognoze$veids <- factor(visi_prognoze$veids, levels = c("Ceļi / Roads", "Ūdeņi / Waters", "Aramzeme / Agricultural land", "Meži / Forests", "Purvi / Marshes", "Niedrāji / Reedbeds", "Smiltāji / Sandbanks"))

ggplot(visi_prognoze, aes(factor(x), predicted,
                          ymin = conf.low, ymax = conf.high,
                          shape = group, col = veids)) +
  geom_pointrange(position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +  # Krāsu kodi
  theme_classic() +
  labs(x = "Preference ainavas klasē / Preference in landscape class",
       y = "Prognozētā varbūtība / Predicted probability",
       shape = "Akustiskā grupa\nAcoustic group",
       color = "Ainava\nLandscape") +
  facet_wrap(~ veids, nrow = 2) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(filename = "./Rezultati/prognoze.jpg", 
       plot = last_plot(),
       height = 1800, 
       width = 3150, 
       dpi = 300, 
       units = "px", 
       device = "jpg")


sjPlot::tab_model(modelis_100, modelis_200, modelis_310, modelis_610, modelis_710, modelis_720, modelis_800)



####


putni_dzied_ainava$Ainava <- as.factor(putni_dzied_ainava$Ainava)
putni_dzied_ainava_long <- pivot_longer(putni_dzied_ainava, 
                                        cols = c(Sugas, Putnu, Ligzd_Sugas, Ligzd_Putnu),
                                        names_to = "Kolonna", 
                                        values_to = "Vērtība")

ggplot(putni_dzied_ainava_long, aes(x = Kolonna, y = Vērtība, group = speciesname, color = Kolonna)) +
  
  geom_line(aes(group = speciesname), color = "lightgray", size = 0.1) +
  geom_point(size = 0.5) +
  geom_hline(yintercept = 1, lty = 3, size = 0.5) +
  
  scale_color_manual(
    values = c(
      "Sugas" = "#F0E442",
      "Putnu" = "#009E73",
      "Ligzd_Sugas" = "#56B4E9",
      "Ligzd_Putnu" = "#E69F00"
    ),
    labels = c(
      "Visu sugu ietvaros, starp ligzdotājiem\nWithin all species, among nesters",
      "Savas sugas ietvaros, starp ligzdotājiem\nWithin one's own species, among nesters",
      "Visu sugu ietvaros, starp visiem novērojumiem\nWithin all species, among all observations",
      "Savas sugas ietvaros, starp visiem novērojumiem\nWithin one's own species, among all observations"
    )
  ) +
  
  scale_y_log10(
    labels = scales::label_log(),
    breaks = c(0.01, 0.1, 1, 10, 100, 1000),
    limits = c(0.01, NA),
    oob = scales::squish
  ) +
  
  xlab(NULL) +
  ylab("Preference ainavas klasē / Preference in landscape class") +
  
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold", size = 7),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.text = element_text(size = 7.5)
  ) +
  
  facet_wrap(~ Ainava, nrow = 1, scales = "fixed") +
  EnvStats::stat_n_text(vjust = -0.5, size = 2) +
  guides(color = guide_legend(title = NULL))


ggsave(filename = "./Rezultati/Putnu_sugu_ainavu preference_1.jpg", 
       plot = last_plot(),
       height = 1800, 
       width = 3150, 
       dpi = 300, 
       units = "px", 
       device = "jpg")












putni_dzied_ainava$Ainava <- as.factor(putni_dzied_ainava$Ainava)
putni_dzied_ainava_long <- pivot_longer(putni_dzied_ainava, 
                                        cols = c(Sugas, Putnu, Ligzd_Sugas, Ligzd_Putnu),
                                        names_to = "Kolonna", 
                                        values_to = "Vērtība")

colorblind_palette <- colorblind_pal()(8)[2:8]

ggplot(putni_dzied_ainava_long, aes(x = Kolonna, y = Vērtība, fill = Kolonna)) +
  
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, color = "black", alpha = 0.8) +
  
  geom_hline(yintercept = 1, lty = 3, size = 0.5) +
  
  scale_fill_manual(values = colorblind_palette) +
  
  scale_fill_manual(
    values = colorblind_palette,
    labels = c(
      "Visu sugu ietvaros, starp ligzdotājiem\nWithin all species, among nesters",
      "Savas sugas ietvaros, starp ligzdotājiem\nWithin one's own species, among nesters",
      "Visu sugu ietvaros, starp visiem novērojumiem \nWithin all species, among all observations",
      "Savas sugas ietvaros, starp visiem novērojumiem\nWithin one's own species, among all observations"
    )
  ) +
  
  scale_y_log10(
    labels = scales::label_log(),
    breaks = c(0.01, 0.1, 1, 10, 100, 1000)
  ) +
  
  xlab(NULL) +
  ylab("Preference ainavas klasē / Preference in landscape class") +
  
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold", size = 7),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.text = element_text(size = 7.5)
  ) +
  facet_wrap(~ Ainava, nrow = 1, scales = "fixed") +
  EnvStats::stat_n_text(vjust = -0.5, size = 2) +
  guides(fill = guide_legend(title = NULL))




ggsave(filename = "./Rezultati/Putnu_sugu_ainavu preference_2.jpg", 
       plot = last_plot(),
       height = 1800, 
       width = 3150, 
       dpi = 300, 
       units = "px", 
       device = "jpg")