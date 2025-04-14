# 1. Bibliotēku ielāde ----
library(readxl)
library(ggplot2)
library(patchwork)




# 2. Datu ielāde ----
putni <- read_excel("./IevadesDati/putni.xlsx")

putni %>%
  distinct(population_trend_method.text)

putni$Dzied.Biotopa.attieciba <- putni$Dzied.Biotopa.attieciba + 1e-5

putni <- putni[, c("Dzied.Biotopa.attieciba", "population_trend_method.text", 
                   "population_estimate_type.text", "population_trend.text", 
                   "population_trend_long.text", "population_size")]

putni <- putni[!(is.na(putni$Dzied.Biotopa.attieciba) & 
                   is.na(putni$population_trend_method.text) & 
                   is.na(putni$population_estimate_type.text) & 
                   is.na(putni$population_trend.text) & 
                   is.na(putni$population_trend_long.text)), ]




# 3. Grafiks Nr. 1 ----
putni$population_trend_method.text <- factor(
  putni$population_trend_method.text,
  levels = c("absentData","estimateExpert", "estimatePartial", "completeSurvey")
)

grafiks_1 <- ggplot(na.omit(putni[, c("population_trend_method.text", "Dzied.Biotopa.attieciba")]), 
                    aes(x = population_trend_method.text, y = Dzied.Biotopa.attieciba, fill = population_trend_method.text)) +
  geom_hline(yintercept = 1, lty = 3, color = "grey", linewidth = 0.5) + 
  geom_violin(alpha = 0.5) + 
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000), labels = scales::label_log()) +
  scale_x_discrete(labels = c(
    estimateExpert = "Ekspertu novērtējums\nExpert estimate\n",
    estimatePartial = "Daļējs novērtējums\nPartial estimate",
    completeSurvey = "Pilnīgs novērtējums\nComplete Survey",
    absentData = "Datu iztrūkst\nAbsent Data"
  )) +
  scale_fill_manual(values = c(
    absentData = "#B0B0B0",
    estimateExpert = "#A49A8E",
    estimatePartial = "#7F5A3C",
    completeSurvey = "#5E4B3C"
  )) +
  xlab("Populācijas īstermiņa pārmaiņu datu kvalitātes klase\nQuality class of data for short-term population changes") +
  ylab("D-B pazīmju attiecība / D-B trait ratio") +
  ggtitle("A") +
  theme_classic() +
  EnvStats::stat_n_text() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

print(grafiks_1)

kruskal_test <- kruskal.test(Dzied.Biotopa.attieciba ~ population_trend_method.text, data = putni)
kruskal_test
# Rezultātā ir statistiski nozīmīgas atšķirības starp Datu kvalitātes klasēm.

knitr::kable(rstatix::dunn_test(Dzied.Biotopa.attieciba ~ population_trend_method.text, data = putni))




# 4. Grafiks Nr. 2 ----
putni$population_estimate_type.text <- factor(
  putni$population_estimate_type.text,
  levels = c("Minimum", "estimate", "interval")
)

grafiks_2 <- ggplot(na.omit(putni[, c("population_estimate_type.text", "Dzied.Biotopa.attieciba")]), 
                    aes(x = population_estimate_type.text, y = Dzied.Biotopa.attieciba, fill = population_estimate_type.text)) +
  geom_hline(yintercept = 1, lty = 3, color = "grey", linewidth = 0.5) + 
  geom_violin(alpha = 0.5) + 
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
  scale_y_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000),
    labels = scales::label_log()
  ) +
  scale_x_discrete(labels = c(
    Minimum = "Minimālā vērtība\nMinimum value\n",
    estimate = "Aptuvenais novērtējums\nEstimate",
    interval = "95% ticamības intervāls\n95% confidence interval"
  )) +
  scale_fill_manual(values = c(
    Minimum = "#C1BEB7",
    estimate = "#A49382",
    interval = "#7A5F48"
  )) +
  xlab("Populāciju lieluma aprēķina veids\nPopulation size calculation method") +
  ylab("D-B pazīmju attiecība / D-B trait ratio") +
  ggtitle("B") +
  theme_classic() +
  EnvStats::stat_n_text() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

print(grafiks_2)

kruskal_test <- kruskal.test(Dzied.Biotopa.attieciba ~ population_estimate_type.text, data = putni)
kruskal_test
# Rezultātā ir statistiski nozīmīgas atšķirības starp Populāciju aprēķinu veidiem.

knitr::kable(rstatix::dunn_test(Dzied.Biotopa.attieciba ~ population_estimate_type.text, data = putni))




# 5. Grafiks Nr. 3 ----
putni$population_trend_method.text <- factor(
  putni$population_trend_method.text,
  levels = c("absentData","estimateExpert", "estimatePartial", "completeSurvey")
)

grafiks_3 <- ggplot(na.omit(putni[, c("population_trend_method.text", "population_size")]), 
                    aes(x = population_trend_method.text, y = population_size, fill = population_trend_method.text)) +
  geom_violin(alpha = 0.5) + 
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000), labels = scales::label_log()) +
  scale_x_discrete(labels = c(
    estimateExpert = "Ekspertu novērtējums\nExpert estimate\n",
    estimatePartial = "Daļējs novērtējums\nPartial estimate",
    completeSurvey = "Pilnīgs novērtējums\nComplete Survey",
    absentData = "Datu iztrūkst\nAbsent Data"
  )) +
  scale_fill_manual(values = c(
    absentData = "#B0B0B0",
    estimateExpert = "#A49A8E",
    estimatePartial = "#7F5A3C",
    completeSurvey = "#5E4B3C"
  )) +
  xlab("Populācijas īstermiņa pārmaiņu datu kvalitātes klase\nQuality class of data for short-term population changes") +
  ylab("Populācijas lielums / Population size") +
  ggtitle("A") +
  theme_classic() +
  EnvStats::stat_n_text(vjust = -0.5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

print(grafiks_3)

kruskal_test <- kruskal.test(population_size ~ population_trend_method.text, data = putni)
kruskal_test
# Rezultātā ir statistiski nozīmīgas atšķirības starp Datu kvalitātes klasēm atkarībā no populācijas lieluma.

knitr::kable(rstatix::dunn_test(population_size ~ population_trend_method.text, data = putni))




# 6. Grafiks Nr. 4 ----
putni$population_estimate_type.text <- factor(
  putni$population_estimate_type.text,
  levels = c("Minimum", "estimate", "interval")
)

grafiks_4 <- ggplot(na.omit(putni[, c("population_estimate_type.text", "population_size")]), 
                    aes(x = population_estimate_type.text, y = population_size, fill = population_estimate_type.text)) +
  geom_violin(alpha = 0.5) + 
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000), labels = scales::label_log()) +
  scale_x_discrete(labels = c(
    Minimum = "Minimālā vērtība\nMinimum value\n",
    estimate = "Aptuvenais novērtējums\nEstimate",
    interval = "95% ticamības intervāls\n95% confidence interval"
  )) +
  scale_fill_manual(values = c(
    Minimum = "#C1BEB7",
    estimate = "#A49382",
    interval = "#7A5F48"
  )) +
  xlab("Populāciju lieluma aprēķina veids\nPopulation size calculation method") +
  ylab("Populācijas lielums / Population size") +
  ggtitle("B") +
  theme_classic() +
  EnvStats::stat_n_text(vjust = -0.5) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )

print(grafiks_4)

kruskal_test <- kruskal.test(population_size ~ population_estimate_type.text, data = putni)
kruskal_test
# Rezultātā ir statistiski nozīmīgas atšķirības starp Populāciju aprēķinu veidiem atkarībā no populācijas lieluma.

knitr::kable(rstatix::dunn_test(population_size ~ population_estimate_type.text, data = putni))




# 7. Attēlu saglabāšana ----
grafiks_1 | grafiks_2

ggsave(filename = "./Rezultati/Putnu_sugu_akustiska_kvalitate.jpg", 
       plot = last_plot(),
       height = 1800, 
       width = 3150, 
       dpi = 300, 
       units = "px", 
       device = "jpg")


 

grafiks_3 | grafiks_4

ggsave(filename = "./Rezultati/Putnu_populacija_monitorings.jpg", 
       plot = last_plot(),
       height = 1800, 
       width = 3150, 
       dpi = 300, 
       units = "px", 
       device = "jpg")