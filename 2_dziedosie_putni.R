library(readxl)
library(ggplot2)
library(patchwork)


putni <- read_excel("putni.xlsx")

putni$Dzied.Biotopa.attieciba <- putni$Dzied.Biotopa.attieciba + 1e-5

putni <- putni[, c("Dzied.Biotopa.attieciba", "population_method.text", 
                   "population_estimate_type.text", "population_trend.text", 
                   "population_trend_long.text", "population_size")]

putni <- putni[!(is.na(putni$Dzied.Biotopa.attieciba) & 
                   is.na(putni$population_method.text) & 
                   is.na(putni$population_estimate_type.text) & 
                   is.na(putni$population_trend.text) & 
                   is.na(putni$population_trend_long.text)), ]


# 1. Grafiks ----

putni$population_method.text <- factor(
  putni$population_method.text,
  levels = c("estimateExpert", "estimatePartial", "completeSurvey")
)

grafiks_1 <- ggplot(na.omit(putni[, c("population_method.text", "Dzied.Biotopa.attieciba")]), 
                    aes(x = population_method.text, y = Dzied.Biotopa.attieciba)) +
  geom_hline(yintercept = 1, lty = 3, color = "grey", linewidth = 0.5) + 
  geom_violin(fill = "lightgrey", alpha = 0.5) + 
  geom_boxplot(width = 0.2, fill = "#E69F00", color = "black") +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000), labels = scales::label_log()) +
  scale_x_discrete(labels = c(
    estimateExpert = "Ekspertu novērtējums\nExpert estimate",
    estimatePartial = "Daļējs novērtējums\nPartial estimate",
    completeSurvey = "Pilnīgs novērtējums\nComplete Survey"
  )) +
  xlab("Datu kvalitātes klase / Data quality class") +
  ylab("D-B pazīmju attiecība / D-B trait ratio") +
  theme_classic() +
  EnvStats::stat_n_text()

print(grafiks_1)

kruskal_test <- kruskal.test(Dzied.Biotopa.attieciba ~ population_method.text, data = putni)
kruskal_test
# H^2 = 42.698, df = 2, n = 187, p < 0.001
# Rezultātā ir statistiski nozīmīgas atšķirības starp Datu kvalitātes klasēm.

knitr::kable(rstatix::dunn_test(Dzied.Biotopa.attieciba ~ population_method.text, data = putni))
# Rezultātos starp visiem Datu kvalitātes klašu salīdzinājumiem ir statistiski nozīmīgas atšķirības.





# 2. Grafiks ----

putni$population_estimate_type.text <- factor(
  putni$population_estimate_type.text,
  levels = c("Minimum", "estimate", "interval")
)

grafiks_2 <- ggplot(na.omit(putni[, c("population_estimate_type.text", "Dzied.Biotopa.attieciba")]), 
                    aes(x = population_estimate_type.text, y = Dzied.Biotopa.attieciba)) +
  geom_hline(yintercept = 1, lty = 3, color = "grey", linewidth = 0.5) + 
  geom_violin(fill = "lightgrey", alpha = 0.5) + 
  geom_boxplot(width = 0.2, fill = "#E69F00", color = "black") +
  scale_y_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000),
    labels = scales::label_log()
  ) +
  scale_x_discrete(labels = c(
    Minimum = "Minimālā vērtība\nMinimum value",
    estimate = "Aptuvenais novērtējums\nEstimate",
    interval = "95% ticamības intervāls\n95% confidence interval"
  )) +
  xlab("Populāciju aprēķina veids / Population calculation method") +
  ylab("D-B pazīmju attiecība / D-B trait ratio") +
  theme_classic() +
  EnvStats::stat_n_text()
print(grafiks_2)

kruskal_test <- kruskal.test(Dzied.Biotopa.attieciba ~ population_estimate_type.text, data = putni)
kruskal_test
# H^2 = 37.202, df = 2, n = 187, p < 0.001
# Rezultātā ir statistiski nozīmīgas atšķirības starp Populāciju aprēķinu veidiem.

knitr::kable(rstatix::dunn_test(Dzied.Biotopa.attieciba ~ population_estimate_type.text, data = putni))
# Visos salīdzinājumos starp grupām, izņemot starp “Minimum” un “estimate", ir novērotas  novērotas statistiski nozīmīgas atšķirības attiecībā uz D/B pazīmju attiecību.





# 3. Grafiks ----

putni$population_method.text <- factor(
  putni$population_method.text,
  levels = c("estimateExpert", "estimatePartial", "completeSurvey")
)

grafiks_3 <- ggplot(na.omit(putni[, c("population_method.text", "population_size")]), 
                    aes(x = population_method.text, y = population_size)) +
  geom_violin(fill = "lightgrey", alpha = 0.5) + 
  geom_boxplot(width = 0.2, fill = "#E69F00", color = "black") +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000), labels = scales::label_log()) +
  scale_x_discrete(labels = c(
    estimateExpert = "Ekspertu novērtējums\nExpert estimate",
    estimatePartial = "Daļējs novērtējums\nPartial estimate",
    completeSurvey = "Pilnīgs novērtējums\nComplete Survey"
  )) +
  xlab("Datu kvalitātes klase / Data quality class") +
  ylab("Populācijas lielums / Population size") +
  theme_classic() +
  EnvStats::stat_n_text(vjust = -0.5)
print(grafiks_3)

kruskal_test <- kruskal.test(population_size ~ population_method.text, data = putni)
kruskal_test
# H^2 = 88.067, df = 2, n = 196, p < 0.001
# Rezultātā ir statistiski nozīmīgas atšķirības starp Datu kvalitātes klasēm atkarībā no populācijas lieluma.

knitr::kable(rstatix::dunn_test(population_size ~ population_method.text, data = putni))
# Visos salīdzinājumos starp grupām ir novērotas statistiski nozīmīgas atšķirības attiecībā uz populācijas lielumu.



# 4. Grafiks ----

putni$population_estimate_type.text <- factor(
  putni$population_estimate_type.text,
  levels = c("Minimum", "estimate", "interval")
)

grafiks_4 <- ggplot(na.omit(putni[, c("population_estimate_type.text", "population_size")]), 
                    aes(x = population_estimate_type.text, y = population_size)) +
  geom_violin(fill = "lightgrey", alpha = 0.5) + 
  geom_boxplot(width = 0.2, fill = "#E69F00", color = "black") +
  scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000), labels = scales::label_log()) +
  scale_x_discrete(labels = c(
    Minimum = "Minimālā vērtība\nMinimum value",
    estimate = "Aptuvenais novērtējums\nEstimate",
    interval = "95% ticamības intervāls\n95% confidence interval"
  )) +
  xlab("Populāciju aprēķina veids / Population calculation method") +
  ylab("Populācijas lielums / Population size") +
  theme_classic() +
  EnvStats::stat_n_text(vjust = -0.5)
print(grafiks_4)

kruskal_test <- kruskal.test(population_size ~ population_estimate_type.text, data = putni)
kruskal_test
# H^2 = 112.1, df = 2, n = 196, p < 0.001
# Rezultātā ir statistiski nozīmīgas atšķirības starp Populāciju aprēķinu veidiem atkarībā no populācijas lieluma.

knitr::kable(rstatix::dunn_test(population_size ~ population_estimate_type.text, data = putni))
#Visos salīdzinājumos, izņemot "Minimum" un "estimate" grupu, ir novērotas statistiski nozīmīgas atšķirības attiecībā uz populācijas izmēru.


# 5. Grafiks ----

putni$population_trend.text <- factor(
  putni$population_trend.text,
  levels = c("UNK", "U", "D", "S", "I")
)

grafiks_5 <- ggplot(na.omit(putni[, c("population_trend.text", "Dzied.Biotopa.attieciba")]), 
                    aes(x = population_trend.text, y = Dzied.Biotopa.attieciba)) +
  geom_hline(yintercept = 1, lty = 3, color = "grey", linewidth = 0.5) + 
  geom_violin(fill = "lightgrey", alpha = 0.5) + 
  geom_boxplot(width = 0.2, fill = "#E69F00", color = "black") +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000), labels = scales::label_log()) +
  scale_x_discrete(labels = c(
    UNK = "Nezināma\nUnknown",
    U = "Neskaidra\nUncertain",
    D = "Samazinās\nDecreasing",
    S = "Stabila\nStable",
    I = "Pieaug\nIncreasing"
  )) +
  xlab("Īsstermiņa populāciju tendence / Short-term population trend") +
  ylab("D-B pazīmju attiecība / D-B trait ratio") +
  theme_classic() +
  EnvStats::stat_n_text()
print(grafiks_5)




# 6. Grafiks ----

putni$population_trend_long.text <- factor(
  putni$population_trend_long.text,
  levels = c("UNK", "U", "D", "S", "I")
)

grafiks_6 <- ggplot(na.omit(putni[, c("population_trend_long.text", "Dzied.Biotopa.attieciba")]), 
                    aes(x = population_trend_long.text, y = Dzied.Biotopa.attieciba)) +
  geom_hline(yintercept = 1, lty = 3, color = "grey", linewidth = 0.5) + 
  geom_violin(fill = "lightgrey", alpha = 0.5) + 
  geom_boxplot(width = 0.2, fill = "#E69F00", color = "black") +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000), labels = scales::label_log()) +
  scale_x_discrete(labels = c(
    UNK = "Nezināma\nUnknown",
    U = "Neskaidra\nUncertain",
    D = "Samazinās\nDecreasing",
    S = "Stabila\nStable",
    I = "Pieaug\nIncreasing"
  )) +
  xlab("Ilgtermiņa populāciju tendence / Long-term population trend") +
  ylab("D-B pazīmju attiecība / D-B trait ratio") +
  theme_classic() +
  EnvStats::stat_n_text()
print(grafiks_6)




grafiks_1 | grafiks_2

ggsave(filename = "Putnu_sugu_akustiska_kvalitate.jpg", 
       plot = last_plot(),
       height = 1800, 
       width = 3150, 
       dpi = 300, 
       units = "px", 
       device = "jpg")




grafiks_3 | grafiks_4

ggsave(filename = "Putnu_populacija_monitorings.jpg", 
       plot = last_plot(),
       height = 1800, 
       width = 3150, 
       dpi = 300, 
       units = "px", 
       device = "jpg")




grafiks_5 | grafiks_6

ggsave(filename = "Putnu_sugu_akustiska_tendence.jpg", 
       plot = last_plot(),
       height = 1800, 
       width = 3150, 
       dpi = 300, 
       units = "px", 
       device = "jpg")

