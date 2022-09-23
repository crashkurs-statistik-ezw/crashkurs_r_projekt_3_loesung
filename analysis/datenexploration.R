# 3.5 Setup ---------------------------------------------------------------

# 3.5.1 Daten einlesen
# Imnportiere die CSV-Datei data/cleaned/spacing_piano_data_cleaned.csv
# und speichere sie in der Variable spacing_data
spacing_data <- read_csv("data/cleaned/spacing_piano_data_cleaned.csv")


# 3.6 Daten explorieren  ------------------------------------------------

# 3.6.1 Geschlecht zählen
# Wie viele Maenner und Frauen sind im Datensatz?
spacing_data %>%
  count(gender)


# 3.6.2 Mittelwerte bestimmen
# Bestimme den Mittelwert des Alters aller Proband*innen
mean(spacing_data$age, na.rm = TRUE)


# 3.6.3 Mittelwerte bestimmen II
# Bestimme den Mittelwert des Alters aller Proband*innen nach ihrem Geschlecht
# mit Hilfe von group_by und summarise. Entferne vorab alle fehlenden 
# Werte in der Variable 'gender' und 'age' mit drop_na
spacing_data %>%
  drop_na(gender, age) %>%
  group_by(gender) %>%
  summarise(
    mean_age = mean(age)
  )


# 3.6.4 Häufigkeit Musikunterricht
# Wie viele der Proband*innen haben Klavierunterricht, Unterricht in einem
# anderen Instrument oder gar keinen Musikunterricht?
spacing_data %>%
  count(music_training)


# 3.6.5 TN pro Gruppe
# Wie viele Teilnehmende waren in den einzelnen Spacing-Gruppen (lag_task1)? 
# Die Zahl steht für die Dauer der Pause zwischen den Übungsphasen 
# beim Klavierspielen in Minuten
spacing_data %>%
  count(lag_task1)


# 3.6.6 Mittelwerte für die Leistungskriterien pro Gruppe
# * Bestimme mit Hilfe von group_by und summarise den Mittelwert je Gruppe
#   (lag_task1) für die drei verschiedenen Leistungskriterien im Abschlusstest
# * Speichere den Output als group_means
spacing_data %>%
  group_by(lag_task1) %>%
  summarise(
    mean_pc_final = mean(pc_final_task1),
    mean_sdl_final = mean(sdl_final_task1),
    mean_sda_final = mean(sda_final_task1)
  )


# 3.6.7 Mittelwerte für die Leistungskriterien pro Gruppe II
# * Führe den folgenden Code aus. Er kommt zum gleichen Ergebniss wie der 
#   Code von 3.6.6 ist allerdings ein wenig eleganter und verwendet die 
#   Funktion across
# * Mehr Informationen findest du unter: 
#   https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-colwise/
spacing_data %>% 
  group_by(lag_task1) %>% 
  summarise(
    across(
      .cols = contains("final_task"),
      .fns  = list(mean = ~ mean(., na.rm = TRUE)),
      .nanmes = "{.col}_{.fn}"
    )
  )


# 3.7 Daten visualisieren -----------------------------------------------------

# 3.7.1 Balkendiagramm der Leistungsentwicklung erstellen
# Untersuche die Entwicklung mit Hilfe eines Balkendiagramms
# * Bringe die Variablen, die den String "pc" enthalten mit Hilfe von 
#   pivot_longer in ein langes Format. Tipp schaue dir das
#   Cheat Sheet zu pivot_longer an, um diese Aufgabe zu lösen
# * Erstelle ein Balkendiagramm mit den Lag Times auf der X-Achse und 
#   dem Messzeitpunkt auf der Y-Achse
# * Ordne die Balken auf der X-Achse den Zeitpunkten nach von baseline bis final
#   Wenn du Hilfe dabei brauchst, schaue dir diesen Thread an:
#   https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
# * Lasse dir ein Balkendiagramm für jede der Gruppen (lag_task1)
#   anzeigen; nutze hierfür facet_wrap
spacing_data %>% 
  select(subject_id, lag_task1, contains("pc")) %>% 
  pivot_longer(
    cols = contains("pc"),
    names_to = c("prefix", "time", "task"),
    names_sep = "_",
    values_to = "value"
  ) %>% 
  group_by(time, task, prefix, lag_task1) %>% 
  summarise(
    group_mean = mean(value, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = factor(time, level = c('baseline', 'post1', 'pre2', 'post2',
                                        'final')), 
        y = group_mean)) +
  geom_col(fill = "grey20", alpha = .7, width = 0.8) +
  scale_y_continuous(expand = expansion(0)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(vars(lag_task1)) +
  labs(
    x     = "Zeitpunkt",
    y     = "Akkuratheit der gespielten Noten"
  )


# 3.7.2 Visualisierung speichern
# Speichere die Visualisierung im R-Projekt ab unter dem Pfad 
# images/balkendiagramm_entwicklung_akkuratheit.png
ggsave("images/balkendiagramm_entwicklung_akkuratheit.png", width = 8,
       height = 5, dpi = 300)


# 3.7.3 Balkendiagramm des Glücks- und Wohlgefühls erstellen
# * Erstelle ein weiteres Balkendiagramm, welches die Entwicklung des Glücks-
#   und Wohlgefühls zwischen den Gruppen vergleicht.
# * Berechne die Mittelwerte von h_c_difference mit group_by und summarise pro
#   Gruppe (lag_task1)
# * Wandle die Variable lag_task1 in einen Faktor um
# * Übergebe den Output an ggplot und erstelle ein Balkendiagramm
# * Welche Übungspause geht mit dem stärksten Anstieg des Wohlbefindens einher?
spacing_data %>%
  drop_na(h_c_difference, lag_task1) %>% 
  group_by(lag_task1) %>% 
  summarise(
    mean_h_c_difference = mean(h_c_difference, na.rm = TRUE)) %>% 
  mutate(
    lag_task1 = as.factor(lag_task1)
  ) %>% 
  ggplot(aes(x = lag_task1, y = mean_h_c_difference)) +
  geom_col() 


# 3.7.4 Visualisierung speichern
# Speichere die Visualisierung im R-Projekt ab unter dem Pfad 
# images/balkendiagramm_gluecks_und_wohlgefuehl.png
ggsave("images/balkendiagramm_gluecks_und_wohlgefuehl.png", width = 8,
       height = 5, dpi = 300)



