# 2.5 Setup ---------------------------------------------------------------

# 2.5.1 Daten einlesen
# Imnportiere die CSV-Datei data/export/spacing_piano_data_cleaned.csv
# und speichere sie in der Variable spacing_data
spacing_data <- read_csv("data/export/spacing_piano_data_cleaned.csv")


# 2.6 Daten explorieren  ------------------------------------------------

# 2.6.1 Geschlecht zählen
# Wie viele Maenner und Frauen sind im Datensatz?
spacing_data %>%
  count(gender)


# 2.6.2 Mittelwerte bestimmen
# Bestimme den Mittelwert des Alters aller Proband*innen
mean(spacing_data$age, na.rm = TRUE)


# 2.6.3 Mittelwerte bestimmen II
# Bestimme den Mittelwert des Alters aller Proband*innen nach ihrem Geschlecht
# mit Hilfe von group_by und summarise. Entferne vorab alle fehlenden 
# Werte in der Variable 'gender' und 'age' mit drop_na
spacing_data %>%
  drop_na(gender, age) %>%
  group_by(gender) %>%
  summarise(
    mean_age = mean(age)
  )


# 2.6.4 Häufigkeit Musikunterricht
# Wie viele der Proband*innen haben Klavierunterricht, Unterricht in einem
# anderen Instrument oder gar keinen Musikunterricht?
spacing_data %>%
  count(music_training)


# 2.6.5 TN pro Gruppe
# Wie viele Teilnehmende waren in den einzelnen Spacing-Gruppen (lag_task1)? 
# Die Zahl steht für die Dauer der Pause zwischen den Übungsphasen 
# beim Klavierspielen
spacing_data %>%
  count(lag_task1)


# 2.6.6 Mittelwerte für die Leistungskriterien pro Gruppe
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


# 2.6.7 Mittelwerte für die Leistungskriterien pro Gruppe II
# Führe den folgenden Code aus. Er kommt zum gleichen Ergebniss wie der 
# Code von 2.5.6 ist allerdings ein wenig eleganter und verwendet die 
# Funktion across
spacing_data %>% 
  group_by(lag_task1) %>% 
  summarise(
    across(
      .cols = contains("final_task"),
      .fns  = ~ mean(., na.rm = TRUE)
    )
  )


# 2.7 Daten visualisieren -----------------------------------------------------

# 2.7.1 Balkendiagramm der Leistungsentwicklung erstellen
# Untersuche die Entwicklung mit Hilfe eines Balkendiagramms
# * Bringe die Variablen, die den String "pc" enthalten mit Hilfe von 
#   pivot_longer in ein langes Format
# * Erstelle ein Balkendiagramm mit den Lag Times auf der X-Achse und 
#   TODO auf der Y-Achse
# * Ordne die Balken auf der X-Achse en Zeitpunkten nach von baseline bis final
# * Lasse dir ein Balkendiagramm für jede der Gruppen mit verschieden langen
#   Übungspausen anzeigen, nutze dafür facet_wrap
spacing_data %>% 
  select(subject_id, lag_task1, contains("pc")) %>% 
  # TODO: Pivot longer
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
  ggplot(
    aes(x = factor(time, level = c('baseline', 'post1', 'pre2', 'post2',
                                        'final')), 
        y = group_mean)) + # TODO: group_mean einfuegen
  geom_col(fill = "grey10", alpha = .7, width = 0.8) + # TODO
  scale_y_continuous(expand = expansion(0)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(vars(lag_task1)) +
  labs(
    x     = "Zeitpunkt",
    y     = "Akkuratheit der gespielten Noten"
  )

# 2.7.2
# TODO: Idee nur Final vergleich der beiden Gruppen als Boxplot


# 2.7.3 Visualisierung speichern
# Speichere die Visualisierung im R-Projekt ab unter dem Pfad images/xxx.png
ggsave("images/balkendiagramm_.png", width = 8,  # TODO: DAteien spezifisch benennen
       height = 5, dpi = 300)


# 2.7.4 Balkendiagramm des Glücks- und Wohlgefühls erstellen
# * Erstelle ein weiteres Balkendiagramm, welches die Entwicklung des Glücks-
#   und Wohlgefühls zwischen den Gruppen vergleicht.
# * Lösche zuerst die fehlenden Werte mit drop_na
# * Berechne die Mittelwerte von h_c_difference mit group_by und summarise
# * Übergebe den Output an ggplot und erstelle ein Balkendiagramm
# * Welche Übungspause geht mit dem stärksten Anstieg des Wohlbefindens einher?
spacing_data %>%
  drop_na(h_c_difference, lag_task1) %>% 
  group_by(lag_task1) %>% 
  summarise(
    mean_h_c_difference = mean(h_c_difference)) %>% 
  ggplot(aes(x = lag_task1, y = mean_h_c_difference)) +
  geom_col() +
  scale_x_continuous(breaks = c(0, 1, 5, 10, 15))

# 2.7.5 Visualisierung speichern
# Speichere die Visualisierung im R-Projekt ab unter dem Pfad images/xxx.png
ggsave("images/xxx.png", width = 8,
       height = 5, dpi = 300)



