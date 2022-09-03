# 1.0 Daten explorieren  ------------------------------------------------

# 1.0.0
# Wie viele Maenner und Frauen sind im Datensatz?
spacing_piano_data_cleaned %>%
  count(gender)

# 1.0.1
# * Bestimme den Mittelwert des Alters aller Probanden sowie je nach Geschlecht
# * Lösche die fehlenden Daten mit Hilfe von drop_na
mean(spacing_piano_data_cleaned$age, na.rm = TRUE)

spacing_piano_data_cleaned %>%
  drop_na(gender, age) %>%
  group_by(gender) %>%
  summarise(
    mean_age = mean(age)
  )

# 1.0.2
# Wie viele der Proband*innen haben Klavierunterricht, Unterricht in einem
# anderen Instrument oder gar keinen Musikunterricht?
spacing_piano_data_cleaned %>%
  count(music_training)

# 1.0.3
# Wie viele Teilnehmende waren in den einzelnen Gruppen (lag_task1)?
spacing_piano_data_cleaned %>%
  count(lag_task1)

# 1.0.4
# * Bestimme mit Hilfe von group_by und summarise den Mittelwert je Gruppe
#   (lag_task1) für die drei verschiedenen Leistungskriterien im Abschlusstest
# * Speichere den Output als group_means
spacing_piano_data_cleaned %>%
  group_by(lag_task1) %>%
  summarise(
    mean_pc_final = mean(pc_final_task1),
    mean_sdl_final = mean(sdl_final_task1),
    mean_sda_final = mean(sda_final_task1)
  )


# 1.3 Daten visualisieren -----------------------------------------------------

# 1.3.0
# Untersuche die Entwicklung mit Hilfe eines Balkendiagramms
# * Bringe die Daten mit Hilfe von pivot_longer in ein langes Format
# * Erstelle ein Balkendiagramm mit Hilfe von geom_col
# * Ordne die Balken den Zeitpunkten nach von baseline bis final
# * Lasse dir ein Balkendiagramm für jede der Gruppen mit verschieden langen
#   Übungspausen anzeigen, nutze dafür facet_wrap
spacing_piano_data_cleaned %>% 
  select(subject_id, lag_task1, contains("pc")) %>% 
  pivot_longer(
    cols = contains("pc"),
    names_to = c("prefix", "time", "task"),
    names_sep = "_",
    values_to = "value"
  ) %>% 
  ggplot(aes(x = factor(time, level = c('baseline', 'post1', 'pre2', 'post2',
                                        'final')), y = value)) +
  geom_col() +
  facet_wrap(~lag_task1) +
  labs(
    x     = "Zeitpunkt",
    y     = "Wert"
  )

# das gleiche mit Mittelwerten, Datensatz in excel erstellt
piano_pc %>% 
  ggplot(aes(x = factor(time, level = c('baseline', 'post1', 'pre2', 'post2',
                                      'final')), y = value)) +
  geom_col() +
  facet_wrap(~condition) +
  labs(
    x     = "Zeitpunkt",
    y     = "Wert"
  )

# 1.3.1
# Speichere die Visualisierung im R-Projekt ab unter dem Pfad images/xxx.png
ggsave("images/xxx.png", width = 8,
       height = 5, dpi = 300)


# 1.3.2
# * Erstelle ein weiteres Balkendiagramm, welches die Entwicklung des Glücks-
#   und Wohlgefühls zwischen den Gruppen vergleicht.
# * Lösche zuerst die fehlenden Werte mit drop_na
# * Berechne die Mittelwerte von h_c_difference mit group_by und summarise
# * Übergebe den Output an ggplot und erstelle ein Balkendiagramm
# * Welche Übungspause geht mit dem stärksten Anstieg des Wohlbefindens einher?
spacing_piano_data_cleaned %>%
  drop_na(h_c_difference, lag_task1) %>% 
  group_by(lag_task1) %>% 
  summarise(
    mean_h_c_difference = mean(h_c_difference)) %>% 
  ggplot(aes(x = lag_task1, y = mean_h_c_difference)) +
  geom_col()

# 1.3.3
# Speichere die Visualisierung im R-Projekt ab unter dem Pfad images/xxx.png
ggsave("images/xxx.png", width = 8,
       height = 5, dpi = 300)



