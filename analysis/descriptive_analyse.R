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
# Untersuche den Datensatz mit Hilfe eines Linien- und Punktdiagramms
# * Stelle den zeitlichen Verlauf der durchschnittlichen Akkuratheit (pc) dar

# @Christian: hier bräuchte ich Hilfe. Es geht um alle Variablen, die pc 
#   enthalten. Möglich wäre auch, das ganze in die verschiedenen lags aufteilen
#   zu lassen, um vergleichen zu können
spacing_piano_data_cleaned %>% 
  select(subject_id, contains("pc")) %>% 
  pivot_longer(
    cols = contains("pc"),
    names_to = c("prefix", "time", "task"),
    names_sep = "_",
    values_to = "value"
  ) %>% 
  ggplot(aes(x = time, y = value)) +
  stat_summary(fun = mean, geom = "bar") 


# 1.3.1
# Speichere die Visualisierung im R-Projekt ab unter dem Pfad
# images/xxx.png
ggsave("images/xxx.png", width = 8,
       height = 5, dpi = 300)


# 1.3.2
# 

# 1.3.3
# Speichere die Visualisierung im R-Projekt ab unter dem Pfad
# images/xxx.png
ggsave("images/xxx.png", width = 8,
       height = 5, dpi = 300)



