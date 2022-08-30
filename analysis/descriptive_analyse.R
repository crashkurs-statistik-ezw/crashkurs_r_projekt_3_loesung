# (Installiere und) lade folgende Pakete
library(tidyverse)
library(janitor)
library(haven)

# 1.0 Daten einlesen ------------------------------------------------------

# 1.0.0
# * Lese den Datensatz data/spacing_piano_data.csv ein
# * Speichere den Datensatz in der Variable spacing_piano_data
spacing_piano_data <- read_csv("data/spacing_piano_data.csv")


# 1.1 Daten bereinigen ---------------------------------------------

# 1.1.0
# * Reinige die Variablen mit der Funktion clean_names()
# * Der Datensatz enthält Variablen zu zwei Aufgaben, uns interessiert nur die
#   erste Aufgabe. Lösche daher alle Variablen, die 'task2' im Variablennamen
#   enthalten mit Hilfe von select und contains
# * Kodiere die Variablen music_training und sheet_music mit case_when um:
#   music_training: 1 -> piano, 2 -> other_instrument
#   sheet_music: 1 -> can_read, 2 -> cannot_read
# * Speichere den bereinigten Datensatz in der Variable 
#   spacing_piano_data_cleaned
spacing_piano_data_cleaned <- spacing_piano_data %>% 
  clean_names() %>% 
  select(-contains("task2")) %>% 
  mutate(
    music_training = case_when(
      music_training == 1 ~ "piano",
      music_training == 2 ~ "other_instrument"),
    sheet_music = case_when(
      sheet_music == 1 ~ "can_read",
      sheet_music == 2 ~ "cannot_read"
    )
  )

# 1.2 Datenexport ---------------------------------------------------------

# 1.2.0
# * Exportiere den Datensatz in den Ordner data/cleaned
# * Speichere die Daten unter data/export/spacing_piano_data_cleaned.csv
write_csv(spacing_piano_data_cleaned, "data/export/spacing_piano_data_cleaned.csv")


# 1.2.1
# * Um die Daten in SPSS zu nutzen, exportiere den gereinigten Datensatz mit der
#   Funktion write_sav
# * Speichere die Daten unter data/export/student_data_cleaned.sav
write_sav(spacing_piano_data_cleaned, "data/export/spacing_piano_data_cleaned.sav")


# 1.2 Daten explorieren  ------------------------------------------------

# 1.2.0
# Wie viele Maenner und Frauen sind im Datensatz?
spacing_piano_data_cleaned %>%
  count(gender)

# 1.2.1
# * Bestimme den Mittelwert des Alters aller Probanden sowie je nach Geschlecht
# * Lösche die fehlenden Daten mit Hilfe von drop_na
mean(spacing_piano_data_cleaned$age, na.rm = TRUE)

spacing_piano_data_cleaned %>%
  drop_na(gender, age) %>%
  group_by(gender) %>%
  summarise(
    mean_age = mean(age)
  )

# 1.2.2
# Wie viele der Proband*innen haben Klavierunterricht, Unterricht in einem
# anderen Instrument oder gar keinen Musikunterricht?
spacing_piano_data_cleaned %>%
  count(music_training)

# 1.2.3
# Wie viele Teilnehmende waren in den einzelnen Gruppen (lag_task1)?
spacing_piano_data_cleaned %>%
  count(lag_task1)

# 1.2.4
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
# Untersuche den Datensatz mit Hilfe eines Balkendiagramms
# * Erstelle ein aneinandergereihtes Balkendiagramm, das die Verteilung der 
#   SuS auf die verschiedenen Level von familiaerer Bindungsqualitaet d
#   auf der X-Achse darstellt und das Geschlecht durch die Farbe der 
#   Balken kennzeichnet
# * Nutze position = position_dodge(), um die Balken nebeneinander zu reihen.
# * Füge sinnvolle Achsen- und Legendentitel hinzu
# * Haben die Schueler bessere familiaere Bindungen als Schuelerinnen?
spacing_piano_data_cleaned %>%
  ggplot(aes(lag_task1, mean_pc_final)) +
  geom_bar()


# 1.3.1
# Speichere beide Visualisierungen im R-Projekt ab unter dem Pfad
# images/barbplot_mothers_education_status.png
ggsave("images/verteilung_bildungsqualitaet_geschlecht.png",
       width = 8, height = 5, dpi = 300)


# 1.3.2
# * Erstelle ein aneinandergereihtes Balkendiagramm mit dem Bildungslevel der 
#   Muetter auf der x-Achse und deren Beschaeftigungsstatus auf der Y-Achse.
# * Erstelle  eine neue bedingte Variable mit Hilfe von case_when.
# * Unterscheiden sich die arbeitenden Muetter von den nicht arbeitenden 
#   Muettern in ihrem Bildungslevel?
student_data_cleaned %>%
  mutate(
    working_mother = case_when(
      mjob %in% c("at_home") ~ "no",
      TRUE ~ "yes"
    )
  ) %>%  
  ggplot(aes(x = medu, fill = working_mother)) +
  geom_bar(position = "dodge") +
  labs(
    x     = "Bildungslevel der Mutter",
    y     = "Anzahl",
    fill  = "Working mother"
  ) +
  scale_y_continuous(expand = expansion(0)) +
  scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.9)


# 1.3.3
# Speichere beide Visualisierungen im R-Projekt ab unter dem Pfad
# images/barbplot_mothers_education_status.png
ggsave("images/barbplot_mothers_education_status.png", width = 8,
       height = 5, dpi = 300)



