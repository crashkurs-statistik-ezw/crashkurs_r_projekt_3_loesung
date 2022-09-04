
# 2.0 Setup -------------------------------------------------------------

# 2.0.1 Pakete installieren
# (Installiere und) lade die Pakete tidyverse, janitor und haven
library(tidyverse)
library(janitor)
library(haven)

# 2.1 Daten einlesen ------------------------------------------------------

# 2.1.1 Daten einlesen
# * Lese den Datensatz data/spacing_piano_data.csv ein
# * Speichere den Datensatz in der Variable spacing_piano_data
spacing_piano_data <- read_csv("data/spacing_piano_data.csv")


# 2.2 Daten bereinigen ---------------------------------------------

# 2.2.1 Daten reinigen
# * Reinige die Variablendes Datensatzes spacing_piano_data
#   mit der Funktion 'clean_names' aus dem Paket janitor
# * Der Datensatz enthält Variablen zu zwei Aufgaben, uns interessiert nur die
#   erste Aufgabe. Entferne daher alle Variablen, die 'task2' im Variablennamen
#   enthalten mit Hilfe der Funktionen 'select' und 'contains'
# * Kodiere die Variablen music_training und sheet_music mit 'case_when' um:
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


# 2.2.2 - Differenz berechnen
# * Füge eine neue Variable hinzu, die anzeigt, wie sich das Glücks- und 
#   Wohlgefühl der Proband*innen vor und nach dem Test unterscheidet.
#   Subtrahiere hierfür h_c_pre von h_c_post
# * Nenne die neue Variable h_c_difference
# * Speichere den bereinigten Datensatz in der Variable 
#   spacing_piano_data_cleaned
spacing_piano_data_cleaned <- spacing_piano_data_cleaned %>%
  mutate(
    h_c_difference = h_c_post - h_c_pre
  )


# 2.2.3
# Proband/in  Nr. 3 hat einen h_c_difference Score von 15, was bedeutet das?


# 2.3 Datenexport ---------------------------------------------------------

# 2.3.1 CSV-Datei speichern
# Speichere den Data Frame unter data/export/spacing_piano_data_cleaned.csv
write_csv(spacing_piano_data_cleaned, "data/export/spacing_piano_data_cleaned.csv")


# 2.3.2
# * Um die Daten in SPSS zu nutzen, exportiere den gereinigten Datensatz mit der
#   Funktion write_sav
# * Speichere die Daten unter data/export/student_data_cleaned.sav
write_sav(spacing_piano_data_cleaned, "data/export/spacing_piano_data_cleaned.sav")
