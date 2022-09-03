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

# 1.2.0
# * Füge eine neue Variable hinzu, die anzeigt, wie sich das Glücks- und 
#   Wohlgefühl der Proband*innen vor und nach dem Test unterscheidet
# * Subtrahiere dafür h_c_pre von h_c_pre
# * Nenne die neue Variable h_c_difference
# * Speichere den bereinigten Datensatz in der Variable 
#   spacing_piano_data_cleaned
# * Proband/in  Nr. 3 hat einen h_c_difference Score von 15, was bedeutet das?
spacing_piano_data_cleaned <- spacing_piano_data_cleaned %>%
  mutate(
    h_c_difference = h_c_post - h_c_pre)

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
