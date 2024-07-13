library(tidyverse)
library(rio)
registros_med <- import("base_registros_medicos.csv")

registros_med_corregido <- registros_med %>%
  mutate(id_registro = ifelse(id_registro < 0, id_registro * (-1), id_registro)) %>%
  mutate(id_paciente = ifelse(id_paciente < 0, id_paciente * (-1), id_paciente)) %>%
  mutate(fecha_admision = ifelse(grepl("^\\d{2}-\\d{2}-\\d{4}$", fecha_admision),
                                 as.character(as.Date(fecha_admision, format = "%d-%m-%Y")),
                                 fecha_admision)) %>%
  mutate(fecha_admision = ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha_admision),
                                 as.character(as.Date(fecha_admision, format = "%Y-%m-%d")),
                                 fecha_admision)) %>%
  mutate(fecha_alta = case_when(
    estado == "Hospitalizado" ~ NA_character_,
    estado == "Desconocido" ~ "Desconocido",
    TRUE ~ as.character(as.Date(fecha_alta, format = "%Y-%m-%d"))
  )) %>%
  mutate(costo = ifelse(is.na(costo), -1, costo)) %>%
  mutate(diagnostico = ifelse(is.na(diagnostico), "No registrado", diagnostico)) %>%
  mutate(estado = ifelse(is.na(estado), "Desconocido", estado)) %>%
  mutate(temp_fecha_admision = ifelse(fecha_admision > fecha_alta & !is.na(fecha_alta), fecha_alta, fecha_admision),
         temp_fecha_alta = ifelse(fecha_admision > fecha_alta & !is.na(fecha_alta), fecha_admision, fecha_alta)) %>%
  select(-fecha_admision, -fecha_alta) %>%
  rename(fecha_admision = temp_fecha_admision, fecha_alta = temp_fecha_alta) 


registros_med_final <- registros_med_corregido %>%
  
  mutate(duracion_estadia = as.numeric(difftime(as.Date(fecha_alta), as.Date(fecha_admision), units = "days")),
         mes_admision = format(as.Date(fecha_admision), "%Y-%m")) %>%

  filter(!is.na(fecha_alta) & fecha_alta != "Desconocido") %>%

  mutate(duracion_anormal = ifelse(duracion_estadia < 0 | duracion_estadia > 365, TRUE, FALSE)) %>%
  
  group_by(id_paciente) %>%
  mutate(visitas_recientes = sum(as.numeric(difftime(Sys.Date(), as.Date(fecha_admision), units = "days")) <= 30)) %>%
  
  mutate(costo_total_paciente = sum(costo, na.rm = TRUE)) %>%
  ungroup() %>%

  mutate(costo_sospechoso = ifelse(costo_total_paciente > quantile(costo_total_paciente, 0.95), TRUE, FALSE))