library(tidyverse)
library(rio)

envios <- import("base_envíos.csv", sheet = "base_envíos")

Envios_corregido <- envios %>%
  mutate(
    id_envío = ifelse(id_envío < 0, id_envío * (-1), id_envío),
    origen = ifelse(is.na(origen), "No registrado", origen),
    destino = ifelse(is.na(destino), "No reportado", destino),
    fecha_envío = ifelse(grepl("^\\d{2}-\\d{2}-\\d{4}$", fecha_envío),
                         as.character(as.Date(fecha_envío, format = "%d-%m-%Y")),
                         fecha_envío),
    fecha_envío = ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", fecha_envío),
                         as.character(as.Date(fecha_envío, format = "%Y-%m-%d")),
                         fecha_envío),
    fecha_entrega = ifelse(is.na(fecha_entrega), "No registrado", as.character(as.Date(fecha_entrega))),
    fecha_envío = ifelse(is.na(fecha_envío), "No registrado", as.character(as.Date(fecha_envío))),
    monto_envío = ifelse(is.na(monto_envío), -1, monto_envío),
    temp_fecha_envio = ifelse(fecha_envío != "No registrado" & fecha_entrega != "No registrado" & as.Date(fecha_envío) > as.Date(fecha_entrega), fecha_entrega, fecha_envío),
    temp_fecha_entrega = ifelse(fecha_envío != "No registrado" & fecha_entrega != "No registrado" & as.Date(fecha_envío) > as.Date(fecha_entrega), fecha_envío, fecha_entrega)
  ) %>%
  select(-fecha_envío, -fecha_entrega) %>%
  rename(fecha_envío = temp_fecha_envio, fecha_entrega = temp_fecha_entrega) %>%
  mutate(
    duracion_envio = ifelse(fecha_envío != "No registrado" & fecha_entrega != "No registrado",
                            as.numeric(as.Date(fecha_entrega) - as.Date(fecha_envío)),
                            NA)
  )

duracion_promedio_region <- Envios_corregido %>%
  group_by(origen) %>%
  summarise(duracion_promedio = mean(duracion_envio, na.rm = TRUE))

duracion_promedio_region
