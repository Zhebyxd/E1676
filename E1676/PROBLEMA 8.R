library(tidyverse)
library(rio)

transacciones <- import("base_transacciones.csv")
transacciones_limpio <- transacciones %>%
  mutate(
    id_transaccion = as.integer(abs(id_transaccion)),
    id_cliente = as.integer(abs(id_cliente)),
    monto = as.numeric(ifelse(is.na(monto), -1, monto)),
    tipo_transaccion = as.character(ifelse(is.na(tipo_transaccion), "no identificado", tipo_transaccion)),
    estado_transaccion = as.character(ifelse(is.na(estado_transaccion), "no identificado", estado_transaccion)),
    fecha_transaccion = as.Date(fecha_transaccion),
    fecha_transaccion = format(fecha_transaccion, "%Y-%m-%d")
  )

transacciones_con_dias <- transacciones_limpio %>%
  arrange(id_cliente, fecha_transaccion) %>%
  group_by(id_cliente) %>%
  mutate(dias_desde_ultima_transaccion = difftime(max(fecha_transaccion), fecha_transaccion, units = "days")) %>%
  ungroup()

transacciones_completadas <- transacciones_con_dias %>%
  filter(estado_transaccion == "Completada")


limite_monto_anormal <- mean(transacciones_con_dias$monto, na.rm = TRUE) + 3 * sd(transacciones_con_dias$monto, na.rm = TRUE)

transacciones_anormales <- transacciones_con_dias %>%
  filter(monto > limite_monto_anormal)
summary(transacciones_anormales$monto)

transsaciones_anormales


