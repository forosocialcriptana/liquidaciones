library(dplyr)
library(quarto)

df <- read.csv("datos/gastos-programas.csv", stringsAsFactors = FALSE,
               fileEncoding = "UTF-8", sep = ";")
names(df) <- c("Año", "organismo", "programa", "economica", "sp",
               "descripcion", "Presupuesto", "Liquidación")

etiquetas <- df |>
  distinct(programa, descripcion) |>
  arrange(descripcion)

for (i in seq_len(nrow(etiquetas))) {
  prog <- etiquetas$programa[i]
  desc <- etiquetas$descripcion[i]

  message(sprintf("[%d/%d] Renderizando: %s", i, nrow(etiquetas), desc))

  quarto_render(
    input          = "programas/programa.qmd",
    execute_params = list(codigo = prog, descripcion = desc),
    output_file    = paste0(prog, ".html")
  )
}

message("Listo. Páginas guardadas en programas/")
