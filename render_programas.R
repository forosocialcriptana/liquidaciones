library(dplyr)
library(quarto)

nombres_areas <- c(
  "1" = "Área 1 – Servicios Públicos Básicos",
  "2" = "Área 2 – Protección y Promoción Social",
  "3" = "Área 3 – Bienes Públicos Preferentes",
  "4" = "Área 4 – Actuaciones Económicas",
  "9" = "Área 9 – Actuaciones de Carácter General"
)

display_areas <- c(
  "1" = "Área 1<br><small>Servicios Básicos</small>",
  "2" = "Área 2<br><small>Prot. Social</small>",
  "3" = "Área 3<br><small>Bienes Pref.</small>",
  "4" = "Área 4<br><small>Económico</small>",
  "9" = "Área 9<br><small>General</small>"
)

nombres_subareas <- c(
  "11" = "Deuda Pública",
  "13" = "Seguridad y Movilidad Ciudadana",
  "15" = "Vivienda y Urbanismo",
  "16" = "Bienestar Comunitario",
  "17" = "Medio Ambiente",
  "21" = "Pensiones",
  "22" = "Otras Prestaciones Económicas",
  "23" = "Servicios Sociales y Promoción Social",
  "24" = "Fomento del Empleo",
  "31" = "Sanidad",
  "32" = "Educación",
  "33" = "Cultura",
  "34" = "Deporte",
  "41" = "Agricultura, Ganadería y Pesca",
  "43" = "Comercio, Turismo y Pymes",
  "44" = "Transporte Público",
  "45" = "Infraestructuras",
  "49" = "Otras Infraestructuras",
  "91" = "Órganos de Gobierno",
  "92" = "Servicios de Carácter General",
  "93" = "Administración Financiera y Tributaria",
  "94" = "Transferencias a otras Administraciones"
)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

df <- read.csv("datos/gastos-programas.csv", stringsAsFactors = FALSE,
               fileEncoding = "UTF-8", sep = ";")
names(df) <- c("Año", "organismo", "programa", "economica", "sp",
               "descripcion", "Presupuesto", "Liquidación")

etiquetas <- df |>
  distinct(programa, descripcion) |>
  arrange(descripcion)

# --- Generar menú de navegación ---
menu_data <- etiquetas |>
  distinct(programa, .keep_all = TRUE) |>
  mutate(
    subarea    = substr(programa, 1, 2),
    area       = ifelse(subarea == "11", "9", substr(programa, 1, 1)),
    area_nom   = nombres_areas[area],
    sub_nom    = nombres_subareas[subarea],
    sub_nom    = ifelse(is.na(sub_nom), paste("Subárea", subarea), sub_nom)
  ) |>
  arrange(area, subarea, programa)

areas_unicas <- distinct(menu_data, area, area_nom) |> arrange(area)

dropdowns <- lapply(areas_unicas$area, function(a) {
  progs_area <- menu_data[menu_data$area == a, ]
  subareas   <- unique(progs_area$subarea)

  bloques <- lapply(seq_along(subareas), function(i) {
    sa <- subareas[i]
    progs_sa <- progs_area[progs_area$subarea == sa, ]
    header <- sprintf(
      '          <li><h6 class="dropdown-header">%s (%s)</h6></li>',
      html_escape(progs_sa$sub_nom[1]), sa
    )
    items <- paste0(
      sprintf('          <li><a class="dropdown-item" href="%s.html">%s — %s</a></li>',
              progs_sa$programa, progs_sa$programa,
              html_escape(progs_sa$descripcion)),
      collapse = "\n"
    )
    divider <- if (i < length(subareas))
      '\n          <li><hr class="dropdown-divider"></li>' else ""
    paste0(header, "\n", items, divider)
  })

  sprintf(
    '        <li class="nav-item dropdown">
          <a class="nav-link dropdown-toggle text-white" href="#" role="button"
             data-bs-toggle="dropdown" aria-expanded="false">%s</a>
          <ul class="dropdown-menu">
%s
          </ul>
        </li>',
    display_areas[a],
    paste(bloques, collapse = "\n")
  )
})

nav_html <- sprintf(
  '<style>
  .dropdown-menu { max-width: 320px; }
  .dropdown-item { white-space: normal; }
  #navProgramas .nav-link { line-height: 1.2; padding: 0.25rem 0.6rem; }
  #navProgramas small { font-size: 0.75em; opacity: 0.85; }
</style>
<nav class="navbar navbar-dark"
     style="background-color:#1e3a5f; position:sticky; top:0; z-index:1020;">
  <div class="container-fluid flex-column align-items-start">
    <div class="d-flex w-100 justify-content-between align-items-center py-2">
      <span class="navbar-brand fw-bold mb-0">Liquidaciones Municipales</span>
      <button class="navbar-toggler d-lg-none" type="button"
              data-bs-toggle="collapse" data-bs-target="#navProgramas"
              aria-controls="navProgramas" aria-expanded="false"
              aria-label="Abrir menú">
        <span class="navbar-toggler-icon"></span>
      </button>
    </div>
    <div class="collapse navbar-collapse w-100 d-lg-block" id="navProgramas">
      <ul class="navbar-nav flex-row flex-wrap pb-2">
%s
      </ul>
    </div>
  </div>
</nav>',
  paste(dropdowns, collapse = "\n")
)

writeLines(nav_html, "programas/_menu.html", useBytes = FALSE)
message("Menú generado en programas/_menu.html")

# --- Renderizar páginas ---
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
