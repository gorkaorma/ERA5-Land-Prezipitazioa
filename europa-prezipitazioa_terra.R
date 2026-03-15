library(terra)        # Raster data handling (crop, mask, read/write)
library(sf)           # Vector/shapefile handling
library(ncdf4)        # Work with NetCDF metadata
library(tidyverse)     # Data manipulation (filter, summarise, mutate)
library(RColorBrewer) # Color palettes
library(rnaturalearth)

# Set working directory
setwd("~/R /ERA5 Land Prezipitazioa")

# Leer datos del archivo ERA5-2025-prez.nc
nc <- rast("ERA5-2025-prez.nc")  # Cargar el archivo NetCDF como un objeto de raster")
nc

# Adjust the precipitation unit from m to mm (assuming the original unit is in meters)
nc_mm <- nc * 1000  # Convertir de metros a milímetros

# descargar países
coast <- ne_coastline(scale = "medium", returnclass = "sv")

# Find the max value in the cropped raster
max_precip <- max(values(nc_mm), na.rm = TRUE)
print(max_precip)

# Crear paleta
my_palette <- colorRampPalette(c("white", "lightblue", "darkblue"))
cols <- my_palette(100)   # 100 colores

# definir rangos fijos
breaks_vals <- seq(0, 14, by = 2)

# Guardar plot como PNG
png("precipitacion_2025.png", width = 1200, height = 1200, res = 300)

# Layout 4 filas x 3 columnas
layout(matrix(c(1:12,13,13,13), nrow=5, byrow=TRUE),
       heights=c(1,1,1,1,0.3))

par(mfrow = c(4, 3),
    mar = c(0.2, 0.2, 4.5, 0.2))  # Margen inferior, izquierdo, superior, derecho

mes_eu <- c(
  "Urtarrila","Otsaila","Martxoa","Apirila",
  "Maiatza","Ekaina","Uztaila","Abuztua",
  "Iraila","Urria","Azaroa","Abendua"
)

for (i in 1:nlyr(nc_mm)) {
  
  # raster del mes
  plot(crop_data[[i]],
       breaks = breaks_vals,
       col = cols,
       axes = FALSE,
       asp = 1,
       legend = FALSE)
  
  # añadir costas
  plot(coast,
       add = TRUE,
       col = "black",
       lwd = 0.4)
  
  title(mes_eu[i], line = 3.0, cex.main=0.75, font.main=1)  # Título del mes
}

dev.off()  # Cerrar el dispositivo gráfico

