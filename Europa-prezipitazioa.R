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

# Plot the first valid time step (assuming the first layer corresponds to a valid time step)
precip_raster <- nc_mm[[1]]  # Seleccionar la primera capa del raster

# descargar países
coast <- ne_coastline(scale = "medium", returnclass = "sv")
precip_europa <- crop(precip_raster, coast)

# Crear paleta
my_palette <- colorRampPalette(c("white", "lightblue", "darkblue"))
cols <- my_palette(100)   # 100 colores

# definir rangos fijos
breaks_vals <- seq(0, 20, by = 2)

# Guardar plot como PNG
png("precipitacion_urtarrila.png", width = 1200, height = 900, res = 150)

# Plot the raster with minimal theme and remove the axis
p1r <- plot(
  precip_raster,
  main = "Urtarrila",
  breaks = breaks_vals,  # usar los breaks definido
  col = cols,
  axes = FALSE,  # Eliminar ejes
  asp = 1,   # Mantener proporción 1:1
  ) +
  plot(coast,
       add = TRUE,       # superponer sobre el raster
       col = "black", # color del borde
       lwd = 0.2 # grosor de línea
  )

# Guardar plot multiple time steps
png("precipitacion_2025.png", width = 3600, height = 2600, res = 300)

plot(nc_mm, 
     main = "Precipitación mensual en Europa (2025)", 
     col = cols, 
     breaks = breaks_vals, 
     axes = FALSE,
     asp = 1
    )
  

dev.off()  # Cerrar el dispositivo gráfico

