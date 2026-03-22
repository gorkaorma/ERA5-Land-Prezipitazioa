library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(terra)
library(sf)

# Set working directory
setwd("~/R /ERA5 Land Prezipitazioa")

# Leer datos del archivo ERA5-2025-prez.nc
nc <- rast("ERA5-2025-prez.nc")  # Cargar el archivo NetCDF como un objeto de raster")
nc

# Transfrom data from m to mm
nc <- nc * 1000  # Convertir de metros a milímetros

# Convert the raster to a data frame for ggplot
nc_df <- as.data.frame(nc, xy = TRUE)

# Change column names for easier access to multiple plotting
months <- c(
  "Urtarrila","Otsaila","Martxoa","Apirila",
  "Maiatza","Ekaina","Uztaila","Abuztua",
  "Iraila","Urria","Azaroa","Abendua"
)
colnames(nc_df) <- c("x", "y", months)

# Crear paleta de colores
my_palette <- colorRampPalette(c("white", "#ffffd9","#edf8b1", "#c8e9b4", "#7fcdbb",
                                 "#41b6c4", "#1b91c0", "#225ea8","#0d2c84"))
cols <- my_palette(100)   # 100 colores

# Define ranges for breaks
breaks_vals <- seq(0, 10, by = 2)

# Add coastlines using rnaturalearth
coast <- ne_coastline(scale = "medium", returnclass = "sf")

# Opcional: recortar a Europa usando límites de lat/lon
xmin <- -25
xmax <- 45
ymin <- 30
ymax <- 72

coast_europe <- st_crop(coast, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

# Plot first layer as an example 
ggplot() +
  geom_raster(data = nc_df, aes(x = x, y = y, fill = Urtarrila)) +
  geom_sf(data = coast_europe, fill = NA, color = "black", linewidth = 0.4) +
  scale_fill_gradientn(colors = cols, breaks = breaks_vals) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  theme_void() +
  labs(title = "Precipitación en Europa (2025)", fill = "mm") +
  theme(legend.position = "bottom")

# Plot all months in the same figure using facets
nc_long <- nc_df %>% pivot_longer(cols = months, names_to = "Month", values_to = "Precipitation")

# Define the order of months for better visualization
nc_long$Month <- factor(nc_long$Month, levels = months)

# Set the same legend for all facets manually by defining breaks and limits in scale_fill_gradientn
# Plot with facets for each month
p <- ggplot() +
  geom_raster(data = nc_long, aes(x = x, y = y, fill = Precipitation)) +
  geom_sf(data = coast_europe, fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_gradientn(colors = cols, breaks = breaks_vals, limits = c(0,10), oob = scales::squish) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  theme_void() +
  labs(title = "Prezipitazioa Europan 2025", fill = "mm") +
  theme(legend.position = "bottom") +
  facet_wrap(~ Month, ncol = 3)

# Guardar plot
ggsave(
  filename = "precipitacion_eu_2025.png",  # nombre del archivo
  plot = p,                                   # plot a guardar
  width = 16,                                 # ancho en pulgadas
  height = 12,                                # alto en pulgadas
  dpi = 300                                   # resolución (para imprimir/publicar)
)
  