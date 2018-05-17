library(rasterVis)
library(rgdal)
library(latticeExtra)
library(grid)
library(RColorBrewer)
library(maptools)
setwd("S:/Daten/Klimatische_Daten/Referenzwerte")

# Geplottet werden mehrere Raster & shapes, um eine Karte (vergleichbar mit QGIS-Ouptut) zu erzeugen.
# Inhalt: CRS, clipping, Geodata-Formate, hillshade & Formatierung von Karten

#### Vorbereitung der Geodaten ####
# Clipping dt. Grids
dwd <- raster("GERMANY_hotd.asc")
crs(dwd) <- "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs"
dwd <- projectRaster(from=dwd, crs="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
dwd <- crop(dwd, extent(area)) #clip to extent
var_rast <- mask(dwd, area) #mask unwanted pixels
# -> dt. Raster, ETRS89 UTM Zone 32 (EPSG:25832)

# Rasterization der frz. csv-Daten (+ clip)
aur <- read.csv("France_Hitzetage.csv", stringsAsFactors = F, dec = ",")
aur[,1:2] <- aur[,1:2]*100
var_rast2 <- rasterFromXYZ(aur, crs = "+proj=lcc +nadgrids=ntf_r93.gsb,null +a=6378249.2000 +rf=293.4660210000000 +pm=2.337229167 +lat_0=46.800000000 +lon_0=0.000000000 +k_0=0.99987742 +lat_1=46.800000000 +x_0=600000.000 +y_0=2200000.000 +units=m +no_defs")
# -> frz. Raster, Lambert Etendu II (IGNF:LAMBE)

area2 <- readOGR(dsn = "shapes", layer = "25832_DE-FR_shape") #kleineres Clip-Shape (ohne CH), um Pixel an Schweizer Grenze zu umgehen
area2 <- spTransform(area2, CRS("+proj=lcc +nadgrids=ntf_r93.gsb,null +a=6378249.2000 +rf=293.4660210000000 +pm=2.337229167 +lat_0=46.800000000 +lon_0=0.000000000 +k_0=0.99987742 +lat_1=46.800000000 +x_0=600000.000 +y_0=2200000.000 +units=m +no_defs"))
var_rast2 <- mask(var_rast2, area2)

# Saving GeoTiffs
vname <- "P_Sommer"
name1 <- paste0("DE_", vname, ".tif")
name2 <- paste0("FR_", vname, ".tif")
writeRaster(var_rast, name1, "GTiff", overwrite=T)
writeRaster(var_rast2, name2, "GTiff", overwrite=T)

# boundary, basemap & hillshade
area <- readOGR(dsn = "shapes", layer = "25832_tutti_shape") # Polygon-Shapefile
city <- readOGR(dsn = "shapes", layer = "25832_cities") # Point-Shapefile

basemap <- raster("S:/Daten/Hohenmodell/georhena_basemap_25832.tif") # DEM-Raster
b_slope <- terrain(basemap, opt="slope")
b_aspect <- terrain(basemap, opt="aspect")
b_hill <- hillShade(b_slope, b_aspect, 40, 270)

# Konvertieren in Vektordaten als Alternative für korrekte Darstellung trotz Transformation (z.B. in spplot, ggplot)
var_poly2 <- rasterToPolygons(var_rast2)
var_poly2 <- spTransform(var_poly2, CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#### Levelplot Methodik ####
#colr <- colorRampPalette(brewer.pal(9, "RdBu")) #Palette für Vars
colr <- colorRampPalette(rev(brewer.pal(9, "RdBu"))) #invertierte Variante
colr <- colorRampPalette(brewer.pal(9, "YlGnBu")) #für Niederschlag
colr2 <- colorRampPalette(c("#d5bb83", "#658b5e", "#e09130", "#9d4423", "#79351c", "#efebe9")) #Palette für basemap
# Definition der Breaks für basemap-Palette
b1=80
b2=400
b3=1000
b4=1400
b5=2000
b6=4000
# Definition der Legenden-Settings
i1=min(c(var_rast[], var_rast2[]), na.rm = T) #Wertebereich definieren
i2=max(c(var_rast[], var_rast2[]), na.rm = T)+0.1
is=round(diff(c(floor(i1), ceiling(i2)))/7, -1)

key1 <- list(space="bottom", labels=list(at=seq(round(i1,-1), round(i2,-1), by=is), cex=0.7))
             #title="Frosttage\npro Jahr", title.gpar=list(cex=0.8, font=2)) # Legendentitel direkt einstellen!
key2 <- list(space="bottom", col="transparent", axis.line=list(col="transparent"), labels=list(at=0, labels=""))

# 1. Value-Raster DE + area
de <- levelplot(var_rast, maxpixels=1e6, margin=F, colorkey=key1, col.regions=colr, #main=list(label='climability',side=1,line=0.5, cex=2),
                at=seq(48, 182, len=101), xlim=c(325000, 505000), ylim=c(5210000, 5480000))

# 2. Basemap aus DEM
base <- levelplot(b_hill, maxpixels=1e6, margin=F, col.regions=grey(0:100/100), colorkey=key2,
                  xlim=c(325000, 505000), ylim=c(5210000, 5480000),
                  scales=list(x=list(cex=0.5),y=list(cex=0.5))) + #Achsen formatieren
  levelplot(basemap, maxpixels=1e6, margin=F, col.regions=colr2, colorkey=key2, alpha.regions=0.6,
            at=unique(c(seq(b1,b2,l=30), seq(b2,b3,l=30), seq(b3,b4,l=30), seq(b4,b5,l=30), seq(b5,b6,l=30))),
            xlim=c(325000, 505000), ylim=c(5210000, 5480000))

# 3. Value-Raster als Polygone (FR data)
# Definition von layout-Variablen (hier: Pointlayer mit Städtenamen)
sl1 <- list("sp.points", city, pch=21, cex=1.1, col="white", fill="black")
sl2 <- list("sp.pointLabel", city, label=as.character(city$name), position='below', textloc="maxDepth",
            cex=0.7, col="black", fontfamily="serif", fontface=2) #fontface: breit/kursiv/etc.

fr <- spplot(var_poly2, col = NA, col.regions=colr(100), at=seq(i1, i2, len=101))
  #layer(sp.polygons(area, col="grey80")) #+layer statt sp.layout, um Shape auf höchste Ebene zu setzen
misc <- spplot(area, zcol="id", col="grey80", sp.layout=list(sl1,sl2))

#spplot(area, zcol="id", sp.layout=list(sl1,sl2, sl3), col="grey80") #dummy-plot

# Kombination der Raster & Nutzung der Legende von 1.
p <- base + de + fr + misc
p$legend$bottom$args <- de$legend$bottom$args # Legendenformatierung aus Levelplot 1 übernehmen

png("plot_fd2.png", width = 12, height = 18, unit="cm", res=200) # Export als PNG
p
trellis.focus("legend", side="bottom", clipp.off=TRUE, highlight=FALSE)
grid::grid.text("Frosttage\npro Jahr", -0.1, 0.8, gp=gpar(fontsize=8, font=2)) #Legendentitel hinzufügen
trellis.unfocus()
dev.off()

#### ggplot Methodik ####
library(ggplot2)
library(Cairo)

# Wichtiger Hinweis: keine on-the-fly-Transformation möglich
# -> Raster mit abweichenden CRS nicht in 1 Plot darstellbar
# workaround: Vektorisierung des betreff. Rasters
# dazu: Methode zur Konvertierung in dataframes für ggplot (langsam!)
var_df2 <- as(var_rast2, "SpatialPixelsDataFrame")
var_df2 <- spTransform(var_df2, CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
var_df2 <- as.data.frame(var_df2)

#PLOT 1: Shape + Raster
p1 <- ggplot() +
  geom_polygon(data = area, aes(x=long, y=lat, group=group), fill=NA, color="grey50", size=0.25) +
  geom_tile(data = var_df, aes(x=x, y=y, fill=DE_Frosttage)) +
  geom_polygon(data = var_poly2, aes(x=long, y=lat, group=group, fill=rep(var_poly2$FR_Frosttage, each = 5)), size = 0) +
  scale_fill_distiller(palette = "Blues") + #nutze RColorBrewer-Paletten für kont. Werte
  theme_map() + #Theme nimmt Voreinstellungen vor: Fonts, Schriftgrößen, Labels
  theme(legend.position = "bottom") +
  theme(legend.key.height = unit(0.5, "cm")) +
  theme(legend.key.width = unit(1.5, "cm")) +
  labs(x = "Rechtswert", y = "Hochwert", fill = "Frosttage")
p1 + coord_equal(xlim=c(3340000, 3500000), ylim=c(5210000, 5480000)) #1:1-Ratio von x:y & Definition Mapausschnitt

geom_tile(data = basemap_df, aes(x=x, y=y, fill=Georhena_basemap_GK3)) +
  scale_colour_gradientn(colours=c("#d5c7a7", "#748b70", "#e0b072", "#9d6651", "#794e3e", "#efebe9"),
                       values=c(0, 0.167, 0.357, 0.7, 0.9, 1))
#Experiment mit coord_map. Korrekte Parameter konnte ich leider nicht ermitteln -> geht nicht
#p1 + geom_tile(data = var_df2, aes(x=x, y=y, fill=FR_Frosttage)) + coord_map("lambert", lat0=47, lat1=49.5, orientation=c(87.6,0,255))

#PLOT 2: Nur Shapes
p2 <- ggplot() +
  geom_polygon(data = var_poly, aes(x=long, y=lat, group=group, fill = rep(var_poly$DE_Frosttage, each = 5)), size = 0) +
  geom_polygon(data = var_poly2, aes(x=long, y=lat, group=group, fill = rep(var_poly2$FR_Frosttage, each = 5)), size = 0) +
  geom_polygon(data = area, aes(x=long, y=lat, group=group), color = "grey50", size = 0.5, fill = NA) +
  scale_fill_distiller(palette = "RdYlBu", direction = 1) +
  #scale_fill_gradientn("RasterValues", colors = heat.colors(255)) + #Andere Color-Gradient-Methode
  theme_map() + #Theme nimmt Voreinstellungen vor: Fonts, Schriftgrößen, Labels
  theme(legend.position = "bottom") +
  theme(legend.key.height = unit(0.5, "cm")) +
  theme(legend.key.width = unit(1.5, "cm")) +
  labs(fill = "Frosttage") +
  coord_fixed(1) #Height-Width Pixel Ratio
  #coord_cartesian(xlim=c(3400000, 3450000), ylim=c(5400000, 5450000)) #Zoom to Coordinates, dann kein coord_fixed()
p2

#Speichern von ggMaps
ggsave(p, file="AMAP.png", width = 4.5, height = 6, type = "cairo-png")
