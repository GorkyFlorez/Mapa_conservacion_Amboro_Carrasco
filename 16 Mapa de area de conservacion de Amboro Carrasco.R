# Librerias
library(raster)
library(sf)
library(ggspatial)
library(sp)
library(ggplot2)
library(tmap)
library(maptools)
library(elevatr)
# Cargamos data
Bol_dep    <- getData('GADM', country='Bolivia', level=1) %>%st_as_sf() 
Bolivia    <- getData('GADM', country='Bolivia', level=0) %>%st_as_sf() 
Amboro     <- st_read ("SHP/Bolivia/Amboro.shp") 
Carrasco   <- st_read ("SHP/Bolivia/Carrasco1.shp") 
Amboro.Carrasco <- st_union(Amboro , Carrasco)
Amboro.Carrasco_box = st_as_sfc(st_bbox(Amboro.Carrasco))
elevation  = get_elev_raster(Amboro.Carrasco,z=8)
Amb        <- crop(elevation, Amboro.Carrasco)
# Creacion del DEM
slope = terrain(Amb , opt = "slope") 
aspect = terrain(Amb , opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)

Bol=ggplot()+
  geom_sf(data = Bol_dep, fill="gray", color="white")+
  geom_sf(data = Bolivia, fill=NA)+
  geom_sf(data = Amboro.Carrasco, fill="black", size=0.3)+
  geom_sf(data = Amboro.Carrasco_box , fill=NA, size=0.3, color="deepskyblue4")+
  theme_void()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))
Bol.grob <- ggplotGrob(Bol)


MAP=tm_shape(hill) +
  tm_raster(palette = gray(0:10 / 10), style = "cont", legend.show = FALSE)+
  tm_shape(Amboro)+
  tm_borders("white",lwd=2)+
  tm_text("NAME",size = .9, col="black",shadow=TRUE,
          bg.color="white", bg.alpha=.45)+
  tm_shape(Carrasco)+
  tm_borders("white",lwd=2)+
  tm_text("NAME",size = .9, col="black",shadow=TRUE,
          bg.color="white", bg.alpha=.45)+
  tm_compass(type="arrow", position=c(.15, .05))+
  tm_scale_bar(position = c(0.2, .005), size=.8)+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 2, position = c(0.005, 0.05))+
  tm_grid(col = "grey",ticks = T, labels.col = "black")+
  tm_layout(title = "MAPA de RELIEVE", fontfamily = "serif",
            title.position =  c(.5, .9))+
  tm_credits("Data: DEM SRTM \n#Aprende R desde Cero Para SIG \nGorky Florez Castillo",  bg.color="white", bg.alpha=.45,
             position = c(0.57, 0.0001), col = "black", fontface="bold", fontfamily = "serif")

g3= tmap_grob(MAP)

# Mapa final
library(cowplot)
im=ggdraw() +
  coord_equal(xlim = c(0, 21), ylim = c(0, 10), expand = FALSE) +
  draw_plot(g3, width = 18, height = 18,x = 2.1, y = -4)+
  draw_plot(Bol.grob, width = 3, height =3 ,x = 18, y = 6.5)

# Exportacion
ggsave(plot = im ,"MAPAS/bolivia.png",
       units = "cm", width = 21,height = 10, dpi = 900)# guardar grafico  
  
