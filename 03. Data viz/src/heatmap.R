library("leaflet")
library("leaflet.extras")
library("data.table")
library("sp")
library("rgdal")
# library("maptools")
library("KernSmooth")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
df = read.csv2('/Users/charlyo/Desktop/ADM/adm-project/report01/newDatasets/final_merged_df.csv', header=TRUE, sep=";")

cbind(as.numeric(df$Longitude), as.numeric(df$Latitude))

kde <- bkde2D(cbind(df$Longitude[0:20000], df$Latitude[0:20000]),
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

pgons <- lapply(1:length(CL), function(i)
    Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

leaflet(quakes) %>% addProviderTiles(providers$CartoDB.Positron) %>% #providers$CartoDB.DarkMatter
    addWebGLHeatmap(lng=df$Longitude, lat=df$Latitude, size = 1000)

