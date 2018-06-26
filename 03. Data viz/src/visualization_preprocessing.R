READ_EXISTING_DATASET = TRUE

## ------- CONFIG -------
# Load and install necessary packages
requiredPackages <- c("readr",
                      "ggplot2",
                      "GGally",
                      "corrplot",
                      "lubridate",
                      "vcd",
                      "FactoMineR",
                      "factoextra",
                      "fastcluster",
                      "ggparallel",
                      "leaflet",
                      "leaflet.extras",
                      "data.table")

for (pac in requiredPackages) {
    if(!require(pac,  character.only=TRUE)){
        install.packages(pac, repos="http://cran.rstudio.com")
        library(pac,  character.only=TRUE)
    } 
}
rm(pac)
rm(requiredPackages)

## ------- WORKING PATH -------
# Our working directories.
wd = getwd()
if(grepl("nora", wd)) {
    setwd("/home/nora/git/adm-project/report03")
    Sys.setlocale("LC_TIME", "en_US.utf8")   # Modern Linux etc.
} else {
    setwd("~/Desktop/ADM/adm-project/report03")
    Sys.setlocale("LC_TIME", "en_US")  # OS X, in UTF-8
}
rm(wd)



## ------- FUNCTIONS -------
getIndexByName <- function(df, name) {
    return (grep(name, colnames(df)))
}

removeColsByName <- function(df, columnNames) {
    indexes = c()
    for(col in columnNames) {
        indexes = append(indexes, getIndexByName(df, col))
    } 
    return (subset(df, select = -indexes))
}

getMultipleIndexByName <- function(df, columnNames) {
    indexes = c()
    for(col in columnNames) {
        indexes = append(indexes, getIndexByName(df, col))
    }
    return (indexes)
}

get_Color <- function(clusters) {
    sapply(clusters, function(cluster) {
        if (cluster == 1) return('red')
        if (cluster == 2) return('green')
        if (cluster == 3) return('blue')
    })
}

if(!READ_EXISTING_DATASET) {
    ## ------- PREPROCESSING -------
    df <- read_delim("../report01/newDatasets/final_merged_df.csv", ";", escape_double = FALSE, trim_ws = TRUE)
    #View(df)
    
    # Removing unused columns
    df = removeColsByName(df, c("X1", "Accident_Index", "worstCasualtySeverity"))
    #df = removeColsByName(df, c("X1", "Accident_Index", "Longitude", "Latitude" , "worstCasualtySeverity"))
    
    final_df = df
    
    # Use only the hour, removing the minutes...
    hours =  hour(as.POSIXct(strptime(final_df$Time, "%H:%M:%S")))
    head(hours)
    final_df$Time = hours
    
    
    # Keep month and dey of the week
    
    date = as.Date(final_df$Date, "%d/%m/%Y")
    months = format(date, "%m")
    
    dayOfWeek =  wday(date, label=TRUE)
    dayOfWeek = as.character(dayOfWeek)
    
    isWeekend = dayOfWeek
    length(dayOfWeek)
    for (i in seq(1, length(dayOfWeek))) {
        isWeekend[i] = ifelse(dayOfWeek[i] == "Sun" || dayOfWeek[i] == "Sat", "WEEKEND", "WORKDAY")
    }
    
    final_df = cbind(final_df, Is_Weekend=isWeekend)
    final_df = cbind(final_df, Day_of_Week = dayOfWeek)
    final_df = cbind(final_df, Month = months)
    
    View(final_df)
    
    
    ## Categorical values instead of numerical ones
    accident_severity = c("FATAL", "SERIOUS", "SLIGHT")
    road_type = c("Roundabout", "One_Way_Street", "Dual_Carriageway", "", "", "Single_Carriageway", "Slip_Road", "", "Unknown", "", "", "One_way/Slip_Road")
    junction_detail = c("No_junction", "Roundabout", "Mini_Roundabout", "T", "Slip_Road", "Crossroads", "More_4_Arms", "Private_entrance", "Other_junction")
    vehicle_type = c("Motorcycle", "car", "trucks", "animals", "mixed")
    worst_casualty_severity = c("Fatal", "Serious", "Slight")
    police_attended = c("Yes", "No", "No-SelfCompletionForm")
    
    
    final_df$Accident_Severity = accident_severity[final_df$Accident_Severity]
    final_df$Road_Type = road_type[final_df$Road_Type]
    final_df$Junction_Detail = junction_detail[final_df$Junction_Detail+1]
    final_df$involvedVehType = vehicle_type[final_df$involvedVehType]
    # final_df$worstCasualtySeverity = worst_casualty_severity[final_df$worstCasualtySeverity]
    final_df$Did_Police_Officer_Attend_Scene_of_Accident = police_attended[final_df$Did_Police_Officer_Attend_Scene_of_Accident]
    
    
    # CATEGORICAL VALUES for each case
    final_df$Light_Conditions = ifelse(final_df$Light_Conditions == "GOOD", "GOOD_LIGHT", "BAD_LIGHT")
    final_df$Weather_Conditions = ifelse(final_df$Weather_Conditions == "GOOD", "GOOD_WEATHER", "BAD_WEATHER")
    final_df$Road_Surface_Conditions = ifelse(final_df$Road_Surface_Conditions == "GOOD", "GOOD_ROAD", "BAD_ROAD")
    final_df$Special_Conditions_at_Site = ifelse(final_df$Special_Conditions_at_Site == "TRUE", "SPECIAL_COND", "NO_SPECIAL_COND")
    final_df$Carriageway_Hazards = ifelse(final_df$Carriageway_Hazards == "TRUE", "ROAD_HAZARD", "NO_ROAD_HAZARD")
    final_df$Did_Police_Officer_Attend_Scene_of_Accident = ifelse(final_df$Did_Police_Officer_Attend_Scene_of_Accident == "Yes", "POLICE_ATTENDED", "NO_POLICE")
    
    # Remove date column
    #final_df = removeColsByName(final_df, c("Date"))
    
    # Remove NA.
    nrow(final_df)
    cleanDF = final_df[complete.cases(final_df), ]
    nrow(cleanDF)
    #final_df[is.na(final_df)] <- " "
    
    View(cleanDF)
    write.csv2(cleanDF, file = "../report03/datasets/visualization_dataset.csv", row.names=FALSE)

} else {
    
    final_df = read.csv2("../report03/datasets/visualization_dataset.csv")
    final_df$Latitude <- final_df$Latitude/1000000

}

final_df <- removeColsByName(final_df, c("Number_of_Casualties"))
dimnames <- names(final_df)


# Num of dim
dim(final_df)
# 68787    30


# MCA
cleanDf = removeColsByName(final_df, c("Latitude", 
                                      "Longitude",
                                      "Date"))

quanti_sup_vars = seq(13, 19)
quanti_sup_vars = append(quanti_sup_vars, c(21, 22, 2))

mca_df = as.data.frame(lapply(cleanDf , factor))

for (i in quanti_sup_vars) {
    mca_df[, i] <- as.numeric(mca_df[, i])
    
}

# View(mca_df)
# mca_df = subset(mca_df, select = -quanti_sup_vars)
if(FALSE) {
mca.df <- MCA(mca_df[],
#mca.df <- MCA(mca_df[, 1:12],
              ncp = 100,
             quali.sup = c(25), # Workday supl to day of week
             quanti.sup = quanti_sup_vars,
              graph=FALSE)

fviz_screeplot(mca.df)

fviz_mca_biplot(mca.df,labelsize = 3)

# Correlation between variables and principal dimensions
fviz_mca_var(mca.df, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
# Biplot
fviz_mca(mca.df)

fviz_contrib(mca.df, choice="var", axes = 1:2, top = 10)
fviz_contrib(mca.df, choice ="ind", axes = 1, top = 30)
# Nothing can be clearly explained

eigen = as.data.frame(mca.df$eig)
(eigen$`cumulative percentage of variance`[3])


plot(mca.df$eig[,1], type='l', ylab = NA, xlab=NA)
title("Eigenvalues", line = 0.7)
# Percentage (+ accumulated)

meanEig <- mean(mca.df$eig[,1])
recompEign <- mca.df$eig[mca.df$eig[,1]>meanEig, 1]
recompEign <- recompEign - meanEig

# PAG 67
plot(recompEign/sum(recompEign), type="l")
cumsum(100*recompEign/sum(recompEign))


nd = 23
### Scatterplot ###
View(cleanDf)
}

numeric_data = c(1, 2, 3, 5, 16, 23)
numeric_data = c(2, 3, 5)
numeric_data = append(numeric_data, seq(13, 19))
numeric_data = append(numeric_data, seq(21, 24))
numeric_data = append(numeric_data, 27)

spDf = subset(cleanDf, select = numeric_data)
# ggpairs(spDf[100, ], aes(colour = Accident_Severity, alpha = 0))

# http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
corrMat = round(cor(spDf), 2)
corrplot(corrMat, method = "ellipse", type = "upper", order = "hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45) # Text label color and rotation)

summary(spDf$numFemaleDriver); table(spDf$numFemaleDriver)
summary(spDf$numMaleDriver); table(spDf$numMaleDriver)

spDf2 = removeColsByName(spDf, c("totalNumCasualties", "numCasualtiesPassengers", "numMaleDriver", "numFemaleDriver", "numCasualtiesPedestrians", "disparityAgeVehicle"))

spDf2 = cbind(spDf2, Accident_Severity = cleanDf$Accident_Severity)
if(FALSE){
    ggpairs(spDf2[, ], aes(colour = Accident_Severity, alpha = 0))    
}

mosaic(spDf, shade=TRUE, legend=TRUE) 

# Clustering

if(FALSE){
d <- dist(rbind(mca.df$ind$coord[,1:nd]), method = "euclidean")
hc <- fastcluster::hclust(d, method = "ward.D2")
barplot(hc$height, main="Heights")
abline(h=9.5, col = 2)

plot(as.dendrogram(hc), labels=NULL)
abline(h=9.5, col = 2)

nc = 3
c1 <- cutree(hc,nc)

cdg <- aggregate(as.data.frame(mca.df$ind$coord[,1]),list(c1),mean)[,2:(nd+1)]
Bss <- sum(rowSums(cdg^2)*as.numeric(table(c1)))
Tss <- sum(rowSums(mca.car$ind$coord[,1:9]^2))
Ib6 <- 100*Bss/Tss
Ib6

nc = 3
kmeans3.df = kmeans(mca.df$ind$coord[,1:23], nc)
plot(mca.df$ind$coord[,1],mca.df$ind$coord[,2],main=paste("Clustering in", toString(nc),"classes (Kmeans)"),col=kmeans3.df$cluster,pch=20,cex=1.0)
abline(h=0,v=0,col="gray")

catdesPlot3 = catdes(cbind(as.factor(kmeans3.df$cluster),mca_df[]),1)
catdesPlot3$category$`1`
catdesPlot3$category$`2`
catdesPlot3$category$`3`
as.data.frame(table(kmeans3.df$cluster))

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% #providers$CartoDB.DarkMatter
    addCircleMarkers(
        final_df$Longitude,
        final_df$Latitude, 
        radius = 2,
        color = get_Color(as.vector(kmeans3.df$cluster)),
        stroke = FALSE, fillOpacity = 0.6
    )

nc = 2
kmeans2.df = kmeans(mca.df$ind$coord[], nc)
plot(mca.df$ind$coord[,1],mca.df$ind$coord[,2],main=paste("Clustering in", toString(nc),"classes (Kmeans)"),col=kmeans2.df$cluster,pch=20,cex=1.0)
abline(h=0,v=0,col="gray")
catdesPlot2 = catdes(cbind(as.factor(kmeans2.df$cluster),mca_df[]),1)
catdesPlot2$category$`1`
catdesPlot2$category$`2`
as.data.frame(table(kmeans2.df$cluster))

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% #providers$CartoDB.DarkMatter
    #addWebGLHeatmap(lng=df$Longitude, lat=df$Latitude, size = 1000)
addCircleMarkers(
    lng= final_df$Longitude, 
    lat= final_df$Latitude,
    radius = 2,
    color = get_Color(as.vector(kmeans2.df$cluster)),
    stroke = FALSE, fillOpacity = 0.6
)

}


## GGPARALLEL ##
ggpdf = subset(mca_df, select = -quanti_sup_vars)
ggpdf = subset(ggpdf, select = -c(2, 4, 5, 13, 14, 16, 17)) 
View(ggpdf)
dim(ggpdf)

write.csv2(ggpdf, file = "../report03/datasets/categorical_dataset.csv", row.names=FALSE)

namesPlot = names(ggpdf)[seq(0, ncol(ggpdf)) ]
ggparallel(namesPlot, ggpdf, weight=1, asp=0.5, method="hammock", ratio=0.2) +
    theme( legend.position="none") +
    scale_fill_brewer(palette="Paired") +
    scale_colour_brewer(palette="Paired")
