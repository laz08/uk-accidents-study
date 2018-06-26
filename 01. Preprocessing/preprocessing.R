
# Change this if we want to execute the computation of aggreggation, which takes more than 2h.
EXECUTE_AGGREGATION = FALSE
#EXECUTE_AGGREGATION = TRUE

DELETE_UNUSED_DF = TRUE
#DELETE_UNUSED_DF = FALSE

IMPUTE_MISSING_VALUES = FALSE
#IMPUTE_MISSING_VALUES = TRUE

READ_FINAL_DF = TRUE
#READ_FINAL_DF = FALSE


# Our working directories.
wd = getwd()
if(grepl("nora", wd)) {
    setwd("/home/nora/git/adm-project/report01")
} else {
  setwd("~/Desktop/ADM/adm-project/report01")
}
rm(wd)

###  ---------- HANDY FUNCTIONS  ---------- 

requiredPackages <- c("DMwR",
                      "ggplot2",
                      "lubridate",
                      "dplyr")

for (pac in requiredPackages) {
    if(!require(pac,  character.only=TRUE)){
        install.packages(pac, repos="http://cran.rstudio.com")
        library(pac,  character.only=TRUE)
    } 
}
rm(pac)
rm(requiredPackages)


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

keepColsByName <- function(df, columnNames) {
    indexes = c()
    for(col in columnNames) {
        indexes = append(indexes, getIndexByName(df, col))
    } 
    return (subset(df, select = indexes))
}

selectVehicleType <- function(typesVec) {

    motorcycles = c(2, 3, 4, 5, 22, 23, 97)
    trucks = c(10, 11, 18, 20, 21, 98)
    car = c(8, 9, 19)
    animal = c(1, 16)
    number_of_motorcycles = 0
    number_of_trucks = 0
    number_of_cars = 0
    number_of_animals = 0
    total_vehicles = 0
    
    for (vehicle in typesVec) {
        number_of_motorcycles = number_of_motorcycles + as.numeric(vehicle %in% motorcycles)
        number_of_trucks = number_of_trucks + as.numeric(vehicle %in% trucks)
        number_of_cars = number_of_cars + as.numeric(vehicle %in% car)
        number_of_animals = number_of_animals + as.numeric(vehicle %in% animal)
        total_vehicles = total_vehicles + 1
    }

    mixed_accident = (total_vehicles > number_of_motorcycles) * as.numeric(number_of_motorcycles > 0) |
                     (total_vehicles > number_of_trucks) * as.numeric(number_of_trucks > 0) |
                     (total_vehicles > number_of_cars) * as.numeric(number_of_cars > 0) |
                     (total_vehicles > number_of_animals) * as.numeric(number_of_animals > 0)

    if (mixed_accident == FALSE && (number_of_motorcycles > 0)) {
        return(1)
    }
    else if (mixed_accident == FALSE && (number_of_cars)) {
        return(2)
    }
    else if (mixed_accident == FALSE && (number_of_trucks)) {
        return(3)
    }
    else if (mixed_accident == FALSE && (number_of_animals)) {
        return(4)
    }
    
    else return(5)
}

simplifyAccidentColumn <- function(column, elements, asGoodBadString) {
    
    naTmp = which(is.na(column))
    tf = as.logical(column %in% elements)
    if(asGoodBadString) {
        tf = replace(tf, tf == FALSE, "BAD")
        tf = replace(tf, tf == TRUE, "GOOD")
        tf <- as.factor(tf)
    }
    for(naIdx in naTmp) {
        tf[naIdx] <- NA
    }
    
    return (tf)
}

simplifyAccidents <- function(clean_accidents) {
  
    light = c(1)
    clean_accidents$Light_Conditions = simplifyAccidentColumn(clean_accidents$Light_Conditions, light, TRUE)
    goodWeather = c(1)
    clean_accidents$Weather_Conditions = simplifyAccidentColumn(clean_accidents$Weather_Conditions, goodWeather, TRUE)
    goodRoadSurface = c(1)
    clean_accidents$Road_Surface_Conditions = simplifyAccidentColumn(clean_accidents$Road_Surface_Conditions, goodRoadSurface, TRUE)
    specialConditions = c(1:7)
    clean_accidents$Special_Conditions_at_Site = simplifyAccidentColumn(clean_accidents$Special_Conditions_at_Site, specialConditions, FALSE)
    hazard = c(1:7)
    clean_accidents$Carriageway_Hazards = simplifyAccidentColumn(clean_accidents$Carriageway_Hazards, hazard, FALSE)
    
    clean_accidents$Light_Conditions <- as.factor(clean_accidents$Light_Conditions)
    clean_accidents$Weather_Conditions <- as.factor(clean_accidents$Weather_Conditions)
    clean_accidents$Road_Surface_Conditions <- as.factor(clean_accidents$Road_Surface_Conditions)
    
    droplevels(clean_accidents$Light_Conditions)
    droplevels(clean_accidents$Weather_Conditions)
    droplevels(clean_accidents$Road_Surface_Conditions)
    
    return(clean_accidents)
}



aggregateCasualties <- function(df, accidents){
    
    ids <- accidents$Accident_Index
    
    number_of_casualties = c()
    male_casualties = c()
    female_casualties = c()
    age_disparity = c()
    worst_severity = c()
    num_drivers = c()
    num_passengers = c()
    num_pedestrians = c()
    
    
    for(id in ids){
        rows_id = df[df$Accident_Index ==  id, ]
        
        number_of_casualties = append(number_of_casualties,
                                      nrow(rows_id))
        
        male_casualties = append(male_casualties, 
                                 nrow(rows_id[rows_id$Sex_of_Casualty == 1,]))
        
        female_casualties = append(female_casualties,
                                   nrow(rows_id[rows_id$Sex_of_Casualty == 2,]))
        
        if(nrow(rows_id) > 0) {
            
            # Age of casualty disparity
            
            age_disparity = append(age_disparity, 
                                   max(rows_id$Age_of_Casualty) - min(rows_id$Age_of_Casualty))
            
            #Worst severity
            worst_severity = append(worst_severity,
                                    min(rows_id$Casualty_Severity))
            
        } else {
            age_disparity = append(age_disparity, 0)
            worst_severity = append(age_disparity, 0) # None
        }
        
        num_drivers = append(num_drivers, 
                             nrow(rows_id[rows_id$Casualty_Class == 1,]))
        
        num_passengers = append(num_passengers, 
                                nrow(rows_id[rows_id$Casualty_Class == 2,]))
        
        num_pedestrians = append(num_pedestrians, 
                                 nrow(rows_id[rows_id$Casualty_Class == 3,]))
        
        
    }
    
    newDF = data.frame(totalNumCasualties = number_of_casualties,
                       maleCasualties = male_casualties,
                       femaleCasualties = female_casualties,
                       ageDisparityBetweenCas = age_disparity,
                       worstCasualtySeverity = worst_severity,
                       numCasualtiesDriver = num_drivers,
                       numCasualtiesPassengers = num_passengers,
                       numCasualtiesPedestrians = num_pedestrians)
    
    return(newDF)
}

aggregateVehicles <- function(df, accidents) {
    
    accidents_id <- accidents$Accident_Index
    
    
    inv_veh_type <- c()
    num_male_driver <- c()
    num_female_driver <- c()
    disparity_age_driver <- c()
    disparity_age_vehicle <- c()
    
    for(id in accidents_id) {
        rows_id = clean_vehicles[clean_vehicles$Accident_Index == id, ]
        num_male_driver = append(num_male_driver, nrow(rows_id[rows_id$Sex_of_Driver == 1, ]))
        num_female_driver = append(num_female_driver, nrow(rows_id[rows_id$Sex_of_Driver == 2, ]))
        disparity_age_driver = append(disparity_age_driver, max(rows_id$Age_Band_of_Driver) - min(rows_id$Age_Band_of_Driver))
        disparity_age_vehicle = append(disparity_age_vehicle, max(rows_id$Age_of_Vehicle) - min(rows_id$Age_of_Vehicle))
        
        inv_veh_type <- append(inv_veh_type, selectVehicleType(rows_id$Vehicle_Type))  
    }
    
    newDF = data.frame(accidentId = accidents_id, 
                       involvedVehType = inv_veh_type,
                       numMaleDriver = num_male_driver,
                       numFemaleDriver = num_female_driver,
                       disparityAgeDriver = disparity_age_driver,
                       disparityAgeVehicle = disparity_age_vehicle)
    
    return(newDF)
    
}




###  ---------- READING DATASETS  ---------- 

## Datasets
# We have only chosen to read the accidents and casualties datasets. Involved vehicles only useful to know how many vehicles are involved and their type.

casualties_df <- read.delim("../datasets/Cas.csv", sep=",", header = TRUE, na.strings=c("","NA", -1))
accidents_df <- read.delim("../datasets/dftRoadSafety_Accidents_2016.csv", sep=",",  header = TRUE, na.strings=c("","NA", -1))
involved_veh_df <- read.delim("../datasets/MakeModel2016.csv", sep=",", header = TRUE, na.strings=c("","NA", -1))

### ----------- FEATURES SELECTION ---------------

accidents_cols_to_rm = c(
  "LSOA_of_Accident_Location",
  "Location_Easting_OSGR",
  "Location_Northing_OSGR",
  "Day_of_Week",
  "Local_Authority_.District.",
  "Local_Authority_.Highway.",
  "X1st_Road_Number",
  "X1st_Road_Class",
    "X2nd_Road_Number",
  "X2nd_Road_Class",
  "Pedestrian_Crossing.Human_Control", # Histogram shows only 1% had no human control
  "Pedestrian_Crossing.Physical_Facilities",
  "Police_Force",
  "Urban_or_Rural_Area"
  )


casualties_cols_to_rm = c(
    "Vehicle_Reference",
    "Casualty_Reference",
    "Age_Band_of_Casualty",
    "Pedestrian_Movement", 	
    "Car_Passenger",			# Already treated
    "Bus_or_Coach_Passenger",	# Already treated
    "Pedestrian_Road_Maintenance_Worker",
    "Casualty_Type",            # Already treated.
    "Casualty_Home_Area_Type",	
    "Casualty_IMD_Decile",	
    "Pedestrian_Location"
)

vehicles_cols_to_keep = c("Accident_Index",
                          "Sex_of_Driver", 
                          "Age_Band_of_Driver",
                          "Age_of_Vehicle",
                          "Vehicle_Type" )



clean_accidents = removeColsByName(accidents_df, accidents_cols_to_rm)
clean_casualties <- removeColsByName(casualties_df, casualties_cols_to_rm)
clean_vehicles <- keepColsByName(involved_veh_df, vehicles_cols_to_keep)

# Clean vehicles names correction
colnames(clean_vehicles)[colnames(clean_vehicles)=="X...Accident_Index"] <- "Accident_Index"
colnames(clean_vehicles)



# ----------- MEMORY FREE -------------------
# 
## We remove the unused dataframes to free memory.
if(DELETE_UNUSED_DF) {
    rm(accidents_df)
    rm(casualties_df)
    rm(involved_veh_df)
    
    
    rm(casualties_cols_to_rm)
    rm(accidents_cols_to_rm)
    rm(vehicles_cols_to_keep)
}

# ----------------  CORRECTIONS  ---------------------

## Study of "clean" datasets
dim(clean_accidents)
dim(clean_casualties)
dim(clean_vehicles)


## ------- ACCIDENTS --------
## 
## 

# Treating NA
nrow(clean_accidents[!complete.cases(clean_accidents),])
# Too much! 57k rows with missing values. 56k of which are from Junction Control.
# So we remove that variable and impute other values.
clean_accidents = removeColsByName(clean_accidents, c("Junction_Control"))
nrow(clean_accidents[!complete.cases(clean_accidents),])
# Now only 1095 rows with missing data.
# We can impute some of them.

# First we remove the NULL rows, for that data cannot be imputed with Knn or MICE or any similar method (it's not a continous variable)
clean_accidents = clean_accidents[!clean_accidents$Speed_limit=="NULL",]
clean_accidents <- droplevels(clean_accidents)

# We also remove the missing rows with longitude/latitude NAs, for they are not continuous and cannot be imputed.
clean_accidents = clean_accidents[!is.na(clean_accidents$Longitude),]
clean_accidents = clean_accidents[!is.na(clean_accidents$Latitude),]

# Same with time
clean_accidents = clean_accidents[!is.na(clean_accidents$Time),]

# Transform to factors
clean_accidents$Accident_Severity = as.factor(clean_accidents$Accident_Severity)
clean_accidents$Number_of_Vehicles = as.factor(clean_accidents$Number_of_Vehicles)
clean_accidents$Junction_Detail = as.factor(clean_accidents$Junction_Detail)
clean_accidents$Light_Conditions = as.factor(clean_accidents$Light_Conditions)
clean_accidents$Weather_Conditions = as.factor(clean_accidents$Weather_Conditions)
clean_accidents$Road_Surface_Conditions = as.factor(clean_accidents$Road_Surface_Conditions)
clean_accidents$Special_Conditions_at_Site = as.factor(clean_accidents$Special_Conditions_at_Site)
clean_accidents$Carriageway_Hazards = as.factor(clean_accidents$Carriageway_Hazards)
clean_accidents$Did_Police_Officer_Attend_Scene_of_Accident = as.factor(clean_accidents$Did_Police_Officer_Attend_Scene_of_Accident)
clean_accidents$Road_Type = as.factor(clean_accidents$Road_Type)
clean_accidents$Number_of_Casualties = as.factor(clean_accidents$Number_of_Casualties)


summary(clean_accidents)
dim(clean_accidents)

## ACCIDENTS SIMPLIFICATION
clean_accidents = simplifyAccidents(clean_accidents)


# ---- IMPUTING --------
# Proceeding to impute

((100* nrow(clean_casualties[!complete.cases(clean_casualties),])) / nrow(clean_casualties))
nrow(clean_casualties[!complete.cases(clean_casualties),])
sum(is.na(clean_casualties))


((100 * nrow(clean_vehicles[!complete.cases(clean_vehicles),])) / nrow(clean_vehicles))
nrow(clean_vehicles[!complete.cases(clean_vehicles),])
sum(is.na(clean_vehicles))

((100 * nrow(clean_accidents[!complete.cases(clean_accidents),])) / nrow(clean_accidents))
nrow(clean_accidents[!complete.cases(clean_accidents),])
sum(is.na(clean_accidents))

if(IMPUTE_MISSING_VALUES & !READ_FINAL_DF) {
    clean_accidents = knnImputation(data = clean_accidents, k = 11)    
}
    
summary(clean_accidents)



## ------- CASUALTIES --------
summary(clean_casualties)

if(EXECUTE_AGGREGATION) {
    
    casualties_aggregated = aggregateCasualties(clean_casualties, clean_accidents)
    write.csv2(casualties_aggregated, file = "./newDatasets/aggregated_casualties.csv")
} else {
    casualties_aggregated = read.delim("./newDatasets/aggregated_casualties.csv", sep=";", header = TRUE)
    casualties_aggregated = removeColsByName(casualties_aggregated, c("X"))
}

## ------- VEHICLES --------
## 
summary(clean_vehicles)

if(EXECUTE_AGGREGATION) {
    vehicles_aggregated = aggregateVehicles(clean_vehicles, clean_accidents)
    write.csv2(vehicles_aggregated, file = "./newDatasets/aggregated_vehicles.csv")
} else {
    vehicles_aggregated = read.delim("./newDatasets/aggregated_vehicles.csv", sep=";", header = TRUE)
    vehicles_aggregated = removeColsByName(vehicles_aggregated, c("X"))
    vehicles_aggregated = removeColsByName(vehicles_aggregated, c("accidentId"))
}


 # ----------  MERGING  ---------------
# Now we are going to merge the three tables.
names(clean_accidents)
names(casualties_aggregated)
names(vehicles_aggregated)

if(READ_FINAL_DF){
    final_df = read.delim("./newDatasets/final_merged_df.csv", sep=";", header = TRUE)
    final_df = removeColsByName(final_df, c("X"))
    final_df[17:30] <- lapply(final_df[17:30] , factor)
} else {
    final_df = cbind(clean_accidents, casualties_aggregated)
    final_df = cbind(final_df, vehicles_aggregated)
    
    final_df[17:30] <- lapply(final_df[17:30] , factor)
    write.csv2(final_df, file = "./newDatasets/final_merged_df.csv")
}

colors = c("#F42A46", "#94C160", "#FF9900", "#80DFFF", "#227D97", "#AA5139")
par(mfrow=c(3, 2), mar = rep(2, 4))
displayBarplotVariables =  c("Light_Conditions", "Carriageway_Hazards", "Weather_Conditions", "Road_Surface_Conditions", "Special_Conditions_at_Site")
for (variable in displayBarplotVariables) {
  barplot(table(na.omit(final_df[variable])), main=variable, col = colors)
}

par(mfrow=c(1, 3), mar = rep(2, 4))
displayBoxplotVariables = c("Number_of_Vehicles", "Number_of_Casualties", "Accident_Severity")
for (variable in displayBoxplotVariables) {
  barplot(table(na.omit(final_df[variable])), main=variable, col = colors)
}

par(mfrow=c(3, 2), mar = rep(2, 4))
displayHistogramVariables = c("Road_Type", "Speed_limit", "Junction_Detail", "disparityAgeDriver", "disparityAgeVehicle")
for (variable in displayHistogramVariables) {
  barplot(table(na.omit(final_df[variable])), main=variable, col = colors)
}

names(final_df)
dim(final_df)

summary(final_df)



# -------- PLOTS -----------

# Accidents by time
#timedf = as.data.frame(table(final_df$Time))

datehour = cut(as.POSIXct(paste(final_df$Date, final_df$Time),
                          format="%d/%m/%Y %H:%M"), breaks="hour")

datehour = as.data.frame(table(datehour))
head(table(datehour))


hourdf = cbind(datehour, hour_of_day = hour(as.POSIXct(strptime(datehour$datehour, "%Y-%m-%d %H:%M:%S"))))
hourdf = removeColsByName(hourdf, c("datehour"))

aggregated_df_hour = data.frame()
hours = seq(0, 23, 1)
freq = c()
for (i in hours) {
    freq = append(freq, sum(hourdf[hourdf$hour_of_day == i,]))
}
aggregated_df_hour = data.frame(hours = hours, freqs = freq)

colorsV = c("Hour" = "#F42A46")
ggplot(aggregated_df_hour, aes(aggregated_df_hour$hours, aggregated_df_hour$freqs, group = 1)) + 
    geom_line(data = aggregated_df_hour, aes(colour="Hour")) +
    scale_colour_manual("", 
                        values = colorsV) +
    ylab("Accidents") +
    xlab("Hour") +
    theme(axis.text.x=element_text(angle = 90, hjust = 0)) +
    ggtitle("Accidents during year by hour") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"))
# 
