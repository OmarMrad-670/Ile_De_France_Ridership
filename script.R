---
  title: "Your Report Title"
author: "Your Name"
date: "Date"
output:
  quarto::quarto_document:
  code_folding: show
---

######### Library ####################
install.packages("readr")
library(readr)

install.packages("sf")
library(sf)

install.packages("dplyr")
library(dplyr)

install.packages("naniar")
library(naniar)

install.packages("readxl")
library(readxl)


library(ggplot2)

install.packages("lubridate")
library(lubridate)

############### Directory
setwd("C:/Users/ASUS/Desktop/Ile_De_France_Ridership_Study-master/datasets/")


#########Analyzing and Visualizing Ridership Patterns in Île-de-France Rail Network############

###### 1 : Data Collection and Cleaning #########

####### Collection

################################################### Data 2023 ############################

validation_data_2023 <- read_excel("validations-reseau-ferre-nombre-validations-par-jour-1er-semestre.xlsx")

View(validation_data_2023)
names(validation_data_2023)


remove_outliers <- function(data, column_name) {
  # Calculate quartiles and IQR
  Q1 <- quantile(data[[column_name]], 0.25)
  Q3 <- quantile(data[[column_name]], 0.75)
  IQR_value <- Q3 - Q1
  
  # Set the lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Remove outliers
  filtered_data <- data[!(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound), ]
  
  return(filtered_data)
}

validation_data_2023 <- remove_outliers(validation_data_2023, "lda")

################# Get unique values of 'lda' column
unique_values <- unique(validation_data_2023$lda)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)



################################################### Data 2022 ##################################

data_2022_S1_NB_FER <- read.delim("data-rf-2022/2022_S1_NB_FER.txt")
nrow(data_2022_S1_NB_FER)

################################### Get unique values of 'lda' column
unique_values <- unique(data_2022_S1_NB_FER$ID_REFA_LDA)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)
###################################

data_2022_S1_NB_FER <- remove_outliers(data_2022_S1_NB_FER, "ID_REFA_LDA")
names(data_2022_S1_NB_FER)



data_2022_S1_PROFIL_FER <- read.delim("data-rf-2022/2022_S1_PROFIL_FER.txt")
data_2022_S1_PROFIL_FER <- remove_outliers(data_2022_S1_PROFIL_FER, "ID_REFA_LDA")



data_2022_S2_Nb_FER <- read.delim("data-rf-2022/2022_S2_NB_FER.txt", sep = ";")
data_2022_S2_Nb_FER <- remove_outliers(data_2022_S2_Nb_FER, "lda")

data_2022_S2_PROFIL_FER <- read.delim("data-rf-2022/2022_S2_PROFIL_FER.txt", sep = ";")
data_2022_S2_PROFIL_FER <- remove_outliers(data_2022_S2_PROFIL_FER, "lda")

###### rename
names(data_2022_S2_Nb_FER)[names(data_2022_S2_Nb_FER) == "lda"] <- "ID_REFA_LDA"
names(data_2022_S2_PROFIL_FER)[names(data_2022_S2_PROFIL_FER) == "lda"] <- "ID_REFA_LDA"


##### Rbind
data_2022_NB_FER <- rbind(data_2022_S1_NB_FER, data_2022_S2_Nb_FER)
data_2022_PROFILL_FER <- rbind(data_2022_S1_PROFIL_FER, data_2022_S2_PROFIL_FER)




########################################### Data 2021 ##################################

data_2021_S1_NB_FER <- read.delim("data-rf-2021/2021_S1_NB_FER.txt")
data_2021_S1_NB_FER <- remove_outliers(data_2021_S1_NB_FER, "ID_REFA_LDA")

data_2021_S1_PROFIL_FER <- read.delim("data-rf-2021/2021_S1_PROFIL_FER.txt")
data_2021_S1_PROFIL_FER <- remove_outliers(data_2021_S1_PROFIL_FER, "ID_REFA_LDA")



data_2021_S2_Nb_FER <- read.delim("data-rf-2021/2021_S2_NB_FER.txt")
data_2021_S2_Nb_FER <- remove_outliers(data_2021_S2_Nb_FER, "ID_REFA_LDA")

data_2021_S2_PROFIL_FER <- read.delim("data-rf-2021/2021_S2_PROFIL_FER.txt")
data_2021_S2_PROFIL_FER <- remove_outliers(data_2021_S2_PROFIL_FER, "ID_REFA_LDA")



##### Rbind
data_2021_NB_FER <- rbind(data_2021_S1_NB_FER, data_2021_S2_Nb_FER)
data_2021_PROFILL_FER <- rbind(data_2021_S1_PROFIL_FER, data_2021_S2_PROFIL_FER)



################# Data 2020 ##################################

data_2020_S1_NB_FER <- read.delim("data-rf-2020/2020_S1_NB_FER.txt")
data_2020_S1_NB_FER <- remove_outliers(data_2020_S1_NB_FER, "ID_REFA_LDA")

data_2020_S1_PROFIL_FER <- read.delim("data-rf-2020/2020_S1_PROFIL_FER.txt")
data_2020_S1_PROFIL_FER <- remove_outliers(data_2020_S1_PROFIL_FER, "ID_REFA_LDA")




data_2020_S2_Nb_FER <- read.delim("data-rf-2020/2020_S2_NB_FER.txt")
data_2020_S2_Nb_FER <- remove_outliers(data_2020_S2_Nb_FER, "ID_REFA_LDA")

data_2020_S2_PROFIL_FER <- read.delim("data-rf-2020/2020_S2_PROFIL_FER.txt")
data_2020_S2_PROFIL_FER <- remove_outliers(data_2020_S2_PROFIL_FER, "ID_REFA_LDA")



##### Rbind
data_2020_NB_FER <- rbind(data_2020_S1_NB_FER, data_2020_S2_Nb_FER)
data_2020_PROFILL_FER <- rbind(data_2020_S1_PROFIL_FER, data_2020_S2_PROFIL_FER)






################# Data 2019 ##################################

data_2019_S1_NB_FER <- read.delim("data-rf-2019/2019_S1_NB_FER.txt")
data_2019_S1_NB_FER <- remove_outliers(data_2019_S1_NB_FER, "ID_REFA_LDA")



data_2019_S1_PROFIL_FER <- read.delim("data-rf-2019/2019_S1_PROFIL_FER.txt")
data_2019_S1_PROFIL_FER <- remove_outliers(data_2019_S1_PROFIL_FER, "ID_REFA_LDA")


data_2019_S2_Nb_FER <- read.delim("data-rf-2019/2019_S2_NB_FER.txt")
data_2019_S2_Nb_FER <- remove_outliers(data_2019_S2_Nb_FER, "ID_REFA_LDA")


data_2019_S2_PROFIL_FER <- read.delim("data-rf-2019/2019_S2_PROFIL_FER.txt")
data_2019_S2_PROFIL_FER <- remove_outliers(data_2019_S2_PROFIL_FER, "ID_REFA_LDA")

##### Rbind
data_2019_NB_FER <- rbind(data_2019_S1_NB_FER, data_2019_S2_Nb_FER)
data_2019_PROFILL_FER <- rbind(data_2019_S1_PROFIL_FER, data_2019_S2_PROFIL_FER)


################# Data 2018 ##################################


data_2018_S1_NB_FER <- read.delim("data-rf-2018/2018_S1_NB_FER.txt")
data_2018_S1_NB_FER <- remove_outliers(data_2018_S1_NB_FER, "ID_REFA_LDA")

data_2018_S1_PROFIL_FER <- read.delim("data-rf-2018/2018_S1_PROFIL_FER.txt")
data_2018_S1_PROFIL_FER <- remove_outliers(data_2018_S1_PROFIL_FER, "ID_REFA_LDA")


data_2018_S2_Nb_FER <- read.delim("data-rf-2018/2018_S2_NB_Fer.txt")
data_2018_S2_Nb_FER <- remove_outliers(data_2018_S2_Nb_FER, "ID_REFA_LDA")

data_2018_S2_PROFIL_FER <- read.delim("data-rf-2018/2018_S2_Profil_Fer.txt")
data_2018_S2_PROFIL_FER <- remove_outliers(data_2018_S2_PROFIL_FER, "ID_REFA_LDA")

##### Rbind
data_2018_NB_FER <- rbind(data_2018_S1_NB_FER, data_2018_S2_Nb_FER)
data_2018_PROFILL_FER <- rbind(data_2018_S1_PROFIL_FER, data_2018_S2_PROFIL_FER)

#########################################
nrow(data_2018_PROFILL_FER)



################# Data 2017 ##################################

data_2017_S1_NB_FER <- read.delim("data-rf-2017/2017S1_NB_FER.txt")
View(data_2018_S1_NB_FER)

data_2017_S1_NB_FER$NB_VALD <- ifelse(data_2017_S1_NB_FER$NB_VALD == "Moins de 5", "5", data_2017_S1_NB_FER$NB_VALD)
data_2017_S1_NB_FER$NB_VALD <- as.numeric(data_2017_S1_NB_FER$NB_VALD)


unique_values <- unique(data_2017_S1_NB_FER$ID_REFA_LDA)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)


unique_values <- unique(data_2017_S1_NB_FER$NB_VALD)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)





data_2017_S1_NB_FER <- data_2017_S1_NB_FER %>%
  filter(!(ID_REFA_LDA == "?" | ID_REFA_LDA == ""))


data_2017_S1_NB_FER$ID_REFA_LDA <- as.numeric(data_2017_S1_NB_FER$ID_REFA_LDA)
summary(data_2017_S1_NB_FER)

data_2017_S1_NB_FER <- remove_outliers(data_2017_S1_NB_FER, "ID_REFA_LDA")


summary(data_2017_S1_NB_FER)




############################ Get unique values of 'ID_REFA_LDA' column

data_2017_S1_PROFIL_FER <- read.delim("data-rf-2017/2017S1_PROFIL_FER.txt")


unique_values <- unique(data_2017_S1_PROFIL_FER$ID_REFA_LDA)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)




###### Remove "?" and ""
data_2017_S1_PROFIL_FER <- data_2017_S1_PROFIL_FER %>%
  filter(!(ID_REFA_LDA == "?" | ID_REFA_LDA == ""))

##### Transform character to numeric

data_2017_S1_PROFIL_FER$ID_REFA_LDA <- as.numeric(data_2017_S1_PROFIL_FER$ID_REFA_LDA)

summary(data_2017_S1_PROFIL_FER)


data_2017_S1_PROFIL_FER <- remove_outliers(data_2017_S1_PROFIL_FER, "ID_REFA_LDA")


data_2017_S2_Nb_FER <- read.delim("data-rf-2017/2017_S2_NB_FER.txt")
data_2017_S2_Nb_FER <- remove_outliers(data_2017_S2_Nb_FER, "ID_REFA_LDA")

unique_values <- unique(data_2017_S2_Nb_FER$ID_REFA_LDA)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)

unique_values <- unique(data_2017_S2_Nb_FER$NB_VALD)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)
summary(data_2017_S2_Nb_FER)

data_2017_S2_PROFIL_FER <- read.delim("data-rf-2017/2017_S2_PROFIL_FER.txt")
data_2017_S2_PROFIL_FER <- remove_outliers(data_2017_S2_PROFIL_FER, "ID_REFA_LDA")

##### Rbind
data_2017_NB_FER <- rbind(data_2017_S1_NB_FER, data_2017_S2_Nb_FER)
data_2017_PROFILL_FER <- rbind(data_2017_S1_PROFIL_FER, data_2017_S2_PROFIL_FER)

####################################
nrow(data_2017_PROFILL_FER)




################ Stops ########################

stops <- read_csv2("zones-d-arrets.csv")
View(stops)

################## Spatial data ###############################################

locations=st_read("REF_ZdA/PL_ZDL_R_05_01_2024.shp",crs=4326)
View(locations)

################### Merge Stops with Spatial #############################

stops_with_locations <- merge(stops, locations, by.x = "ZdAId", by.y = "id_refa")
View(stops_with_locations)



############# Check for missing values in the entire dataset

View(validation_data_2023)

missing_values <- validation_data_2023 %>%
  summarise_all(~sum(is.na(.)))


# Display the result
print(missing_values)


#columns_to_impute <- c("CODE_STIF_RES ", "CODE_STIF_ARRET ")
#validation_data_2023$CODE_STIF_RES[is.na(validation_data_2023$CODE_STIF_RES)] <- mean(validation_data_2023$CODE_STIF_RES, na.rm = TRUE)
#validation_data_2023$CODE_STIF_ARRET[is.na(validation_data_2023$CODE_STIF_ARRET)] <- mean(validation_data_2023$CODE_STIF_ARRET, na.rm = TRUE)

#=========================================> no missing value

####################### Replace ? by mode

clean_dataset <- function(df) {
 
  ############## Calculate the mode of the 'titre' column
  mode_value <- names(sort(table(df$CATEGORIE_TITRE), decreasing = TRUE))[1]
  
  ##################################### Replace "?" with mode value
  
  df$CATEGORIE_TITRE[df$CATEGORIE_TITRE == "?"] <- mode_value
  
  return(df)
}


######### Merge Data


validation_data_2023 <- clean_dataset(validation_data_2023)
data_2022_NB_FER <- clean_dataset(data_2022_NB_FER)
data_2021_NB_FER <- clean_dataset(data_2021_NB_FER)
data_2020_NB_FER <- clean_dataset(data_2020_NB_FER)
data_2019_NB_FER <- clean_dataset(data_2019_NB_FER)
data_2018_NB_FER <- clean_dataset(data_2018_NB_FER)
data_2017_NB_FER <- clean_dataset(data_2017_NB_FER)


names(validation_data_2023)[names(validation_data_2023) == "lda"] <- "ID_REFA_LDA"
View(validation_data_2023)


View(data_2017_NB_FER)

missing_values <- validation_data_2023 %>%
  summarise_all(~sum(is.na(.)))
print(missing_values)


missing_values <- data_2018_NB_FER %>%
  summarise_all(~sum(is.na(.)))
print(missing_values)

########################################## Update The Date

data_2022_NB_FER$JOUR <- format(as.Date(data_2022_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")

data_2021_NB_FER$JOUR <- format(as.Date(data_2021_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")

data_2020_NB_FER$JOUR <- format(as.Date(data_2020_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")

data_2019_NB_FER$JOUR <- format(as.Date(data_2019_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")

data_2018_NB_FER$JOUR <- format(as.Date(data_2018_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")

data_2017_NB_FER$JOUR <- format(as.Date(data_2017_NB_FER$JOUR, format = "%d/%m/%Y"), "%Y-%m-%d")

##########################################

View(validation_data_2023)
View(data_2022_NB_FER)
View(data_2021_NB_FER)
View(data_2020_NB_FER)
View(data_2019_NB_FER)
View(data_2018_NB_FER)
View(data_2017_NB_FER)

names(validation_data_2023)

#################################### Checking data 2017

summary(data_2017_NB_FER)

View(data_2017_NB_FER)

unique_values <- unique(data_2017_NB_FER$NB_VALD)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)

missing_values <- data_2017_NB_FER %>%
  summarise_all(~sum(is.na(.)))
print(missing_values)


#################################### outliers


validation_data_2023 <- remove_outliers(validation_data_2023, "NB_VALD")
data_2022_NB_FER <- remove_outliers(data_2022_NB_FER, "NB_VALD")
data_2021_NB_FER <- remove_outliers(data_2021_NB_FER, "NB_VALD")
data_2020_NB_FER <- remove_outliers(data_2020_NB_FER, "NB_VALD")
data_2019_NB_FER <- remove_outliers(data_2019_NB_FER, "NB_VALD")
data_2018_NB_FER <- remove_outliers(data_2018_NB_FER, "NB_VALD")
data_2017_NB_FER <- remove_outliers(data_2017_NB_FER, "NB_VALD")

boxplot(validation_data_2023$NB_VALD, main="Boxplot of NB_VALD", ylab="Values")


#################################



data_NB_FER <- rbind(validation_data_2023, data_2022_NB_FER,data_2021_NB_FER,data_2020_NB_FER,data_2019_NB_FER,data_2018_NB_FER,data_2017_NB_FER)
unique_values <- unique(data_NB_FER$NB_VALD)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)


missing_values <- data_NB_FER %>%
  summarise_all(~sum(is.na(.)))
print(missing_values)


write.csv(data_NB_FER, "data_NB_FER.csv", row.names = FALSE)
write.csv(data_NB_FER, "data_NB_FERT.csv", row.names = TRUE)

data_NB_FER <- read_delim("data_NB_FER.csv")

##### i want extract 10 000 lines


data_NB_FER <- data_NB_FER[sample(nrow(data_NB_FER), 10000), ]

write.csv(data_NB_FER, "data_NB_FERT_sample.csv", row.names = FALSE)



nrow(data_NB_FER)
View(data_NB_FER)

summary(data_NB_FER)

View(data_NB_FER)



nrow(data_NB_FER)

######################################
names(validation_data_2023)
names(data_2022_NB_FER)
names(data_2021_NB_FER)
names(data_2020_NB_FER)
names(data_2019_NB_FER)
names(data_2018_NB_FER)
names(data_2017_NB_FER)


summary(validation_data_2023)
summary(data_2022_NB_FER)
summary(data_2021_NB_FER)
summary(data_2020_NB_FER)
summary(data_2019_NB_FER)
summary(data_2018_NB_FER)
summary(data_2017_NB_FER)
###################################

############################ Get unique values of 'ID_REFA_LDA' column

unique_values <- unique(data_NB_FER$NB_VALD)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)

View(data_NB_FER)
summary(data_NB_FER)
names(validation_data_2023)




################## 2 : EDA

# Overall Trends
ggplot(data_NB_FER, aes(x = JOUR, y = NB_VALD)) +
  geom_line() +
  ggtitle("Overall Ridership Trends") +
  xlab("Date") +
  ylab("Number of Validations")


#######################################

data_NB_FER$JOUR <- as.Date(data_NB_FER$JOUR)


# Extract month and year
data_NB_FER$Month <- month(data_NB_FER$JOUR, label = TRUE)
data_NB_FER$Year <- year(data_NB_FER$JOUR)


unique_values <- unique(data_NB_FER$Year)
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)

missing_values <- data_NB_FER %>%
summarise_all(~sum(is.na(.)))
print(missing_values)


# Plot seasonality and monthly trends
ggplot(data_NB_FER, aes(x = Month, y = NB_VALD, group = Year, color = factor(Year))) +
  geom_line() +
  ggtitle("Seasonality and Monthly Trends") +
  xlab("Month") +
  ylab("Ridership") +
  scale_x_discrete(labels = month.name)


###################################################### Data_NB_FER

missing_values <- data_NB_FER %>%
  summarise_all(~sum(is.na(.)))
print(missing_values)



data_NB_FER$JOUR <- as.Date(data_NB_FER$JOUR, format = "%Y-%m-%d")
View(data_NB_FER)
#data_NB_FER$ID_REFA_LDA <- as.factor(data_NB_FER$ID_REFA_LDA)




##################################################

# Adding Month and Year Columns
#data_NB_FER$Month <- factor(format(data_NB_FER$JOUR, "%b"), levels = month.abb)
data_NB_FER$Year <- as.numeric(format(data_NB_FER$JOUR, "%Y"))

# Summary Statistics post-cleaning
print(summary(data_NB_FER))

# Investigate zero counts
sum(data_NB_FER$NB_VALD == 0)
any(validation_data_2023$NB_VALD == 0)

#result =0


View(data_NB_FER)

# Boxplot to identify outliers
ggplot(data_NB_FER, aes(x = '', y = NB_VALD)) + # x is left blank for a single boxplot
  geom_boxplot() +
  labs(title = "Boxplot of Daily Validations", y = "Number of Validations")




################### Density

ggplot(data_NB_FER, aes(x = log(NB_VALD + 0.001))) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Log-transformed Density Plot of Daily Validations", x = "Log(Number of Validations + 0.001)") +
  xlim(c(min(log(data_NB_FER$NB_VALD + 0.001)), max(log(data_NB_FER$NB_VALD + 0.001))))



############## Histogram of Ridership
ggplot(data_NB_FER, aes(x = NB_VALD)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Daily Ridership", x = "Daily Validations", y = "Frequency")


hist(data_NB_FER$NB_VALD, breaks = 30, col = "lightblue", main = "Histogram of NB_VALD")


############# Time Series Plot
ggplot(data_NB_FER, aes(x = JOUR, y = NB_VALD)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Ridership Over Time", x = "Date", y = "Number of Validations")


################ Boxplot by Month for a particular year
ggplot(subset(data_NB_FER, Year == 2020), aes(x = Month, y = NB_VALD)) +
  geom_boxplot() +
  labs(title = "Monthly Ridership Distribution for 2020", x = "Month", y = "Ridership")


################# Trend
ggplot(data_NB_FER, aes(x = JOUR, y = NB_VALD)) +
  geom_line() +
  theme_minimal() +
  ggtitle("Ridership Trend Over Time") +
  xlab("Date") +
  ylab("Number of Validations")

################# Seasonality 
ggplot(data_NB_FER, aes(x = Month, y = NB_VALD)) +
  geom_boxplot() +
  facet_wrap(~Year) +
  theme_minimal() +
  ggtitle("Monthly Ridership Patterns by Year") +
  xlab("Month") +
  ylab("Ridership")

################# Distribution of Ridership
ggplot(data_NB_FER, aes(x = NB_VALD)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Ridership") +
  xlab("Ridership") +
  ylab("Frequency")

################# Weekdays vs. Weekends
data_NB_FER$Weekday <- weekdays(as.Date(data_NB_FER$JOUR))

ggplot(data_NB_FER, aes(x = Weekday, y = NB_VALD)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Ridership on Weekdays vs. Weekends") +
  xlab("Day of Week") +
  ylab("Ridership")

################# Station-Level Analysis:
ggplot(data_NB_FER, aes(x = ID_REFA_LDA, y = NB_VALD)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Ridership by Station") +
  xlab("Station ID") +
  ylab("Ridership")

################# Comparative Analysis Across Years
ggplot(data_NB_FER, aes(x = JOUR, y = NB_VALD, color = as.factor(Year))) +
  geom_line() +
  theme_minimal() +
  ggtitle("Annual Comparative Ridership Trend") +
  xlab("Date") +
  ylab("Ridership")

################# Correlation Analysis exemple temp

################################### basil normal 

names(data_NB_FER)

baseline_week <- data_NB_FER %>%
  group_by(day_of_week = weekdays(JOUR)) %>%
  summarise(avg_vald = mean(NB_VALD))



# Create a bar plot with a specific color
ggplot(baseline_week, aes(x = day_of_week, y = avg_vald)) +
  geom_bar(stat = "identity", position = "dodge", fill = "skyblue", color = "black") +
  labs(title = "Baseline Average Validations by Day of the Week",
       x = "Day of the Week",
       y = "Average Validations") +
  theme_minimal()

###############################################################

calculate_easter <- function(year) {
  a <- year %% 19
  b <- year %/% 100
  c <- year %% 100
  d <- b %/% 4
  e <- b %% 4
  f <- (b + 8) %/% 25
  g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4
  k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  month <- (h + l - 7 * m + 114) %/% 31
  day <- ((h + l - 7 * m + 114) %% 31) + 1
  return(as.Date(paste(year, month, day, sep = "-")))
}


# Function to generate a list of holidays for a specific year
generate_holidays <- function(year) {
  easter_sunday <- calculate_easter(year)
  holidays <- c(
    as.Date(paste(year, "01-01", sep = "-")),        # New Year's Day
    easter_sunday + days(-1),                        # Easter Monday
    as.Date(paste(year, "05-01", sep = "-")),        # Labor Day
    as.Date(paste(year, "05-08", sep = "-")),        # Victory in Europe Day
    easter_sunday + days(39),                        # Ascension Day
    easter_sunday + days(50),                        # Whit Monday
    as.Date(paste(year, "07-14", sep = "-")),        # Bastille Day
    as.Date(paste(year, "08-15", sep = "-")),        # Assumption of Mary
    as.Date(paste(year, "11-01", sep = "-")),        # All Saints' Day
    as.Date(paste(year, "11-11", sep = "-")),        # Armistice Day
    as.Date(paste(year, "12-25", sep = "-"))         # Christmas Day
  )
  return(holidays)
}




# Generate a list of holidays from 2017 to 2023
holidays_all <- lapply(2017:2023, generate_holidays)

all_holidays <- unique(unlist(holidays_all))


all_holidays <- as.Date(all_holidays)






data_NB_FER$is_holiday <- data_NB_FER$JOUR %in% all_holidays


# Assuming holiday_data is your data frame
column_name_to_remove <- "IsHoliday"

# Remove the specified column by name
data_NB_FER <- data_NB_FER[, !colnames(data_NB_FER) %in% column_name_to_remove]

View(data_NB_FER)

# Create a dataset for holiday and non-holiday periods
holiday_data <- data_NB_FER[data_NB_FER$is_holiday, ]
non_holiday_data <- data_NB_FER[!data_NB_FER$is_holiday, ]


# Calculate average validations for holiday and non-holiday periods
avg_vald_holiday <- mean(holiday_data$NB_VALD)
avg_vald_non_holiday <- mean(non_holiday_data$NB_VALD)

# Print average validations for comparison
print(paste("Average Validations on Holidays:", avg_vald_holiday))
print(paste("Average Validations on Non-Holidays:", avg_vald_non_holiday))

# Visualize the deviations
deviations_df <- data.frame(
  period = c("Holidays", "Non-Holidays"),
  avg_vald = c(avg_vald_holiday, avg_vald_non_holiday)
)

# Create a bar plot
library(ggplot2)
ggplot(deviations_df, aes(x = period, y = avg_vald, fill = period)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Validations During Holidays and Non-Holidays",
       x = "Period",
       y = "Average Validations") +
  theme_minimal()

########################################  the impact of vacations and school breaks on ridership pattern


cor_matrix <- cor(data_NB_FER[, c("is_holiday", "NB_VALD")])


# Create a heatmap of the correlation matrix
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(20), 
        main = "Correlation Heatmap: Is Holiday vs Number of Validations",
        xlab = "Variable", ylab = "Variable")


#######################################

View(data_NB_FER)
View(stops_with_locations)


names(data_NB_FER)
names(stops_with_locations)
getwd()
########################Detecting missing value

missing_values <- stops_with_locations %>%
  summarise_all(~sum(is.na(.)))
print(missing_values)


############################

unique_values <- unique(stops_with_locations$ZdAId )
sorted_unique_values <- sort(unique_values)
print(sorted_unique_values)


stops_with_locations <- remove_outliers(stops_with_locations, "ZdAId")
stops_with_locations <- remove_outliers(stops_with_locations, "idrefa_lda")

nrow(stops_with_locations)








###########################################  statics

summary(data_NB_FER$NB_VALD)


#This summary provides insights into the distribution of the number of validations, including central tendency (mean, median) and dispersion (range, quartiles).

#The mean represents the average value of the data set, calculated by adding up all the values and dividing by the number of observations. In this case, the mean number of validations is 189.9.

#The median is the middle value of the data set when it is sorted in ascending order. In this case, the median number of validations is 76, which means that half of the observations have a number of validations below 76, and half have a number of validations above 76.


t.test(data_NB_FER$NB_VALD ~ data_NB_FER$is_holiday)

"The obtained p-value, which is well below the 0.05 threshold, indicates a high level of significance. This suggests a noteworthy disparity in validation numbers between periods classified as Holidays and those categorized as Non-Holidays. Furthermore, the confidence interval furnishes a range for the variance in means, underscoring that the mean number of validations is significantly diminished during Holiday periods in comparison to Non-Holiday periods."


hist(data_NB_FER$NB_VALD, main = "Histogram of NB_VALD")
"We lack normality, rendering it impractical to consider the results of the t-test."

wilcox.test(NB_VALD ~ is_holiday, data = data_NB_FER)



#The Wilcox on rank sum test with continuity correction was conducted to compare the distribution of the variable NB_VALD between holiday and non-holiday periods. The test resulted in a W statistic of 1743751 and a p-value of 0.01235. The alternative hypothesis suggests that there is a significant shift in the location (median) between the two groups, indicating a difference in the distribution of "NB_VALD" during holiday and non-holiday periods. The p-value being less than the significance level (typically 0.05) suggests that the observed difference is statistically significant.


anova(lm(NB_VALD ~ Weekday, data = data_NB_FER))
#The outcomes of the ANOVA analysis reveal a substantial disparity in means among various weekdays. The low p-value < 0.05 indicates a noteworthy variation in validation numbers between at least two weekdays. The substantial F value reinforces the evidence of a significant difference. This significant outcome points to variations specific to weekdays in terms of ridership.


##########################################################################



