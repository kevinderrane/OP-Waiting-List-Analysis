# Libraries 
library(ckanr)
library(ggplot2)
library(scales)
library(dplyr)
library(RColorBrewer)
library(treemapify)
library(openxlsx)
library(writexl)


# Q1: Accessing the datasets available on Open Data Platform

# ckanr_setup sets CKAN connection details. 
# We will be setting this to the data.gov.ie website. 
ckanr_setup(url = "https://data.gov.ie/")

# Here we are selecting the packages that are related to 
# op-waiting-list-by-group-hospital
govData <- package_show('op-waiting-list-by-group-hospital')

# Assign the year 2020 to variable year.
# Using this we can write the correct year to each csv 
year = 2020 

# Use a for loop to oop through these packages and grab the csv files from the url 
# Adding a +1 as the year starts at 2019 and we only need from 2018 back. 
for (i in 1:length(govData$resources)+1){
  # Here we are giving a name to the pulled csv file
  # we are using the above variable year to write the correct year for 
  # easier identification. 
  govDataName <- paste("csv", year-i, sep = "")
  # Here we are reading in the pulled csv file.
  govDataTemp <- read.csv(govData$resource[[i]]$url, header=TRUE, sep=",")
  # For each dataset, we are going to add in a year column. 
  # This will make it easier to aggregate our data. 
  govDataTemp$Year <- year-i
  # Here we are going to write the dataframe and name it 
  assign(govDataName, govDataTemp)
}

# Q2 Integrating the datasets into one dataset:

# Check for any missing values. 
sum(is.na(csv2014))
sum(is.na(csv2015))
sum(is.na(csv2016))
sum(is.na(csv2017))
sum(is.na(csv2018))

# For the 2017 data some of the column names are different and spelt different 
# compared to 2014-2016. To keep in line with each other we will be renaming this CSV
# to the column names of 2014-2016. 
colnames(csv2017)[names(csv2017) == "Age.Profile"] <- "Age.Categorisation"
colnames(csv2017)[names(csv2017) == "Speciality"] <- "Specialty"
colnames(csv2017)[names(csv2017) == "Total"] <- "Count"


# Our 2018 dataset does not have any column values. So we are going to add 
# column names based on the other four datasets. 
colnames(csv2018) <- c('Archive.Date', 'Group', 'Hospital.HIPE', 'Hospital', 'Specialty.HIPE', 
                       'Specialty', 'Adult.Child', 'Age.Categorisation', 'Time.Bands', 'Count', 'Year')

# Join the datasets
# Here we are going to concatenate the datasets vertically. 
# The number of columns will remain the same but the number of rows will increase. 
csvFull <- do.call("rbind", list(csv2014, csv2015, csv2016, csv2017, csv2018))


# View the joined dataset. 
head(csvFull)



# Q3 Aggregate counts:

# Below are a number of aggregations showing the changes year wise 
# across several dimensions. 

# A) Year count: This is the total number of patients on the 
# waiting lists for each year 2014-2018. 
yearCount <- csvFull %>% 
  group_by(Year) %>% 
  summarise(Count = sum(Count))


# B) Age category count: This is the total number of patients 
# for each age category for each year 2014-2018.
yearAge <- csvFull %>% 
  group_by(Year, Age.Categorisation) %>% 
  summarise(Count = sum(Count))


# C) Adult & Child count: This is the total number of patients 
# who are adults or childen on the waiting list for each year 2014-2018.
yearAdultChild <- csvFull %>% 
  group_by(Year, Adult.Child) %>% 
  summarise(Count = sum(Count))

# C1) Adult & Child count: This is the total number of patients 
# who are adults or childen on the waiting list for each Hospital 
# for each year 2014-2018.
yearAdultChildHospital <- csvFull %>% 
  group_by(Year, Hospital, Adult.Child) %>% 
  summarise(Count = sum(Count))


# D) Time Bands count: This is the total number of patients on the 
# waiting lists for each Time Band for each year 2014-2018. 
yearTimeBand <- csvFull %>% 
  group_by(Year, Time.Bands) %>% 
  summarise(Count = sum(Count))

# D1) Time Bands count: This is the total number of Time Bands 
# for each Hospital for each year 2014-2018. 
yearHospitalTimeBandsCount <- csvFull %>% 
  group_by(Year, Hospital, Time.Bands) %>% 
  summarise(Count = sum(Count)) 


# E&E1) Hospital count: This is the total number of patients on the 
# waiting lists for each Hospital for each year 2014-2018. 
yearHospitalCount <- csvFull %>% 
  group_by(Year, Hospital) %>% 
  summarise(Count = sum(Count)) 


# F) Specialty Time Bands count: This is the total number time bands
# for each specialty for each year 2014-2018. 
yearSpecialtyTimeBandsCount <- csvFull %>% 
  group_by(Year, Specialty, Time.Bands) %>% 
  summarise(Count = sum(Count)) 


# G) Specialty count: This is the number of occurences of 
# each specialty for each year 2014-2018. 
yearSpecialtyCount <- csvFull %>%
  group_by(Year) %>%
  count(Specialty, sort = T) 


# Write all the tables above to one xlsx file with multiple sheets for each table
# The writexl library is used. 
sheets <- list("Patients Count" = yearCount, "Age Categories" = yearAge,
               "Adult & Children" = yearAdultChild, "Adult & Children per Hospital" = yearAdultChildHospital,
               "Time Bands" = yearTimeBand, "Time Bands per Hospital" = yearHospitalTimeBandsCount,
               "Hospitals" = yearHospitalCount, "Specialty Time Bands" = yearSpecialtyTimeBandsCount,
               "Specialty Count" = yearSpecialtyCount) 

write_xlsx(sheets, "OP Waiting list.xlsx")


# Q4 Visualisation of the results:

# Below are some visualisations based on the above summary tables. 

# A) Year count:
pA <- ggplot(yearCount, aes(x = Year, y = Count, fill = as.factor(Year)))
pA + ggtitle("OP Waiting list totals 2014 - 2018:") +
  geom_bar(stat="identity", width=0.7) +
  scale_y_continuous(name="Total number of patients (millions):", 
                     labels = scales::comma) +
  theme(plot.title = element_text(size = 12, face = "bold")) +
  labs(fill='Year') +
  scale_fill_brewer(palette="Dark2")


# B) Age category count 2014-2018: 
pB <- ggplot(yearAge, aes(Year, Count, fill = Age.Categorisation))
pB + ggtitle("Age category totals 2014 - 2018:") +
  geom_bar(stat="identity", width=0.7) +
  scale_y_continuous(name="Total number of patients (millions):", 
                     labels = scales::comma) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom", legend.box = "horizontal") +
  labs(fill='Age Category') +
  scale_fill_brewer(palette="Dark2")


# C) Adult & Child count 2014-2018: 
pC <- ggplot(yearAdultChild, aes(Year, Count, fill = Adult.Child))
pC + ggtitle("Adult & Child totals 2014 - 2018:") +
  geom_bar(stat="identity", width=0.7) +
  scale_y_continuous(name="Total number of patients (millions):", 
                     labels = scales::comma) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom", legend.box = "horizontal") +
  labs(fill='Category:') +
  scale_fill_brewer(palette="Dark2")

# C1) Adult & Child for each Hospital count 2014-2018: 
pC1 <- ggplot(yearAdultChildHospital, aes(Hospital, Count, fill = Adult.Child))
pC1 + ggtitle("Adult & Child totals per Hospital 2014 - 2018:") +
  geom_bar(stat="identity", width=0.7) +
  facet_wrap(~Year) +
  scale_y_continuous(name="Total number of patients (thousands):", 
                     labels = scales::comma) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom", legend.box = "horizontal", 
        axis.text.x = element_text(size = 5, angle=90, vjust=0.6)) +
  labs(fill='Time Bands') +
  scale_fill_brewer(palette="Dark2")

# D) Time Bands count 2014-2018: 
pD <- ggplot(yearTimeBand, aes(Year, Count, fill = Time.Bands))
pD + ggtitle("Time Bands totals 2014 - 2018:") +
  geom_bar(stat="identity", width=0.7) +
  scale_y_continuous(name="Total number of patients (millions):", 
                     labels = scales::comma) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom", legend.box = "horizontal") +
  labs(fill='Time Bands') +
  scale_fill_brewer(palette="Dark2")


# D1) Time Bands count for each hospital 2014-2018: 
pD1 <- ggplot(yearHospitalTimeBandsCount, aes(Hospital, Count, fill = Time.Bands))
pD1 + ggtitle("Time Bands totals per Hospital 2014 - 2018:") +
  geom_bar(stat="identity", width=0.7) +
  facet_wrap(~Year) +
  scale_y_continuous(name="Total number of patients (thousands):", 
                     labels = scales::comma) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom", legend.box = "horizontal", 
        axis.text.x = element_text(size = 5, angle=90, vjust=0.6)) +
  labs(fill='Time Bands') +
  scale_fill_brewer(palette="Dark2")


# E) Hospital count for each hospital 2014-2018: 
pE <- ggplot(yearHospitalCount, aes(Year, Count, fill = Hospital))
pE + ggtitle("Waiting list total for each Hospital 2014 - 2018:") +
  geom_line(data=yearHospitalCount, size =1, show.legend = TRUE, 
               (aes(x =Year, y=Count, colour= Hospital, group = Hospital))) +
  facet_wrap(~Hospital) +
  scale_y_continuous(name="Total number of patients (thousands):", 
                     labels = scales::comma) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 5), axis.text.x = 
          element_text(size=8, angle =60, hjust=1, vjust = .5), 
        legend.position = "none") 
  

# E1) Hospital count comparing hospitals 2014-2018: 
# Below, three hospitals have been selected to plot against the rest. 
selectedHospitals<- subset(yearHospitalCount, Hospital=="Bantry General Hospital" | 
                                Hospital =="Galway University Hospital" | 
                                Hospital =="Our Lady's Children's Hospital Crumlin")

pE1 <- ggplot(yearHospitalCount, aes(Year, Count, group = Hospital))
pE1 + ggtitle("Waiting lists for Hospitals 2014-2018:") +
  geom_line( size= 0.25, na.rm = TRUE, color="grey90", alpha =0.6, 
             show.legend = FALSE ) +
  geom_line(data=selectedHospitals, size =1, show.legend = TRUE, 
            (aes(x =Year, y=Count, colour= Hospital, group = Hospital))) +
  stat_summary(fun.y=mean,geom="line",lwd=1,aes(group=1)) +
  scale_y_continuous(name="Total number of patients (millions):", 
                     labels = scales::comma) +
  # this theme clears away grid lines, makes backgound white  
  theme(plot.title = element_text(size = 12, face = "bold"),
        panel.grid.major = element_blank(), panel.background = element_blank(), 
        axis.line = element_line(colour = "black", size = 0.25), axis.title.x=element_blank(), 
        axis.text.x = element_text(angle = 30, hjust=1, vjust = .5), 
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25)) +
  scale_colour_brewer(palette = "Set1") +  
  scale_shape_manual("", values=c("mean"="-")) 



# F) Time Band count for each Specialty 2014-2018: 
pF <- ggplot(yearSpecialtyTimeBandsCount, aes(Specialty, Count, fill = Time.Bands))
pF + ggtitle("Time Bands totals per Specialty 2014 - 2018:") +
  geom_bar(stat="identity", width=0.7) +
  facet_wrap(~Year) +
  scale_y_continuous(name="Total number of patients (thousands):", 
                     labels = scales::comma) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom", legend.box = "horizontal", 
        axis.text.x = element_text(size = 5, angle=90, vjust=0.6)) +
  labs(fill='Time Bands') +
  scale_fill_brewer(palette="Dark2")



# g) Treemap for each Specialty 2014-2018: 
pG <- ggplot(yearSpecialtyCount, aes(area = n, fill = n)) +
  geom_treemap() +
  geom_treemap_text(aes(label = Specialty) , color = "black", 
                    place = "centre", grow = T,
                    min.size = 2) +
  facet_wrap( ~ Year) +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  theme(plot.title = element_text(size = 12, face = "bold"))+
  labs(
    title = "Specialty occurrences 2014-2018: ",
    fill = "Frequency (thousands):" 
  )

pG
