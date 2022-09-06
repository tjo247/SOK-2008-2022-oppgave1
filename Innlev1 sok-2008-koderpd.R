# Oppgave 1.3.5-> kodingen

#setwd("/sok-2008")


library(tidyverse)  
library(readxl) 
library(ineq)
# Set your working directory to the correct folder.
# Insert your file path for 'YOURFILEPATH'.
#setwd("YOURFILEPATH")

decile_data <- read_excel("GCIPrawdatatest.xlsx", skip = 2)  
#The data is now in a 'tibble' (like a spreadsheet for R). Let's use the head function to look at the first few rows:
head(decile_data) 
#Now we use loops to complete our task. We begin by creating a new variable in our dataset, gini, which we initially set to 0 for all country-year combinations.
decile_data$gini <- 0
#Now we use a loop to run through all the rows in our dataset (country-year combinations). For each row we will repeat the Gini coefficient calculation from R walk-through 5.4 and save the resulting value in the gini variable we created.
#The function that calculates Gini coefficients from a vector of numbers is called Gini,and we apply it to the income deciles
# Give us the number of rows in decile_data
noc <- nrow(decile_data)

for (i in seq(1, noc)){
  # Go to Row I to get the decile data
  decs_i <- unlist(decile_data[i, 3:12])
  decile_data$gini[i] <- Gini(decs_i)
}
#With this code, we calculated 4,799 Gini coefficients without having to manually run the same command 4,799 times. We now look at some summary measures for the gini variable.
#First we use the subset function to select Nordic countries and save their data as temp_data. As an example, we have chosen four anglophone countries: the UK, the US, Ireland, and Australia.
temp_data <- subset(
  decile_data, Country %in% c("United States","Sweden","Finland","Norway", 
                              "Denmark"))
#Now we plot the data using ggplot.

ggplot(temp_data, 
       aes(x = Year, y = gini, color = Country)) +
  geom_line(size = 1) +
  theme_bw() +
  ylab("Gini") +
  ggtitle("Gini coefficients for Nordic countries")


#Oppgave 1.3.6->kodingen
library(janitor)
library(gglorenz)
library(cowplot)
library(PxWebApiData)
#Hvilke variabler som finnes i tabellen
data <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                     returnMetaFrames = TRUE)
variables <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                     returnMetaFrames = TRUE)
names(variables)

#hvilke verdier har ulike variablene
values <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                  returnMetaData = TRUE)
#Kommunekoder
values[[1]]$values
#Inntekt før/etter skatt
values[[2]]$values # 00 = Samlet inntekt, 00S=Inntekt etter skatt
#Desiler
values[[3]]$values
#Statistikkvariabel
values[[4]]$values
#År
values[[5]]$values
data <- ApiData("https://data.ssb.no/api/v0/en/table/12558/",
                Tid =c("2005","2020"), # Velg årene 2005 og 2020
                Desiler=c("01", "02", "03" ,"04", "05", "06" ,"07", "08" ,"09", "10"), #Vi velger alle desiler
                InntektSkatt="00", #Vi velger samlet inntekt
                ContentsCode="VerdiDesil", #Velger den høyeste verdien i desilen
                Region=c("5401","1902")) #Tromsø endret kommunenummer i 2020


#henter ut data
lldata<- data[[2]] %>% 
  drop_na(value) 
ldata<-data[[1]]%>%
  drop_na(value)
# Lager Lorenz-kurve for begge årene
lldata %>%
  ggplot(aes(value, color = Tid)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(x = "Kumulativ andel befolkning",
       y = "Kumulativ andel inntekt",
       title = "Inntektsulikhet i befolkningen i 2005 og 2020") +
  annotate_ineq(tabell$value)+
  theme_bw()

