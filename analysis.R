setwd('/Users/azeezatmustapha/Desktop/school/summer3/ics/ics1')
getwd()

install.packages("DescTools")
library("DescTools")
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

path <- "census.csv"
censusData <- read_csv(file=path)
censusData <- censusData[,-1]

View(censusData)

# Renaming the columns to simpler names
names(censusData) <- c('CountryName', 'Subregion', 'Region', 'Year', 'LifeExpectancyBothSexes',
                       'LifeExpectancyMale', 'LifeExpectancyFemale', 'MortalityBothSexes', 'MortalityMales', 'MortalityFemales')

# check the structure of Data
head(censusData)
str(censusData)

#Check for missing data
sum(is.na(censusData))
censusData[rowSums(is.na(censusData)) > 0, ]

# Replacing missing data in column Region and SubRegion with Africa and Western Africa 
censusData$Subregion[107:108]<-"Western Africa"
censusData$Region[107:108]<-"Africa"

# Replacing missing data in column Region and SubRegion with America and South America 
censusData$Subregion[101:102]<-"Caribbean"
censusData$Region[101:102]<-"Americas"

# Fix missing values for year 2002 by replacing with means of the subregions

### Remove NA to get mean of subregions
censusData_na <- na.omit(censusData)

#Replace missing data with group mean ##keeping for future use
subData = subData %>% 
  group_by(smoke) %>% 
  mutate_at(vars(wt), 
            ~replace_na(., 
                        mean(., na.rm = TRUE)))

### mean of Northern Africa
NAFData = censusData_na[censusData_na$Year == 2002& censusData_na$Subregion == "Northern Africa",]
NAF_means <- apply(NAFData[,5:10],2,mean)
NAF_means 


### mean of Caribbean
CarData = filter(censusData_na, censusData_na$Year == 2002 &Subregion == "Caribbean")
Car_means <- apply(CarData[,5:10],2,mean)
Car_means

### mean of Western Asia
WAData = filter(censusData_na, censusData_na$Year == 2002 &Subregion == "Western Asia")
WA_means <- apply(WAData[,5:10],2,mean)
WA_means

### mean of Northern America
NAMData = filter(censusData_na, censusData_na$Year == 2002 &Subregion == "Northern America")
NAM_means <- apply(NAMData[,5:10],2,mean)
NAM_means

### Get the rows of the missing data
which(is.na(censusData$LifeExpectancyBothSexes))

### Replace NA with their respective Subregion mean

censusData[c(235,379,385), 5:10] <- as.list(NAF_means)
censusData[c(325),5:10] <- as.list(Car_means)

censusData[c(393),5:10] <- as.list(WA_means)

censusData[c(429),5:10] <- as.list(NAM_means)


#regions and subregions
unique(censusData$CountryName)
unique(censusData$Subregion)
unique(censusData$Region)

#Filter Data for year 2022
censusData2022 <- filter(censusData, censusData$Year == 2022)
View(censusData2022)

#variables definition for clarity
lem <- censusData2022$LifeExpectancyMale
lef <- censusData2022$LifeExpectancyFemale
mrm <- censusData2022$MortalityMales
mrf <- censusData2022$MortalityFemales

#Exploring histogram of Life Expectancy variables for the year 2022
colors<-c("Africa" = "#000000",
          "Americas" = "#009268",
          "Asia" = "#1b98e0",
          "Europe" = "#f6f805",
          "Oceania" = "#d41c27")

histLebs <- ggplot(censusData2022, aes(LifeExpectancyBothSexes, fill = Region)) + 
  geom_histogram(aes(y = ..density..),  binwidth = 2) +                                         
  scale_fill_manual(values = colors )+
  labs(title = "Histogram of Life expectancy of Both sexes by region", x = "Life Expectancy - both sexes", y = "Frequency density")+
  theme(legend.position = c(0.2, 0.8))


histLEF <- ggplot(censusData2022, aes(LifeExpectancyFemale, fill = Region)) + 
  geom_histogram(aes(y = ..density..), binwidth = 2) + 
  scale_fill_manual(values = colors )+
  labs(title = "Histogram of life expectancy of Female by region", x = "Life Expectancy - Female" , y = "Frequency density")+
  theme(legend.position = c(0.2, 0.8))


histLEM <- ggplot(censusData2022, aes(LifeExpectancyMale, fill = Region)) + 
  geom_histogram(aes(y = ..density..), binwidth = 2) + 
  scale_fill_manual(values = colors )+
  labs(title = "Histogram of life expectancy of Male by region",  x = "Life Expectancy - Male" , y = "Frequency density")+
  theme(legend.position = c(0.2, 0.8))

histMbs <- ggplot(censusData2022, aes(MortalityBothSexes, fill = Region)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) + 
  scale_fill_manual(values = colors ) +
  labs(title = "Histogram of mortality of both sexes by region", x = "Child mortality rates - both sexes" , y = "Frequency density") + 
  theme(legend.position = c(0.8, 0.8))


histMF <- ggplot(censusData2022, aes(MortalityFemales, fill = Region)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) + 
  scale_fill_manual(values = colors ) +
  labs(title = "Histogram of mortality rates of Females", x = "Child mortality rates - Female" , y = "Frequency density") + 
  theme(legend.position = c(0.8, 0.8))



histMM <- ggplot(censusData2022, aes(MortalityMales, fill = Region)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) + 
  scale_fill_manual(values = colors ) +
  labs(title = "Histogram of mortality rates - Male", x = "Child mortality rates - Male" , y = "Frequency density") + 
  theme(legend.position = c(0.8, 0.8))


grid.arrange(histLebs, histLEF, histLEM, nrow = 2)
grid.arrange(histMbs, histMF, histMM, nrow = 2)

#Comparing Histogram of Life Expectancy of both sexes 
ggplot() + 
  geom_histogram(aes(x = lem, fill = "Male", y = ..density..), alpha = 0.4, binwidth = 5) +
  geom_histogram(aes(x = lef, fill = "Female", y = ..density..), alpha = 0.4, binwidth = 5) +
  scale_fill_manual(values = alpha(c("Male" = "blue", "Female" ="red", "Sexes overlap" ="maroon"), .3))+
  labs(title = "", x = "Life expectancy at birth", y = "Frequency density")+guides(fill=guide_legend(title="Legend"))+ 
  theme(legend.position = c(0.2, 0.8))


#Comparing Histogram of Mortality rate of both sexes 
ggplot() + 
  geom_histogram(aes(x = mrm, fill = "Male", y = ..density..), alpha = 0.4, binwidth = 5) +
  geom_histogram(aes(x = mrf, fill = "Female", y = ..density..), alpha = 0.4, binwidth = 5) +
  scale_fill_manual(values = alpha(c("Male" ="blue", "Female" ="red", "Sexes overlap" ="maroon"), .4))+
  labs(title = "", x = "Child mortality rates", y = "Frequency density")+
  guides(fill=guide_legend(title="Legend"))+ 
  theme(legend.position = c(0.8, 0.8))


#QUESTION 2
censusData2022Asia <- censusData2022[censusData2022$Region == "Asia",]
censusData2022Asia
#Boxplot of Life Expectancy by Subregion

colors2<-c("Eastern Asia" = "red",
           "South-Central Asia" = "#009268",
           "South-Eastern Asia" = "#1b98e0",
           "Western Asia" = "#f6f805")

boxplt_subregion1 <- ggplot(censusData2022Asia, aes(y = LifeExpectancyBothSexes , x =Subregion , fill=Subregion )) + geom_boxplot() + 
  scale_fill_manual(values = colors2)+
  theme(legend.position = c(0.8, 0.2))+
  ggtitle("Life expectancy of both sexes by Subregion for year 2022")

boxplt_subregion2 <- ggplot(censusData2022Asia, aes(x = LifeExpectancyFemale , y = Subregion, fill=Subregion )) + geom_boxplot() +
  scale_fill_manual(values = colors2)+
  theme(legend.position = c(0.8, 0.2))+
  ggtitle("Life expectancy of female by Subregion for year 2022")

boxplt_subregion3 <- ggplot(censusData2022Asia, aes(y = LifeExpectancyMale, x = Subregion , fill=Subregion)) + geom_boxplot() +
  scale_fill_manual(values = colors2)+
  theme(legend.position = c(0.8, 0.2))+
  ggtitle("Life expectancy of male by Subregion for year 2022")

boxplt_subregion4 <- ggplot(censusData2022Asia, aes(y = MortalityBothSexes, x = Subregion , fill=Subregion)) + geom_boxplot() +
  scale_fill_manual(values = colors2)+
  theme(legend.position = c(0.8, 0.8))+
  ggtitle("Child mortality rates of both sexes by Subregion for year 2022")

boxplt_subregion5 <- ggplot(censusData2022Asia, aes(y = MortalityFemales  , x = Subregion , fill=Subregion )) + geom_boxplot() + 
  scale_fill_manual(values = colors2)+
  theme(legend.position = c(0.8, 0.8))+
  ggtitle("Child mortality rates of females by Subregion for year 2022")

boxplt_subregion6 <- ggplot(censusData2022Asia, aes(y = MortalityMales , x = Subregion , fill=Subregion ))+ geom_boxplot() + 
  scale_fill_manual(values = colors2)+
  theme(legend.position = c(0.8, 0.8))+
  ggtitle("Child mortality rates of male by Subregion for year 2022")

grid.arrange(boxplt_subregion1,boxplt_subregion2,boxplt_subregion3, nrow = 2)
grid.arrange(boxplt_subregion4,boxplt_subregion5,boxplt_subregion6, nrow = 2)

#Descriptive statistics of subregions in asia
aggregate(censusData2022Asia[,5:10], list(censusData2022Asia$Subregion), FUN=mean)
aggregate(censusData2022Asia[,5:10], list(censusData2022Asia$Subregion), FUN=median)
aggregate(censusData2022Asia[,5:10], list(censusData2022Asia$Subregion), FUN=Mode)

#QUESTION 3
# Extract numeric variables life expectancy of both sex, life expectancy of male and life expectancy of female
numeric_var <- censusData2022[ , c(5:10)]

# Calculate correlation coefficient between the variables and present it in a matrix form
corrmat <- round(cor(numeric_var), 4)
corrmat

#Scatter plots

scatter_plt1 <- ggplot(censusData2022, aes(x=LifeExpectancyFemale, y=LifeExpectancyMale)) + 
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=90, y=90, label= paste("r is", corrmat[2,3]))+
  labs(title = "Scatter plot of Life expectancy of female VS Life expectancy at birth of male",
       x = "Life expectancy of for female at birth " , 
       y = "Life expectancy of for male at birth"
      )

scatter_plt2 <- ggplot(censusData2022, aes(x=LifeExpectancyFemale, y=MortalityMales )) + 
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=90, y=90, label= paste("r is", corrmat[3,5])) +
  labs(title = "Scatter plot of Life expectancy of female VS Child mortality rates of Male", 
       x = "Life expectancy of female at birth" , 
       y = "Child mortality rates of male"
    )

scatter_plt3 <- ggplot(censusData2022, aes(x=LifeExpectancyFemale, y=MortalityFemales )) + 
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=90, y=90, label= paste("r is", corrmat[3,6]))+
  labs(title = "Scatter plot of Life expectancy of female VS Child mortality rates of Female", 
       x = "Life expectancy of female at birth" , 
       y = " Child mortality rate of Female"
  )

scatter_plt4 <- ggplot(censusData2022, aes(x=LifeExpectancyMale, y=MortalityMales )) + 
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=90, y=90, label= paste("r is", corrmat[2,5]))+
  labs(title = "Scatter plot of Life expectancy of female VS Child mortality rates of Female", 
       x = "Life expectancy of male at birth" , 
       y = " Child mortality rates of male"
  )

scatter_plt5 <- ggplot(censusData2022, aes(x=LifeExpectancyMale, y=MortalityFemales )) + 
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=85, y=90, label= paste("r is", corrmat[2,6])) +
  labs(title = "Scatter plot of Life expectancy of male VS Child mortality rates of Female", 
       x = "Life expectancy of male at birth" , 
       y = "Child mortality rates of female"
  )

scatter_plt6 <- ggplot(censusData2022, aes(x=LifeExpectancyBothSexes, y=MortalityBothSexes )) + 
  geom_point()+
  geom_smooth(method=lm)+
  annotate("text", x=85, y=90, label= paste("r is", corrmat[1,4]))+
  labs(title = "Scatter plot of Life expectancy of both Sexes VS Child mortality rate of both Sexes", 
       x = "Life expectancy at birth of both Sexes" , 
       y = "Child mortality rates of both Sexes"
  )

grid.arrange(scatter_plt1,scatter_plt2,scatter_plt3, nrow = 2)
grid.arrange(scatter_plt4,scatter_plt5,scatter_plt6, nrow = 2)


#QUESTION 4
#Boxplot comparing Life Expectancy by Year

par(mfrow=c(2,2))
#dev.off() ##Change the graph to show at a time.

boxplot(LifeExpectancyBothSexes~Year,data=censusData, main="Life expectancy of both sexes by year",
        xlab="Year", ylab="Life Expectancy - Both Sexes")

boxplot(LifeExpectancyMale~Year,data=censusData, main="Life expectancy of Male by year",
        xlab="Year", ylab="Life Expectancy - Male")

boxplot(LifeExpectancyFemale~Year,data=censusData, main="Life expectancy of Female by year",
        xlab="Year", ylab="Life Expectancy - Female")



#Boxplot comparing Mortality rate by Year
par(mfrow=c(2,2))

boxplot(MortalityBothSexes~Year,data=censusData, main="Mortality rates of both sexes by year",
        xlab="Year", ylab="Child mortality rates - Both Sexes")

boxplot(MortalityMales~Year,data=censusData, main="Mortality rates of Female by year",
        xlab="Year", ylab="Child mortality rates - Male")

boxplot(MortalityFemales~Year,data=censusData, main="Mortality rates of Male by year",
        xlab="Year", ylab="Child mortality rates - Female")


