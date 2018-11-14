### NOEL GABRIEL HORNILLA
### Coumpting HW1

#1. WHO dataset

#b. country with the biggest population
WHO = read.csv("WHO.csv")
WHO$Country[which.max(WHO$Population)]

#c. population of Malaysia
WHO$Population[WHO$Country=="Malaysia"]

#d. country with the lowest literacy
WHO$Country[which.min(WHO$LiteracyRate)]

#e. Richest country in Europe based on GNI
WHO.Europe = subset(WHO, Region == "Europe")
WHO.Europe$Country[which.max(WHO.Europe$GNI)]

#f. Mean Life expectancy of countries in Africa
WHO.Africa = subset(WHO, Region == "Africa")
mean(WHO.Africa$LifeExpectancy)

#g. Number of countries with population greater than 10,000
WHO.PopMoreThan10K = subset(WHO, Population>10000)
nrow(WHO.PopMoreThan10K)

#h. Top 5 countries in the Americas with the highest child mortality
WHO.Americas <- subset(WHO, Region == "Americas")
WHO.Americas = WHO.Americas[order(-WHO.Americas$ChildMortality),]WHO.Americas$Country[1:5]




#2. NBA dataset (Historical NBA Performance.xlsx)

#a. The year Bulls has the highest winning percentage 
library(readxl)
NBA = read_excel("Historical NBA Performance.xlsx")
NBA.Bulls = subset(NBA, Team == "Bulls")
NBA.Bulls$Year[which.max(NBA.Bulls$`Winning Percentage`)]

#b. Teams with an even win-loss record in a year
NBA.EvenWin = subset(NBA, NBA$`Winning Percentage`==0.5)
NBA.EvenWin

#3. Seasons_Stats.csv

#a. Player with the highest 3-pt attempt rate in a season.
Seasons_Stats = read.csv("Seasons_Stats.csv")
Season_Stats.AggX3PA_AggFGA = aggregate(cbind(X3PA, FGA)~Year+Player, Seasons_Stats,sum) #Aggregates 3PA and FGA per year per player
Season_Stats.AggX3PA_AggFGA$Computed_3PAr = Season_Stats.AggX3PA_AggFGA$X3PA / Season_Stats.AggX3PA_AggFGA$FGA #Adds column for computed 3PAr using 3PA and FGA. It's better to compute for the value rather than the simple average of the 3PAr column so it's a weighted average
subset(Season_Stats.AggX3PA_AggFGA, Computed_3PAr == max(Season_Stats.AggX3PA_AggFGA$Computed_3PAr, na.rm = TRUE)) #Returns player(s) with highest 3PAr


#b. Player with the highest free throw rate in a season.
Seasons_Stats.AggFTr = aggregate(cbind(FTA,FGA)~Year+Player,Seasons_Stats,sum)
Seasons_Stats.AggFTr$ComputedFTr = Seasons_Stats.AggFTr$FTA / Seasons_Stats.AggFTr$FGA
is.na(Seasons_Stats.AggFTr) <- sapply(Seasons_Stats.AggFTr, is.infinite) #removes inifinite values
subset(Seasons_Stats.AggFTr, ComputedFTr == max(Seasons_Stats.AggFTr$ComputedFTr, na.rm = TRUE))

#c. What year/season does Lebron James sLcored the highest?
LBJ = subset(Seasons_Stats, Player=="LeBron James")
LBJ$Year[which.max(LBJ$PTS)]

#d. What year/season does Michael Jordan scored the highest?
MJ = subset(Seasons_Stats, Player =="Michael Jordan*")
MJ$Year[which.max(MJ$PTS)]

#e. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
Kobe = subset(Seasons_Stats, Player == "Kobe Bryant")
Kobe$PER[which.min(Kobe$MP)]

#4. National Universities Rankings.csv

#a. University with the most number of undergrads 
NUR = read.csv("National Universities Rankings.csv")
NUR$Undergrad.Enrollment = as.numeric(gsub("\\,","",NUR$Undergrad.Enrollment))
NUR$Name[which.max(NUR$Undergrad.Enrollment)]

#b. Average Tuition in the Top 10 University
NUR = NUR[order(NUR$Rank),]
NUR.Top10 = head(NUR,10)
NUR.Top10$Tuition.and.fees = as.numeric(gsub("\\$|\\,", "", NUR.Top10$Tuition.and.fees))
mean(NUR.Top10$Tuition.and.fees)

