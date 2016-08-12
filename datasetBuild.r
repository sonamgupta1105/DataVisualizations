# Creating an empty dataframe
newSelfDf <- data.frame(cupsOfCoffee = integer(0), hoursSleep = integer(0), hoursInSocialMedia = integer(0), hoursOnResearch = numeric(0), minsInNovelReading = numeric(0), workout = integer(0))

# loop to create rows
for(i in 1:365)
{
	# get cupsOfCoffee
	cupsOfCoffee = round(rnorm(1, 2, 0.5))
	
	# workout yes or no
	if(runif(1) < 0.9)
	{
		workout = 1
	}
	else
	{
		workout = 0
	}
	
	
	# get hoursSleep
	if ( runif(1) < 0.85)
	{
		meanHrs = 7.5
		stdHrs = 0.75
		if (cupsOfCoffee >= 3)
		{
			meanHrs = meanHrs - runif(1, min = 1, max = 1.5)
		}
		else if (cupsOfCoffee == 2)
		{
			meanHrs = meanHrs - runif(1, min = 0.75, max = 1)
		}
		else
		{
			stdHrs = 0.25
		}
		
		if (workout == 0)
		{
			meanHrs = meanHrs - runif(1, min = 0.25, max = 1)
		}
		hoursSleep = round(rnorm(1, meanHrs, stdHrs) * 2) / 2
	}
	else
	{
		meanHrs = 4.5
		stdHrs = 0.5
		if (cupsOfCoffee >= 3)
		{
			meanHrs = meanHrs - runif(1, min = 1, max = 1.5)
		}
		else if (cupsOfCoffee == 2)
		{
			meanHrs = meanHrs - runif(1, min = 0.75, max = 1)
		}
		else
		{
			stdHrs = 0.25
		}
		hoursSleep = round(rnorm(1, meanHrs, stdHrs) * 2) / 2
	}
	
	# get hoursInSocialMedia, research, novel reading
	if (workout == 1)
	{
		minSocialMedia = 1
		maxSocialMedia = 2
		minResearch = 5
		maxResearch = 9
	}
	else
	{
		minSocialMedia = 2
		maxSocialMedia = 4
		minResearch = 3
		maxResearch = 7
	}
	
	hoursInSocialMedia = sample(minSocialMedia:maxSocialMedia, 1, replace = TRUE)
	hoursOnResearch = sample(minResearch:maxResearch, 1, replace = TRUE)
	
	if (hoursOnResearch >= 6 )
	{
		minNovel = 15 
		maxNovel = 60
	}
	else
	{
		minNovel = 45
		maxNovel = 120
	}
	minsInNovelReading =  ceiling(    sample(minNovel:maxNovel, 1, replace = TRUE)     /15)*15    
		
	rowsDataset <- c(cupsOfCoffee, hoursSleep, hoursInSocialMedia, hoursOnResearch, minsInNovelReading, workout)
	newSelfDf <- rbind(newSelfDf, rowsDataset)
}

names(newSelfDf) <- c("cupsOfCoffee", "hoursSleep", "hoursInSocialMedia", "hoursOnResearch", "minsInNovelReading", "workout")

#writing dataframe to csv file : 
write.table(newSelfDf, file = "D:/HbgUniv/Summer2016/DataVisual512/quantifiedSelf/datasetSelf.csv", sep = ",", col.names = NA)
summary(newSelfDf)
```{r}  cupsOfCoffee   hoursSleep     hoursInSocialMedia hoursOnResearch minsInNovelReading    workout      
 Min.   :1     Min.   : 4.000   Min.   :1.000      Min.   :3.000   Min.   :0.0000      Min.   :0.0000  
 1st Qu.:2     1st Qu.: 6.500   1st Qu.:1.000      1st Qu.:4.000   1st Qu.:0.0000      1st Qu.:1.0000  
 Median :2     Median : 7.000   Median :2.000      Median :6.000   Median :1.0000      Median :1.0000  
 Mean   :2     Mean   : 7.029   Mean   :1.857      Mean   :5.971   Mean   :0.9714      Mean   :0.8857  
 3rd Qu.:2     3rd Qu.: 8.000   3rd Qu.:2.500      3rd Qu.:8.000   3rd Qu.:1.5000      3rd Qu.:1.0000  
 Max.   :3     Max.   :10.000   Max.   :3.000      Max.   :8.000   Max.   :2.0000      Max.   :1.0000  
```

scatterCoffeeSleep <- ggplot(newSelfDf, aes(x = cupsOfCoffee, y = hoursSleep, group = cupsOfCoffee )) + geom_boxplot()
scatterCoffeeSleep
ggplotly(scatterCoffeeSleep)
# Box plot:
# Box plot shows median (middle line=sleep=7.5 when I have 1 cup of coffee, normal hours for sleep)
# cupsOfCoffee = 2 , variable hours of sleep with occassional outliers
# cupsOfCoffee = 3, variable hours but definitely less sleep


# HEATMAP for research vs novel
heatResearchNovel <- ggplot(newSelfDf, aes(x = minsInNovelReading, y = hoursOnResearch)) + stat_binhex() + scale_x_continuous(breaks = seq(0, 120, by = 15)) + scale_fill_gradient(name = "Frequency")
heatResearchNovel

#lot of times 9 hours of research and 1 hour of novel reading. Very infrequently I do 
#4 hours of research. Reading is mostly in between 30mins to 1 hour while mostly 
#research is 6-9 hours. Negative correlation, hoursOnResearch goes down and minsInNovelReading goes up.

#Average day pie chart
colNames = c("cupsOfCoffee", "hoursSleep", "hoursInSocialMedia", "hoursOnResearch", "minsInNovelReading", "workout", "otherTasks")
meanCoffee = mean(newSelfDf$cupsOfCoffee)
meanSleep = mean(newSelfDf$hoursSleep)
meanMedia = mean(newSelfDf$hoursInSocialMedia)
meanResearch = mean(newSelfDf$hoursOnResearch)
meanNovel = mean(newSelfDf$minsInNovelReading) / 60.0
meanWorkout = mean(newSelfDf$workout)

totalMeans = meanCoffee + meanSleep + meanMedia + meanResearch + meanNovel + meanWorkout
meanOtherTasks = 24 - totalMeans

valuesForPieChart = c(meanCoffee, meanSleep, meanMedia, meanResearch, meanNovel, meanWorkout, meanOtherTasks)
plot_ly(newSelfDf, labels = colNames, values = valuesForPieChart, type = "pie") %>%
  layout(title = "What's my day like?")

research <- ggplot(newSelfDf, aes(hoursOnResearch)) + labs(y = "Number of Days") + ggtitle("Hours Spent on Research")
research + geom_bar() + scale_x_continuous(breaks = seq(3, 9, by = 1))
#number of days in a year (count).82 days I study for 5.5 hours