library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
drivers <- read_csv("drivers.csv")
results <- read_csv("results.csv")
races <- read_csv("races.csv")

status <- read_csv("status.csv")

results_agg_total <- inner_join(drivers,results,by = "driverId")
results_agg_total_alt <- merge(drivers,results,by = "driverId")
results_agg_total <- inner_join(races,results_agg_total,by = "raceId")



results_agg_total <- results_agg_total %>% unite(name, forename, surname, sep=" ")

results_winners <- results_agg_total %>% filter(position==1)

results_podium_finishers <- results_agg_total %>%
  
  filter(position==1|position==2|positionText==3)

mostwins <- as.data.frame(sort(table(results_winners$name), decreasing=TRUE)[1:10])

View(mostwins)

ggplot(mostwins, aes(x = reorder(Var1, Freq),y=Freq)) +
  geom_point() + ggtitle('Most Title Wins') + xlab('Driver Name') + ylab('TotalTitles')+coord_flip()

most_podium <- as.data.frame(sort(table(results_podium_finishers$name), decreasing=TRUE))
races_total <- as.data.frame(sort(table(results_agg_total$name), decreasing=TRUE))
freq_winners <- inner_join(most_podium, races_total, by="Var1")
freq_winners$pct <- freq_winners$Freq.x/freq_winners$Freq.y   
freq_winners <- arrange(freq_winners, desc(pct))
freq_winners2 <- filter(freq_winners, Freq.y >= 20)
freq_winners2 <- slice(freq_winners2, 1:10)



ggplot(freq_winners2, aes(x = reorder(Var1, pct),y=pct)) +
  
  geom_point() + ggtitle('Most Frequent Podium Placement (20 Or More Races)') + xlab('Driver Name') + ylab('Total Podium Percentage')+ coord_flip()







#Plots

library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(ggrepel)
library(viridis)
library(circlize)


results<-read.csv('results.csv',sep=',',stringsAsFactors=F)
#convert character to numeric
results$fastestLapSpeed<-as.numeric(results$fastestLapSpeed)
#convert FastestLap(character) tonumeric(secondes)
convertFastestLap<-function(x){
  if(length(x)>0){
    curMinute<-as.numeric(strsplit(x,":")[[1]][1])
    curSecond<-as.numeric(strsplit(strsplit(x,":")[[1]][2],"\\.")[[1]][1])
    return(curMinute*60 + curSecond)
  }
  else if(length(x)==0){
    return(NA)
  }
}

races<-read.csv('races.csv',stringsAsFactors=F,sep=',')
#convert character to Date
races$date<-as.Date(races$date,"%Y-%m-%d")
#remove "Grand Prix" in the name
races$name<-gsub(" Grand Prix","",races$name)

results_2<-left_join(
  results %>% dplyr::select(-time, -fastestLapTime), 
  races %>% dplyr::select(-time, -url), 
  by='raceId')

results_3<-left_join(
  results %>% dplyr::select(-time), 
  races %>% dplyr::select(-time, -url), 
  by='raceId')



circuits<-read.csv("circuits.csv",sep=",",stringsAsFactors=F)
races<-left_join(races %>% select(-name,-url), circuits %>% select(-url), by='circuitId')


results_2 %>% 
  dplyr::filter(year>2004) %>% 
  dplyr::group_by(name,year) %>% 
  summarize(medianFastestLapSpeed = median(fastestLapSpeed,na.rm=T)) %>% 
  ggplot(aes(x=factor(year),y= medianFastestLapSpeed,color=medianFastestLapSpeed)) + 
  geom_point() + theme_fivethirtyeight() + 
  scale_color_gradientn(name="",colours=rev(viridis::viridis(20))) +
  theme(
    axis.text.x = element_text(size=6,angle=45),
    strip.text.x = element_text(size = 10)) + facet_wrap(~name,ncol=9) + 
  labs(title='Fastest Lap per Circuit, from 2005 to 2023',
       subtitle='speed in km/h') +
  guides(color=FALSE)



results_2 %>% 
  dplyr::filter(year>2004) %>% 
  dplyr::group_by(name,year) %>% 
  summarize(medianFastestLapSpeed = median(fastestLapSpeed,na.rm=T)) %>% 
  ggplot(aes(x=factor(year),y= medianFastestLapSpeed,color=medianFastestLapSpeed)) + 
  geom_boxplot(alpha=.25) + theme_fivethirtyeight() + 
  geom_jitter(shape=16,position=position_jitter(0.2),size=1.5) + 
  geom_smooth(method='loess',aes(group=1),color='red',lty=2,size=.5) +
  scale_color_gradientn(name="",colours=rev(viridis::viridis(20))) + 
  labs(title='Fastest Lap per Year',
       subtitle='in km/h, grouped by Grand Prix') + 
  guides(color = FALSE)


results_2 %>% 
  dplyr::filter(year>2004) %>% 
  dplyr::group_by(name) %>% 
  ggplot(aes(x=fastestLapSpeed)) + 
  geom_histogram(bins=100) + theme_fivethirtyeight() + 
  scale_color_gradientn(name="",colours=rev(viridis::viridis(20))) +
  theme(
    axis.text.x = element_text(size=6,angle=45),
    strip.text.x = element_text(size = 10)) + facet_wrap(~name,ncol=9) + 
  labs(title='Fastest Lap distribution per Circuit',
       subtitle='speed in km/h, grouped by years') +
  guides(color=FALSE)




#More plots



library(rvest)    
library(tidyr)    
library(dplyr)    
library(ggplot2)  



browseURL('https://www.formel1.de/saison/wm-stand/2023/fahrer-wertung')

f1 <- read_html('https://www.formel1.de/saison/wm-stand/2023/fahrer-wertung') %>% 
  html_node('table') %>% 
  html_table()

f1 <- f1[complete.cases(f1), ]


colnames(f1) <- c('Pos', 'Driver', 'Total', sprintf('R%02d', 1:21))

f1 <- as_tibble(f1) %>% 
  filter(as.integer(Pos) <= 9)

f1$Driver <- as.factor(f1$Driver)
f1[, -2] <- apply(f1[, -2], 2, function(x) as.integer(gsub('-', '0', as.character(x))))
f1long <- gather(f1, Race, Points, R01:R21)

View(f1long)


ggplot(f1long, aes(x = Race, y = Points, group = Driver, colour = Driver)) + 
  geom_line() +
  scale_x_discrete(breaks=c('R01', 'R06', 'R11', 'R16', 'R21')) +
  labs(title = 'F1 race results 2023')



ggplot(f1long, aes(x = Race, y = Points, group = Driver, colour = Driver)) + 
  geom_line(show.legend = FALSE) + facet_wrap(~ Driver) +
  scale_x_discrete(breaks=c('R01', 'R06', 'R11', 'R16', 'R21')) +
  labs(title = 'F1 race results 2023',
       caption = 'source: www.formel1.de')

















#Regression part

circuits <- read.csv('circuits.csv')
laptimes <- read.csv('lap_times.csv')
pitstops <- read.csv('pit_stops.csv')
seasons <- read.csv('seasons.csv')
status <- read.csv('status.csv')
constructor_standings <- read.csv('constructor_standings.csv')
constructors <- read.csv('constructors.csv')
driver_standings <- read.csv('driver_standings.csv')
drivers <- read.csv('drivers.csv')
races <- read.csv('races.csv')
constructor_results <- read.csv('constructor_results.csv')
results <- read.csv('results.csv')
qualifying <- read.csv('qualifying.csv')


# Merge 'results' and 'races' dataframes on 'raceId' column
df <- merge(results, races[c('raceId', 'year', 'name', 'round', 'date')], by = 'raceId', all.x = TRUE)

# Merge 'df' and 'drivers' dataframes on 'driverId' column
df <- merge(df, drivers[c('driverId', 'driverRef', 'forename', 'surname', 'nationality', 'dob')], by = 'driverId', all.x = TRUE)

# Merge 'df' and 'constructors' dataframes on 'constructorId' column
df <- merge(df, constructors[c('constructorId', 'name', 'nationality')], by = 'constructorId', all.x = TRUE)

# Merge 'df' and 'status' dataframes on 'statusId' column
df <- merge(df, status[c('statusId', 'status')], by = 'statusId', all.x = TRUE)




df$driver_name <- paste(df$forename, df$surname, sep = ' ')

# Drop 'forename' and 'surname' columns from the 'df' dataframe
df <- df[, !(names(df) %in% c('forename', 'surname'))]

df$dob <- as.Date(df$dob)



dates <- as.Date(Sys.Date()) - df$dob
age <- as.numeric(dates) / 365

df$age <- round(age)

df <- as.data.frame(lapply(df, function(x) gsub("\\\\N", "NA", x)))



df_2022 <- subset(df, year == 2022)



# Define a custom function to convert lap time
convert_lap_time <- function(time_str) {
  if (is.character(time_str)) {
    time_parts <- as.numeric(strsplit(time_str, ":")[[1]])
    minutes <- time_parts[1]
    seconds <- time_parts[2]
    return(minutes + seconds / 60)
  } else {
    return(time_str)
  }
}

# Apply the custom function to convert lap time in 'fastestLapTime' column
df_2022$fastestLapTime <- sapply(df_2022$fastestLapTime, convert_lap_time)

# Convert 'fastestLapTime' column to numeric
df_2022$fastestLapTime <- as.numeric(df_2022$fastestLapTime)


# Select specific columns for 'X' dataframe
X <- df_2022[, c("raceId", "grid", "points", "laps", "fastestLap", "fastestLapTime")]


y <- df_2022$positionOrder
# Encode the 'y' variable
#y <- factor(y)



set.seed(17)

# Calculate the number of rows for the test set
test_size <- round(nrow(X) * 0.3)

# Generate random indices for the test set
test_indices <- sample(1:nrow(X), test_size)

# Split the data into training and testing sets
X_train <- X[-test_indices, ]
X_test <- X[test_indices, ]
y_train <- y[-test_indices]
y_test <- y[test_indices]




#data$y <- as.numeric(as.character(data$y))
data1 <- cbind.data.frame(y, X)

# Build a linear regression model
model1 <- lm(y ~ ., data = data1)

summary(model1)

