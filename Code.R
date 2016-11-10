# Data Collection

vgsales <- read.csv("C:/Username /Video game sales/vgsales.csv")
View(vgsales)

#Packages and Libraries

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(xlsx) 

# DATA WRANGLING

vgs <- mutate(vgs, NA_net_sales= (NA_Sales*100)/ Global_Sales)
vgs <- mutate(vgs, EU_net_sales= (EU_Sales*100)/ Global_Sales)
vgs <- mutate(vgs, JP_net_sales= (JP_Sales*100)/ Global_Sales)
vgs <- mutate(vgs, other_net_sales= (Other_Sales*100)/ Global_Sales)

#EXPLORATORY DATA ANALYSIS

# Overall Summary Statistics 
  summary_video_game_sales<-summary(vgs)

#Summary Stats based on a variable

  # Mean Sales based on Platform
  summary_GSales_Platform<- summarise(group_by(vgs, Platform), mean(Global_Sales))

  # Mean Sales based on Publisher of the Game
  summary_GSales_Publisher<- summarise(group_by(vgs, Publisher), mean(Global_Sales))

  # Mean Sales based on Genre of Game
  summary_GSales_Genre<- summarise(group_by(vgs, Genre), mean(Global_Sales))

  # Mean Sales based on the Year
  summary_GSales_Year<- summarise(group_by(vgs, Year), mean(Global_Sales))

# DATA VISUALIZATION

  #Total Revenue on each Year, Name, Genre
  Revenue_By_YNG<- vgs %>%  group_by(Year, Name, Genre) %>% summarise(Revenue= sum(Global_Sales))
  
  ggplot(Revenue_By_Year, aes(Year, Revenue) + geom_bar(fill=”Blue”, stat=”identity”) + ggtitle(“Revenue By Year”) + scale_x_discrete(breaks(1980,2016,3))
  
# Revenue of Games between 1980 and 1995
  
  Revenue_1980_1995<- subset(Revenue_by_YNG, Year %in% c("1980": "1995"))
  p1<- ggplot(Revenue_1980_1995, aes(Year,Revenue)) + geom_bar(fill= "Blue", stat= "identity") + ggtitle("Revenue between 1980 and 1995") + ylim(0,800)
  

# Revenue of Games between 1995 and 2000
  
  Revenue_1995_2000<- subset(Revenue_by_YNG, Year %in% c("1995": "2000"))
  p2<- ggplot(Revenue_1995_2000, aes(Year,Revenue)) + geom_bar(fill= "Blue", stat= "identity") + ggtitle("Revenue between 1995 and 2000") + ylim(0,800)  

# Revenue of Games between 2000 and 2010

  Revenue_2000_2010<- subset(Revenue_by_YNG, Year %in% c("2000": "2010"))
  p3<- ggplot(Revenue_2000_2010, aes(Year,Revenue)) + geom_bar(fill= "Blue", stat= "identity") + ggtitle("Revenue between 2000 and 2010") + ylim(0,800) 

# Revenue of Games between 2010 and 2016

  Revenue_2010_2016<- subset(Revenue_by_YNG, Year %in% c("2010": "2016"))
  p4<- ggplot(Revenue_2010_2016, aes(Year,Revenue)) + geom_bar(fill= "Blue", stat= "identity") + ggtitle("Revenue between 2010 and 2016") + ylim(0,800) 

# Grid: Revenue betweeen 1980 and 2016

  grid.arrange(p1,p2,p3,p4,top= "Video Game", left= "REVENUE GROWTH", bottom= "YEARS")
  
# Revenue by Year based on Genre: Bar Graph

  ggplot(Revenue_By_YNG, aes(Year, Revenue, na.omit(TRUE))) + geom_bar(stat = "identity", aes(color= Genre)) + ggtitle("Revenue by Year") + scale_x_discrete(breaks=seq(1980,2016,4)) + theme_classic()

# Revenue by Year based on Genre: Density

  ggplot(Revenue_By_Year, aes(Year, Revenue, na.omit("TRUE"))) + geom_density(stat = "identity", aes(color= Genre)) + ggtitle("Revenue by Year") + scale_x_discrete(breaks=seq(1980,2016,4)) + theme_classic()

# Revenue by Year based on Genre: Jitter

  ggplot(Revenue_By_Year, aes(Year, Revenue, na.omit("TRUE"))) + geom_jitter(stat = "identity", aes(color= Genre)) + ggtitle("Revenue by Year") + scale_x_discrete(breaks=seq(1980,2016,4)) + theme_classic()
