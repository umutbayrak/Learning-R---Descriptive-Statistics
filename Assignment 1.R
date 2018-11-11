# Umut Bayrak 
# Statistical Data Analysis 
# Principles of Statistical Data Analysis - Group Assignment 1 
# Last update: 05.10.2017

# To visualize data efficiently

library(ggplot2)


# Read the data first. 

data <- read.csv("C:/Users/Umut/Data Science/MaStat17/Fall Semester/Principals of Statistical Data Analysis/HW 1/finaldata4.dat", header = TRUE, sep = " ")

levels(data$level)


# Renaming gender variables
levels(data$gender) <- c("Man", "Woman")

#str(data)
#attributes(data)
summary(data)

# Exploring the variables

# 1. Age 

boxplot(data$age)
tab <- table(data$age)
briefAge <- summary(data$age) # combine mean, median, quartiles, min, max, #NA
barplot(tab, col="royalblue", border=NA, ylim=c(0,300))
briefAge
hist(data$age)
qqnorm(data$age)
# 2. Gender 

prop.table(table(data$gender, exclude = NULL))
plot(data$gender,ylim=c(0,600),ylab="Absolute Frequency",xlab="Gender",main="Barplot of Gender")
barplot(prop.table(table(data$gender, exclude = NULL)),
        ylab = "Percentage",
        names.arg=c("Man","Woman","NA"),
        xlab="Gender",
        ylim=c(0,0.6),
        col="forestgreen")

# 3. Belgian

tab3 <- table(data$belgian)
prop.table(table(data$belgian, exclude = NULL))
barplot(prop.table(tab3),
        ylim=c(0,1),
        density=20,
        col=c("red","gold"),
        names.arg=c("Belgian","Other"))

# 4. Degree  - Find how to put percentages over barplot

tab4 <- table(data$degree)
par(mar=c(7,3,1,1), oma=c(0,0,0,0))
y <- barplot(prop.table(tab4),
             ylim=c(0,0.45),
             col=c("lightcoral"),
             border=NA,
             names.arg=c("Unknown","Satisfactory","Cum laude","Magna
                         cum laude","Summa
                         cum laude"),
             las=2)

# 5. Startdate

data$startyear <- format(as.Date(data$startdate, origin = "1899-12-31"),"%Y")

sum(is.na(data$startdate))
hist(data$startdate)
hist(as.Date(data$startdate, origin = "1899-12-31"),
     breaks="years",main="Histogram of Startdate",xlab="startdate")

boxplot(data$startdate)
boxplot(as.Date(data$startdate, origin = "1899-12-31"))

mean(data$startdate,na.rm=T)
as.Date(mean(data$startdate,na.rm=T),origin="1899-12-31")
median(data$startdate,na.rm=T)
as.Date(median(data$startdate,na.rm=T),origin="1899-12-31")
sd(data$startdate,na.rm=T)
IQR(data$startdate,na.rm=T)
min(data$startdate,na.rm=T)
max(data$startdate,na.rm=T)

qqnorm(data$startdate)
qqline(data$startdate)


# 6. Months

sum(is.na(data$months))
mean(data$months,na.rm=T)
median(data$months,na.rm=T)
sd(data$months,na.rm=T)
IQR(data$months,na.rm=T)
min(data$months,na.rm=T)
max(data$months,na.rm=T)

hist(data$months,breaks=70,freq=F)
lines(0:140,dnorm(0:140,mean(data$months,na.rm=T),
                  sd(data$months,na.rm=T))) # add normal density plot

boxplot(data$months)

qqnorm(data$months)
qqline(data$months)

# 7. phd
freqtab<-table(factor(data$phd,exclude=NULL))
barplot(freqtab)

# 8. years
sum(is.na(data$years))
hist(data$years)
barplot(table(data$years)) # discrete variable, limited number of classes
hist(data$years[data$belgian=="B"],main="histogram of years for Belgian students")
boxplot(data$years) # outliers are not 1 point but several observations with the same value!

mean(data$years,na.rm=T)
median(data$years,na.rm=T)
sd(data$years,na.rm=T)
IQR(data$years,na.rm=T)
min(data$years,na.rm=T)
max(data$years,na.rm=T)
qqline(data$years)

# 9. totdfte
sum(is.na(data$totdfte))
hist(data$totdfte)
boxplot(data$totdfte)

mean(data$totdfte,na.rm=T)
median(data$totdfte,na.rm=T)
sd(data$totdfte,na.rm=T)
IQR(data$totdfte,na.rm=T)
min(data$totdfte,na.rm=T)
max(data$totdfte,na.rm=T)


qqnorm(data$totdfte)
qqline(data$totdfte)

# 10. level
# We grouped the similar types of PhD programs together to understand the behaviour of 

levels(data$level) = c("FWO","AST", "FWO", "DCTBS", 
                       "DCTBS", "APL", "APL", 
                       "APL", "FWO", "APL", "WTSP", 
                       "WTSP")

prop.table(table(data$level, exclude = NULL))
plot(data$level,ylim=c(0,600),ylab="Absolute Frequency",xlab="Level",main="Barplot of Level")
barplot(prop.table(table(data$level, exclude = NULL)),
        ylab = "Percentage",
        xlab="Level",
        ylim=c(0,0.6),
        col="darkgoldenrod1")

table(data$level)
freqtab <- table(data$level, exclude = NULL)     
freqtab 



# 11. class

freqtab<-table(factor(data$class,exclude=NULL))
barplot(freqtab)
barplot(freqtab,names.arg=c("11","1a","1c","2","3","4a","5a","5b","6a","6b","7a","7b","NA"))

# 12. faculty
freqtab<-table(factor(data$faculty,exclude=NULL))
barplot(freqtab)

#Multivariate part
  
# "car" package is installed

#Age & Level 

plot(data$level, data$age, main = "Plot of Level & Age",
     xlab="Level ", ylab="Age ", pch=19)


# Gender & Age

plot(data$gender, data$age, main = "Plot of Gender & Age",
     xlab="Gender ", ylab="Age ", pch=19)

# Level & Gender

plot(data$level, data$gender, main = "Plot of Gender & Level",
     xlab="Gender ", ylab="Level ", pch=19)

# Level & Time & Gender tables
# Requires ggplot2 package 

frequency_tab <- table(factor(data$startyear,exclude=NA),
                       factor(data$level,exclude=NA),
                       factor(data$gender,exclude=NA))

prop_tab <- prop.table(frequency_tab,1)
prop_plot <- data.frame(prop_tab)
colnames(prop_plot) <- c( 'Startyear', 'Level', 'Gender','Fraction')

ggplot() + geom_bar(data = prop_plot, aes(y = Fraction, x = Startyear, fill = Gender), stat="identity") +
  theme_bw() + facet_grid(Level~.)




