

#Section 11-1  - 6) Blood Type
observed <- as.vector(c(12, 8, 24, 6)) 

p <- c(0.2, 0.28, 0.36, 0.16) 

result =chisq.test(x=observed, p=p)

alpha = 0.1 
alpha

ifelse(result$p.value>alpha, "Fail to reject null hypothesis",
       "Reject the null hypothesis")
result

#	8) On-time Performance by Airlines

observed =c(125, 10,25, 40)

alpha = 0.05

p = c(0.708, 0.082, 0.09, 0.12)

result= chisq.test(x=observed, p=p)

ifelse(result$p.value>alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")
result

# Section 11-2  - 8) Ethnicity and Movie Admissions

alpha = 0.05

#Creating vectors for rows of matrix
r1 =c(724, 370)
r2= c(335,292)
r3 = c(174, 152)
r4 =c(107,140)

numberOfRows=4

mtrx = matrix(c(r1,r2,r3,r4), nrow = numberOfRows, byrow = TRUE)

rownames(mtrx)=c("Caucasian","Hispanic","African American", "Other")

colnames(mtrx)=c("2013","2014")
mtrx

result <- chisq.test(mtrx)

ifelse(result$p.value>alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

result

# 10) Women in Millitary

alpha = 0.05

#Create vectors of rows of matrix
r1 = c(10791, 62491)
r2 = c(7816, 42750)
r3 = c(932, 9525)
r4 = c(11819, 54344)

numberOfRows=4

mtrx =matrix(c(r1,r2,r3,r4), nrow = numberOfRows, byrow = TRUE) > mtrx

rownames(mtrx)=c("Army","Navy","Marine Corps", "Air Force")

colnames(mtrx)=c("Officers", "Enlisted")
result <- chisq.test(mtrx)
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")
result

# Section 12-1  - 8) Sodium Contents of Foods

alpha = 0.05

#Creating dataframes for each food type
condimentsDF <- data.frame("sodium"=c(270,130,230,180,80,70,200), "foodType"=rep("condiments",7), stringsAsFactors=FALSE)

cerealsDF <- data.frame("sodium"=c(260,220,290,290,200,320,140),"foodType"=rep("cereals",7), stringsAsFactors = FALSE)

dessertsDF <- data.frame("sodium"=c(100,180,250,250,300,360,300,160),"foodType"=rep("desserts",8), stringsAsFactors=FALSE)

#Combining all the above data.frames into one
sodiumDF <- rbind(condimentsDF, cerealsDF, dessertsDF)
str(sodiumDF)
sodiumDF$food <- as.factor(sodiumDF$food) # changing variable from char to factor

#Running the ANOVA test
sodiumAnova <- aov(sodium~foodType, data = sodiumDF)
summary(sodiumAnova)

a.summary = summary(sodiumAnova)

#Degrees of freedom
# k-1: between group variance - numerator 
df.numerator = a.summary
df.numerator

#n-k: within group variance -denominator

df.denominator <- a.summary
df.denominator

F.value <- a.summary[[1]][1, "F value"]
F.value

p.value <- a.summary[[1]][1, "Pr(>F)"]
p.value

ifelse(p.value>alpha, "Fail to reject null hypothesis", "Reject null hypothesis")
TukeyHSD(sodiumAnova)

# Section 12-2 - 10) Sales of Leading Companies 

alpha = 0.01

#Create data.frame for the companies
cereal = data.frame("Sales"=c(578,320,264,249,237), "Company"=rep("Cereal",5), stringsAsFactors = FALSE)
Chocolate = data.frame("Sales"=c(311,106,109,125,173),"Company"=rep("Chocolate Candy", 5), stringsAsFactors = FALSE)
Coffee = data.frame("Sales"=c(261,185,302,689),"Company"=rep("Coffee",4), stringsAsFactors = FALSE)
sales = rbind(cereal, Chocolate, Coffee)
sales$Company = as.factor(sales$Company)

anova = aov(Sales~Company, data=sales)
#summary of the result
summary(anova)
a.summary = summary(anova)
df.numerator = a.summary 
df.numerator

#n-k: within group variance: denominator 
df.denominator <- a.summary 
df.denominator

F.value <- a.summary[[1]][1, "F value"]
F.value

p.value <- a.summary[[1]][1, "Pr(>F)"]
p.value

ifelse(p.value>alpha, "Fail to reject null hypothesis", "Reject null hypothesis")
TukeyHSD(anova)

# 12) Per-Pupil Expenditures
Alpha = 0.05

eastern = data.frame("Expenditures"=c(4946, 5953, 6202, 7243, 6113), "Section"=rep("Eastern third",5), stringsAsFactors = FALSE)

middle = data.frame("Expenditures"=c(6149,7451,6000,6479), "Section"=rep("Middle third", 4), stringsAsFactors = FALSE)
western = data.frame("Expenditures"=c(5282,8605,6528,6911), "Section"=rep("Western third", 4), stringsAsFactors = FALSE)
expenditure = rbind(eastern,middle, western)
expenditure$Section = as.factor(expenditure$Section)
anova = aov(Expenditures~Section, data=expenditure)
a.summary = summary(anova)

df.numerator = a.summary 
df.numerator

#n-k: within group variance: denominator 
df.denominator <- a.summary 
df.denominator

F.value <- a.summary[[1]][1, "F value"]
F.value

p.value <- a.summary[[1]][1, "Pr(>F)"]
p.value

ifelse(p.value>alpha, "Fail to reject null hypothesis", "Reject null hypothesis")
TukeyHSD(anova)

# Section 12-3 - 10) Increase Plant Growth

library(splitstackshape)
library(tidyverse)
alpha = 0.05

data = data.frame(C1=c("A", "B"), C2=c("9.2,9.4,8.9","7.1,7.2,8.5"), C3=c("8.5,9.2,8.9","5.5,5.8,7.6"), stringsAsFactors = FALSE)
names(data)=c("Plant_food", "Light1", "Light2")
plant = cSplit(data, c("Light1","Light2"), sep = ",", direction = "long")
plant = plant%>% gather(Light, Inches,Light1:Light2 )
anova_2 = aov(Inches~Plant_food+Light + Plant_food:Light, data=plant)
a.anova2 = summary(anova_2)
a.anova2

p.value1 = a.anova2[[1]][1, "Pr(>F)"]
p.value1
p.value2 = a.anova2[[1]][2, "Pr(>F)"] 
p.value2
p.value3 = a.anova2[[1]][3, "Pr(>F)"]
p.value3

ifelse(p.value1>alpha,"There is no difference in the mean growth concerning plant food",
       "There is a significant difference in the mean growth concerning plant food" )

ifelse(p.value2>alpha, "There is no difference in the mean growth concerning light", 
       "There is a significant difference in the mean growth concerning light")

ifelse(p.value3 >alpha,"There is no difference in the mean growth concerning plant food",
       "There is a significant difference in the mean growth concerning plant food" )


# On Your Own - baseball.csv
baseballDF= read.csv("/Users/shivanivellanki/Downloads/baseball.csv",1)
baseballDF

# Changing the Char type variables to factors
baseballDF$Team <- as.factor(baseballDF$Team)
baseballDF$League <- as.factor(baseballDF$League)
# Descriptive Statistics
str(baseballDF)
summary(baseballDF)

# Scatterplot of Runs Allowed vs Wins
plot(baseballDF$RA,Baseb$W , main = "RUNS ALLOWED Vs WINS",
     xlab = "RUNS ALLOWED", ylab = "WINS",pch = 19)

# Scatterplot of Runs Scored vs Wins
plot(Baseb$RS,Baseb$W , main = "RUNS SCORED Vs WINS",
     xlab = "RUNS SCORED", ylab = "WINS", pch= 19)

alpha = 0.05

r1 <- baseballDF$Team
r2 <- baseballDF$RA
r3 <- baseballDF$RS
r4 <- baseballDF$W
rows=4

#matrix from the rows
mtrx = matrix(c(r1,r2,r3,r4), nrow = rows, byrow = TRUE)

#-naming rownames and colnames
rownames(mtrx)=c("TEAM","RA","RS", "W")

colnames(mtrx)= baseballDF$Year

result <- chisq.test(mtrx)
result

ifelse(result$p.value>alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

