#KDD Project 
#Knowledge Discovery in Data - Summer 2016

#Team Members
#Brad LaMotte
#Pooja Sharma
#Sara El Mawas

#External contribution from msjgriffiths on kaggle.com at 
#https://www.kaggle.com/msjgriffiths/d/kaggle/sf-salaries/explore-sf-salary-data 

#Data Preparation

#Data Cleaning 
#Data Cleaning and Preparation

rm(list = ls())
gc()

setwd("~/UNCC/UNCC_Coursework/Summer 2016/KDDProject")

#importing data in R
salaries <- read.csv(file="Salaries_Condensed.txt", stringsAsFactors = FALSE)

library(sqldf)
#Including another data having names and gender to get gender for each person.
library(sqldf)

names <- read.csv(file="names.csv", stringsAsFactors = FALSE)
salaries["Gender"] <- sqldf("select (select Gender from names where instr(lower(salaries.EmployeeName), lower(names.Name)) > 0) from salaries")

View(salaries)

#exploring missing values in the dataset 
colSums(is.na(salaries))
#There are 20000 NAs in Notes column. Also from the view at the data in the previous step we know the
#Agency column also does not provide any useful information. So let us remove these variables.

#removing Agency and Notes column because they provide no information
colnames(salaries)                          #this indicates 11 and 12 as the indexes for the two columns
salaries <- salaries[,-c(11,12)]

#replacing "Not Provided" and blank data values with <NA>.
salaries[salaries == "Not Provided"] <- NA
salaries[salaries == ""]                         <- NA

#checking which columns have NA values after above step.
colSums(is.na(salaries))

#From the table we can see 5 columns with missing values - BasePay, Overtime, OtherPay, Benefits and Status.
#Let us handle missing values in each individual variable.

#Let us look at the structure of the data
str(salaries) 

#This shows "BasePay", "OverTime", "OtherPay" and "Benefits" have been read in as character and should be numbers.

#We can also read in the data as a data table - 
library(data.table)
DT <- data.table(salaries)

#converting the columns to numeric
DT$BasePay          <- as.numeric(DT$BasePay)
DT$OvertimePay <- as.numeric(DT$OvertimePay)
DT$Benefits          <- as.numeric(DT$Benefits)
DT$Status              <- as.numeric(DT$Status)
DT$OtherPay            <- as.numeric(DT$OtherPay)

#OvertimePay and OtherPay columns just have one missing value so let us extract and examine those records.
DT[is.na(OvertimePay)]
#This shows only one record with id 148652 having OvertimePay and OtherPay as NA and Employee name as Not provided.

#removing the data row which has no useful information in any column.
DT <-DT[!(DT$EmployeeName =="Not provided"),]

length(unique(DT$JobTitle))
#This gives 1516 unique job titles. We need to reduce and bin them into 
#specific departments. For example job titles like registered nurse / special
#nurse can be grouped under Medical department. 

#convert all variables to lower case 
DT$JobTitle <- tolower(DT$JobTitle)

#create a new variable "jobtype" to reduce the job titles. Referred Kaggle to identify the job types.
DT$jobtype  <- NULL

DT$jobtype[DT$JobTitle %like% "police"]       <- "Police"
DT$jobtype[DT$JobTitle %like% "fire"]         <- "Fire"
DT$jobtype[DT$JobTitle %like% "sherif"]       <- "Police"
DT$jobtype[DT$JobTitle %like% "probation"]    <- "Police"
DT$jobtype[DT$JobTitle %like% "sergeant"]     <- "Police"
DT$jobtype[DT$JobTitle %like% "mta"]          <- "Police"
DT$jobtype[DT$JobTitle %like% "transit"]       <- "Transit"
DT$jobtype[DT$JobTitle %like% "anesth"]       <- "Medical"
DT$jobtype[DT$JobTitle %like% "medical"]      <- "Medical"
DT$jobtype[DT$JobTitle %like% "nurs"]         <- "Medical"
DT$jobtype[DT$JobTitle %like% "health"]       <- "Medical"
DT$jobtype[DT$JobTitle %like% "physician"]    <- "Medical"
DT$jobtype[DT$JobTitle %like% "orthopedic"]   <- "Medical"
DT$jobtype[DT$JobTitle %like% "pharm"]        <- "Medical"
DT$jobtype[DT$JobTitle %like% "airport"]      <- "Airport"
DT$jobtype[DT$JobTitle %like% "animal"]       <- "Animal"
DT$jobtype[DT$JobTitle %like% "architect"]    <- "Architectural"
DT$jobtype[DT$JobTitle %like% "court"]        <- "Law"
DT$jobtype[DT$JobTitle %like% "legal"]        <- "Law"
DT$jobtype[DT$JobTitle %like% "mayor"]        <- "Mayor"
DT$jobtype[DT$JobTitle %like% "librar"]       <- "Library"
DT$jobtype[DT$JobTitle %like% "parking"]      <- "Parking"
DT$jobtype[DT$JobTitle %like% "public works"] <- "Public Works"
DT$jobtype[DT$JobTitle %like% "attorney"]     <- "Attorney"
DT$jobtype[DT$JobTitle %like% "mechanic"]     <- "Automotive"
DT$jobtype[DT$JobTitle %like% "automotive"]   <- "Automotive"
DT$jobtype[DT$JobTitle %like% "custodian"]    <- "Custodian"
DT$jobtype[DT$JobTitle %like% "engineer"]     <- "Engineering"
DT$jobtype[DT$JobTitle %like% "engr"]         <- "Engineering"
DT$jobtype[DT$JobTitle %like% "account"]      <- "Accounting"
DT$jobtype[DT$JobTitle %like% "gardener"]     <- "Gardening"
DT$jobtype[DT$JobTitle %like% "general laborer"] <- "General Laboror"
DT$jobtype[DT$JobTitle %like% "food serv"]    <- "Food Service"
DT$jobtype[DT$JobTitle %like% "clerk"]        <- "Clerk"
DT$jobtype[DT$JobTitle %like% "porter"]       <- "Porter" 

#Now checking number of rows which did not get a job type and are still NA.
table(is.na(DT$jobtype))

#Out of 20000 records we have binned 12161 records however 7838 still have no bins.

#let us have a look at the job titles and job types which were not binned.
unbinned.jobtitles <- DT[is.na(DT$jobtype) == TRUE]
unbinned.jobtitles <- subset(unbinned.jobtitles, select = c(3,12))  
head(unbinned.jobtitles,10)

#replacing the remaining unbinned jobtitles into a constant "Others" job type.
DT$jobtype[is.na(DT$jobtype) == TRUE ]  <- "Other"

#Now after binning the job titles into fewer job types we can replace the missing 
#base pays with the median base pay salaries.

#Step1: Find job types(newly created variables based on job titles) which have missing BasePay values.
#Step2: Calculate the median salary for each of the job types identified in Step1 and store the medians in a dataframe along with their job types.
#Step3: Replace the NAs in BasePay with the medians calculated in Step2 based on the job type in the two data frames.
library(dplyr)
#records without NA in BasePay column 
basepay.NONNAs <- subset(DT[ which(is.na(DT$BasePay) == FALSE),])
basepay.NONNAs <- data.table(basepay.NONNAs)

#records with NA in BasePay column
basepay.NAs <- (DT[ which(is.na(DT$BasePay) == TRUE),])
basepay.NAs    <- data.table(basepay.NAs$jobtype)
colnames(basepay.NAs)[1] <- "jobtype"
colnames(basepay.NAs)[1]

#Extract only those records for which median needs to be calculated because there may be #job types for which there are no missing base pays so we can remove those records and #avoid unnecessary processing.
data.formeans <- semi_join(basepay.NONNAs,basepay.NAs)

#Calculating median for the selected job types
df2 <- aggregate(data.formeans$BasePay,list(data.formeans$jobtype),median)

#renaming columns 
names(df2)[1]<-paste("jobdesc")
names(df2)[2]<-paste("med")

#replacing the missing values with median base pays of each job type.
DT$BasePay <- ifelse(is.na(DT$BasePay), df2$med[match(DT$jobtype, df2$jobdesc)], DT$BasePay)

colSums(is.na(DT))

#Replace NA benefits with TotalPayBenefits - BasePay - OvertimePay - OtherPay as #TotalPayBenefits seems to be the sum of all Pays.
DT2 <- DT[is.na(DT$Benefits)]
DT$Benefits[is.na(DT$Benefits)] <- DT2$TotalPayBenefits - (DT2$BasePay + DT2$OvertimePay + DT2$OtherPay)

#This is giving some negative values also.
#Probably not all records have other pays included in the TotalPay and benefits field.
DT$Benefits[DT$Benefits < 1 ] <- 0
summary(DT$Benefits)

#Replacing missing values in Status column 
DT$Status[is.na(DT$Status) & DT$BasePay > 25000 & DT$Benefits > 0] <- "FT"
DT$Status[is.na(DT$Status) & DT$BasePay <= 25000 & DT$Benefits < 1] <- "PT"

#there are records with 0 benefits and BasePay greater than 25000 which are probably Part Time because most full time jobs have benefits.
DT$Status[is.na(DT$Status) & DT$BasePay > 25000] <- "PT"
DT$Status[is.na(DT$Status) & DT$BasePay < 25000 & DT$Benefits > 0] <- "FT"

colSums(is.na(DT))


#Categorizing the income into different brackets 
DT$PayRange[DT$TotalPay<50000] <- "Very Low"
DT$PayRange[DT$TotalPay>50000 & DT$TotalPay<100000] <- "Low"
DT$PayRange[DT$TotalPay>100000 & DT$TotalPay<150000] <- "Entry Level"
DT$PayRange[DT$TotalPay>150000 & DT$TotalPay<200000] <- "Middle Income"
DT$PayRange[DT$TotalPay>200000 & DT$TotalPay<250000] <- " Upper Middle Income"
DT$PayRange[DT$TotalPay>250000 & DT$TotalPay<300000] <- " High Income"
DT$PayRange[DT$TotalPay>300000 & DT$TotalPay<350000] <- " Super High Income"


#Saving the cleaned files.
write.csv(DT,file = "cleaned_salaries.csv", row.names = FALSE)


#Data Exploration 

rm(list = ls())
gc()

salaries <- read.csv(file="cleaned_salaries.csv", stringsAsFactors = FALSE)

# range of Total Pay
range(salaries$TotalPay)

# summary of Total Pay
summary(salaries$TotalPay)

# Breakdown by gender
unknowns <- salaries
unknowns[is.na(unknowns$Gender) == TRUE,]$Gender <- "Unknown"
unknowns <- setNames(aggregate(unknowns$TotalPay ~ unknowns$Gender, FUN=length), c("Gender", "Count"))
unknowns["Percentage"] <- unknowns$Count / nrow(salaries) * 100
unknowns

# Difference of genders per year
unknowns <- salaries
unknowns[is.na(unknowns$Gender) == TRUE,]$Gender <- "Unknown"
gender_counts <- table(unknowns$Year, unknowns$Gender)
gender_counts2 <- as.data.frame.matrix(gender_counts)
gender_counts2["Female Diff"] <- gender_counts2$F - gender_counts2$M
gender_counts2

# How many different job types
length(unique(salaries$jobtype))

# What years are represented in the dataset
range(salaries$Year)

# How many records per year
table(salaries$Year)

# Summaries of females and gender subsets
females <- subset(with_gender, with_gender$Gender == "F")
summary(females$TotalPay)
males <- subset(with_gender, with_gender$Gender == "M")
summary(males$TotalPay)

## Begin: Women's percentage of Pay
# average total pays per year
female_pay_by_year <- setNames(aggregate(females$TotalPay ~ females$Year, FUN=mean), c("Year", "TotalPay"))
male_pay_by_year <- setNames(aggregate(males$TotalPay ~ males$Year, FUN=mean), c("Year", "TotalPay"))

data.frame(female_pay_by_year, male_pay_by_year)

# female difference in average total pay per year
female_diff <- female_pay_by_year$TotalPay - male_pay_by_year$TotalPay

# female total pay proportion to male per year
female_proportion <- female_pay_by_year$TotalPay / male_pay_by_year$TotalPay * 100

# table showing for every year female total pay diff and proportion
pay_diff <- setNames(data.frame(years, female_pay_by_year$TotalPay, male_pay_by_year$TotalPay, female_diff, female_proportion), c("Year", "Female", "Male", "Diff", "Proportion"))
barplot(pay_diff$Proportion, 
        names.arg=pay_diff$Year, 
        ylim=c(80,95), 
        xpd=FALSE, 
        col="#749AF2",
        main="Women's Percentage of Pay",
        ylab="Percentage",
        xlab="Year")
## End: Women's percentage of Pay

## Begin: table showing female proportion per deptartment
female_pay_by_dept <- setNames(aggregate(females$TotalPay ~ females$jobtype, FUN=mean), c("Dept", "TotalPay"))
male_pay_by_dept <- setNames(aggregate(males$TotalPay ~ males$jobtype, FUN=mean), c("Dept", "TotalPay"))
female_dept_proportion <- setNames(data.frame(female_pay_by_dept$Dept, female_pay_by_dept$TotalPay / male_pay_by_dept$TotalPay * 100), c("Dept", "Proportion"))
x <- barplot(female_dept_proportion$Proportion, 
             names.arg=female_dept_proportion$Dept, 
             xaxt="n", 
             col="#749AF2",
             main="Proportional Pay by Job Type",
             xlab="Job Type",
             ylab="Percentage",
             ylim=c(60,150),
             xpd=FALSE)
abline(h=100, col="red")
## End: table showing female proportion per deptartment

#Total Pay over the years 
library(ggplot2)

library(sqldf)
salaries %>%
  ggplot(aes(x = TotalPay)) +
  geom_density(aes(fill = jobtype), alpha = 0.6)

library(scales)
salaries$Year <- as.factor(salaries$Year)
p <- ggplot(salaries, aes(x=TotalPay, group=Year, fill=Year, color=Year)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  scale_x_continuous(labels = comma) +
  xlab("Total Salary ($)") +
  ylab("Density of Employees") + 
  theme(axis.text.y=element_blank())
p

ggsave("salaryDistribution.png", p, height=4, width=7, units="in")


library("ggplot2")
library("scales")
y2011 <- subset(salaries,jobtype == "Fire" | jobtype == "Medical")
str(y2011)

ggplot(y2011, aes(x=Year, y=BasePay)) +
  geom_boxplot(fill="#53cfff") +
  theme_light(base_size=16) +
  scale_y_continuous(labels = comma) +
  ylab("Base Salary ($)")

#Get the top ten jobtypes by frequency 

#Get the bottom ten job types by frequency 

p <- ggplot(y2011, aes(x=TotalPay, group=jobtype, fill=TotalPay, color=TotalPay)) +
  geom_density(alpha=0.3) +
  theme_light(base_size=16) +
  scale_x_continuous(labels = comma) +
  xlab("Total Salary ($)") +
  ylab("Density of Employees") + 
  theme(axis.text.y=element_blank())
p

#
salaries.new <- salaries[(salaries$jobtype != "Other"),]
aggregate.output <- aggregate(TotalPay ~ Year * jobtype, data = salaries.new, FUN = median)
aggregate.output
head(aggregate.output)

#get most frequent occuring jobs
factor(salaries.new$jobtype)
library(plyr)
a <- count(salaries.new,'jobtype')
class(a)
head(a)
colnames(a)
sort.salaries <- a[order(-a$freq),]  #sort freq in desc order
head(sort.salaries)

top.5 <- head(sort.salaries,5)

#Top 5 most frequent occuring jobs in the data are 
#Medical Police Transit Clerk and Fire 
desired.result <- aggregate.output[aggregate.output$jobtype %in% top.5$jobtype,]

boxplot(desired.result$TotalPay~desired.result$jobtype)
boxplot(desired.result$TotalPay~desired.result$jobtype+desired.result$Year)
#Fire department is having the highest payscale 

plot(desired.result$TotalPay ~ desired.result$Year,col="dark red")


colors = c("green", "orange", "blue", "yellow", "pink")

a1<- ggplot(data=desired.result, aes(x=Year, y=TotalPay, fill=jobtype)) +
  geom_bar(stat="identity", position=position_dodge(),
           colour="black") +
  scale_fill_manual(values=colors)
a1
ggsave("Pooja1.png", a1, height=4, width=7, units="in")

#Question 2: Compare the Total Pay of job type bins to the national average pay
#We are comparing the ational average pay with the most recent year data 2014
medians.2014 <- subset(aggregate.output,aggregate.output$Year == 2014)

#Only interested in the four jobdesc defined below - Fire/Police/Transit and Clerk
desired.result2 <- medians.2014[medians.2014$jobtype %in% top.5$jobtype,]
desired.result2 <- desired.result2[desired.result2$jobtype != 'Medical',-1]
desired.result2
desired.result2$paytype <- "Given"

jobtype     <- c('Fire','Police','Transit','Clerk')
TotalPay <- c(44043,52810,40160,30000)
paytype <- rep("National",4)
natdata     <- data.frame(jobtype,TotalPay,paytype,stringsAsFactors = FALSE)
natdata

mergedpays <- rbind(natdata,desired.result2)
mergedpays


#Creating a line plot to see how the data compares
ggplot(data=mergedpays, aes(x=factor(mergedpays$jobtype), y=mergedpays$TotalPay, group=mergedpays$paytype, colour=mergedpays$paytype)) +
  geom_line() +
  geom_point()

#Q3.Check the percentage benefit to total pay for each job type

#Removing TotalPay = 0 before calculating percentages.
salaries.new2 <- subset(salaries.new,salaries.new$TotalPayBenefits > 0 )

salaries.new2$percent.benefits <- (salaries.new2$Benefits/salaries.new2$TotalPayBenefits) * 100 

summary(salaries.new2$percent.benefits)

#Exploring the benefits for the top 5 jobtypes
salaries.new2 <- salaries.new2[salaries.new2$jobtype %in% top.5$jobtype,]

boxplot(salaries.new2$percent.benefits~salaries.new2$jobtype)

ggplot(salaries.new2, aes(x=salaries.new2$jobtype, y=salaries.new2$percent.benefits, fill=salaries.new2$jobtype)) + geom_boxplot() +
  guides(fill=FALSE)

#Q3. Compare the total pay and benefits for different jobtypes

salaries2 <- salaries[salaries$jobtype %in% top.5$jobtype,]
a1 <- aggregate(TotalPay ~ jobtype, data = salaries2,FUN = sum)

b1 <- aggregate(Benefits ~ jobtype, data = salaries2,FUN = sum)

str(a1)
str(b1)

b1

# jobtype| CTC | CTC.Type(Compensation or Benefits)
#rename benefits and total columns to "CTC" to merge them into one dataset 

colnames(a1)[2] <- "CTC"
colnames(b1)[2] <- "CTC"
str(a1)
str(b1)

a1$CTC.Type <- "TotalPay"
b1$CTC.Type <- "Benefits"
a1
b1


combineddata <- rbind(a1,b1)
combineddata
combineddata$customlabels <- c('20M','21M','47M','45M','33M','63M','110M','210M','220M','120M')
combineddata


ggplot(combineddata,aes(x=jobtype,y=CTC)) +
  geom_bar(aes(fill=CTC.Type),stat = "identity") +
  geom_text(aes(label = customlabels),size = 4)

#Check the income ranges over years based on the number of records for each range
#Start
library(ggplot2)
colors = c("orange", "coral", "lightblue", "pink", "green","yellow", "cyan")

a1<- ggplot(data=salaries, aes(Year, fill=PayRange)) +
  geom_bar( position=position_dodge(),
           colour="black") +
  scale_fill_manual(values=colors) +
ggtitle(" Income Ranges over the Years")
a1
#End


## Barchart for the top three types: Clerk, Medical, Transit
#Start
##create subsets for the top three types
Police <- subset(salaries, salaries$jobtype == "Police")
Medical <- subset(salaries, salaries$jobtype == "Medical")
Clerk <- subset(salaries, salaries$jobtype == "Clerk")
## add the tables intor one table to create the chart 
total <- rbind(Clerk,Medical,Police )
## Bar Chart
colors = c("coral", "pink", "lightblue")

a2<- ggplot(data=total, aes(Year, fill=jobtype)) +
  geom_bar( position=position_dodge(),
            colour="black") +
  scale_fill_manual(values=colors) +
  ggtitle(" Top 3 Type Jobs in 4 years")
a2
#End


# Pie Chart with Percentages
#Start
table<-table(salaries$PayRange) [1-7] / nrow(salaries) * 100
slices <- c(table) 
lbls <- c("High Income", "Upper Middle Income","Entry Level", "Low", "Middle Income", "Very Low", "Super High Income")
pct <-  round ((table(salaries$PayRange) [1-7] / nrow(salaries) * 100), digits = 2)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
colors = c("red", "yellow", "green", "violet", 
          "orange", "blue", "pink", "cyan") 
pie(slices,labels = lbls, col=rainbow (length(lbls)),
    main="Pie Chart of Income Ranges")

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)
library(mixtools)

#Exploring how does pay compare to the people who share title ?

data %>%
  mutate(SharesTitle = duplicated(JobTitle)) %>%
  ggplot(aes(x = TotalPay)) + 
  geom_density(aes(fill = SharesTitle), alpha = 0.6)


##exploring overtime pay of full time employees

ft <- data %>% filter(Status == "FT")
ft %>%
  ggplot(aes(x = OvertimePay)) +
  geom_density(fill = "grey40") 


#Working with job levels 

ft %>%
  mutate(level = str_extract(JobTitle, "(?<=\\s).$")) ->
  ft

# Subset down
ft %>%
  filter(!is.na(level)) ->
  jobs_with_levels

# Peek
(head(jobs_with_levels))

jobs_with_levels %>%
  group_by(level) %>%
  summarise(People = n(), Titles = n_distinct(JobTitle))

ft %>%
  mutate(Level = ifelse(is.na(level), "No Level", level)) %>% 
  ggplot(aes(x = TotalPay)) +
  geom_density(aes(fill = Level), alpha = 0.6) +
  facet_wrap(~Level, scales="free_y")


#Extracing and exploring the salary of Supervisors and assistants in SF 
ft %>%
  mutate(Assistant = str_extract(JobTitle, "Asst|Assistant")) %>%
  group_by(Assistant) %>%
  summarise(mean(TotalPayBenefits))

#It looks like people who are called Asst tend to make more money. It turns out - great surprise! - that when the City shortens the word, it's because it's a long title. Often when Manager or Supervisor is after it.

ft %>%
  mutate(Assistant = str_extract(JobTitle, "Asst|Assistant")) %>%
  filter(Assistant == "Asst") %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n(), pay = mean(TotalPayBenefits), sd = sd(TotalPayBenefits)) %>%
  arrange(desc(Frequency))

#End


#Modelling 

#Hypothesis Testing
females <- subset(salaries, salaries$Gender == "F")
males <- subset(salaries, salaries$Gender == "M")
t.test(females$TotalPay, males$TotalPay)

#Linear Regression 

#read in the data
rm(list = ls())
gc()

salaries <- read.csv(file="cleaned_salaries.csv", stringsAsFactors = FALSE)

colnames(salaries)

subsetdata <- subset(salaries,select=c("BasePay","Benefits"))


#Split the dataset into "training"  (80%) and validation (20%).
ind <- sample(2,nrow(subsetdata),replace = TRUE,prob = c(0.8,0.2))

train  <- subsetdata[ind == 1,]
test   <- subsetdata[ind == 2,]

TestBasePay <- test$Benefits
colnames(test)
#Removing the target variable from the test dataset
test   <- test[,-c(2)]
colnames(test)
results <- lm(Benefits ~ BasePay,train)
summary(results)

#This is giving just 43% adjusted R squared value 
#Obviously there are other factors like department, part time/full time 
#status which are governing the target variable.

#Lets try only for Fire department employees
subsetdata <- subset(salaries,select=c("BasePay","Benefits","OvertimePay","OtherPay"))

ind <- sample(2,nrow(subsetdata),replace = TRUE,prob = c(0.8,0.2))

train  <- subsetdata[ind == 1,]
test   <- subsetdata[ind == 2,]

results <- lm(Benefits ~ OvertimePay + OtherPay + BasePay,train)
summary(results)
#predicting benefits in the test dataset
pred <- predict(results,test)

head(pred)
head(test)

#This is giving just 43% adjusted R squared value 
#Obviously there are other factors like department, part time/full time 
#status which are governing the target variable.

#Lets try only for Fire department employees
subsetdata <- subset(salaries,jobtype == "Fire" & Benefits > 0,select=c("BasePay","Benefits"))

head(subsetdata)

#Split the dataset into "training"  (80%) and validation (20%).
ind <- sample(2,nrow(subsetdata),replace = TRUE,prob = c(0.8,0.2))

train  <- subsetdata[ind == 1,]
test   <- subsetdata[ind == 2,]

TestBenefits <- test$Benefits
colnames(test)
#Removing the target variable from the test dataset
test   <- as.data.frame(test[,1])
colnames(test)
#rename the colname in test dataset 
names(test)[1] <- "BasePay"

head(test)
results <- lm(Benefits ~ BasePay,train)
summary(results)

#This is giving just 43% adjusted R squared value 
#Obviously there are other factors like department, part time/full time 
#status which are governing the target variable.

#Lets try only for Fire department employees


#predicting benefits in the test dataset
test$pred <- predict(results,test)

test$diffinpred <- TestBenefits - test$pred
test$diffinpred <- TestBenefits - test$pred

min(abs(test$diffinpred))

#The Cart Model 
#Model2 - CART and C5.0 

#Read in the data and install and laod requried packages 
salaries <- read.csv(file="cleaned_salaries.csv", stringsAsFactors = FALSE)

#install.packages(c("rpart", "rpart.plot", "C50"))
library("rpart"); library("rpart.plot"); library("C50")
library("stringr")

#Target Variable:     Pay Range
#Predictor Variables: Gender/Department/Status/BasePay/level(maybe)

#Since we were not able to extract gender information for 13% of the data 
#Let us exclude those rows from our model.
salaries %>%
  mutate(level = str_extract(JobTitle, "(?<=\\s).$")) ->
  salaries


subsetdata <- subset(salaries,Gender == "M" | Gender == "F", select=c("BasePay","Gender","jobtype","Status","Benefits","level","PayRange"))

head(subsetdata)

#Lets remove records with "Others" as jobtype or records having
#unknown levels because they do not provide useful information 
#for our model 
subsetdata <- subsetdata[is.na(subsetdata$level) == FALSE,]
head(subsetdata)
data <- subsetdata[subsetdata$jobtype != "Other",]
head(data)

#Standardize the basepay numeric variable for CART model 
data$Benefits <- (data$Benefits - mean(data$Benefits))/sd(data$Benefits)


head(data)


# Use predictors to classify the total pay range of a person 
cartfit <- rpart(PayRange ~ Gender + jobtype + Benefits + Status + level,
                 data = data,
                 method = "class")
print(cartfit)

#Plot the decision tree 
rpart.plot(cartfit)

#The Apriori Algorithm (1) Supervised Learning 
#Trying to determine which combinations go along with Overtime culture 
#
#Apriori on Salaries Dataset
salaries <- read.csv(file="cleaned_salaries.csv", stringsAsFactors = FALSE)

colnames(salaries)

#Determine which department and income bracket tends to have more
#overtime culture in SF city.

#Generate a cateogorical variable for overtime pay
#If overtime pay = 0 then Overtime = No else Overtime=Yes

#So - Overtime-Categorical | Income Bracket | JobType | Job status 

#Generate rules with 80% confidence

#Overtimeind | jobtype | PayRange | Gender | 

#Preparing dataset for OvertimePay 

salaries$OvertimePay.ind <- ifelse(salaries$OvertimePay ==0,"No", "Yes")  

salaries.subset <- salaries[,c(11,12,13,14,15)]
head(salaries.subset)

#Remove data where gender information is not available 
salaries.subset <- salaries.subset[is.na(salaries.subset$Gender) == FALSE,]
salaries.subset <- salaries.subset[salaries.subset$jobtype != "Other",]


#Generate rules which show what inicators drive Overtime Work culture in the 
#city of San Francisco.

#change the columns to factors 
str(salaries.subset)

#convert all characters into factor
salaries.subset[sapply(salaries.subset, is.character)] <- lapply(salaries.subset[sapply(salaries.subset, is.character)], 
                                                                 as.factor)

rules <- apriori(salaries.subset,
                 control = list(verbose=F),
                 parameter = list(minlen=2,supp=0.1, conf=0.75),
                 appearance = list(rhs=c("OvertimePay.ind=No",
                                         "OvertimePay.ind=Yes"),
                                   default="lhs"))
inspect(rules)

#The Apriori Algorithm (2) Unsupervised Learning 
library(arules)
library(arulesViz)
library(datasets)

colnames(salaries)
# create a subset that doesnt have Gender =NA and jobtype=other
salaries <- read.csv(file="cleaned_salaries.csv", stringsAsFactors = TRUE)
subset.salaries<-salaries[,c(11,12,13)]
head(subset.salaries)
subset.salaries<-subset.salaries[is.na(subset.salaries$Gender)== FALSE,]
subset.salaries<-subset.salaries[subset.salaries$jobtype != "Other",]


## generate the rules with support =0.001 and confidence =0.80 for the three variables. It is an unsupervised association rule 
## so we didn't specify the target therefore the rhs will not be constant the same for all the asscoaiation rules 
rules <- apriori(subset.salaries, parameter = list(supp = 0.001, conf = 0.80))
inspect(rules)
summary(rules)


