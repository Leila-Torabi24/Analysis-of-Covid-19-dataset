
library(readxl)
library(tidyverse)
library(psych)
library(Hmisc)

data <- read_excel("COVID19_line_list_data.xlsx")
View(data)


## Descriptive Analysis
summary(data)
str(data)
describeData(data)

describe(data$gender)
describe(data$age)
describe(data$death)
class(data$death)
class(data$age)

## Cleaning Dataset
# change the type of gender and age and death

# gender
data <- data %>%
  mutate(gender=as.factor(data$gender))
class(data$gender)
unique(data$gender)

# age
unique(data$age)
data <- data %>%
  mutate(age=as.numeric(data$age))
class(data$age)
unique(data$age)
summary(data$age)

# death
head(data$death)
tail(data$death)
unique(data$death)
class(data$death)

data <- data %>% 
  mutate(death=if_else((death!="0" & death!="1"), "1", death))
unique(data$death)
class(data$death)


unique(data$death)
unique(as.integer(data$death))


# **************************************************************** #
## ******** Question 1: Are the death people older?   ********* ##
# **************************************************************** #

## Descriptive Analysis
# Rate of death
data %>% {prop.table(table(.$death))*100}

# Mean of age in death and alive group 
data %>% 
  group_by(death) %>% 
  summarise(mean(age, na.rm=TRUE))

# Test of equality of age between death and alive groups
t.test(age~death, data=data) # the age is not equal between two groups
# the age is a statistically significant 
# factor between alive and dead people

# Cohen effect size
df<- data %>% 
  group_by(death) %>% 
  summarise(age_avg=mean(age, na.rm=TRUE),
            age_var=var(age, na.rm=TRUE))

pooled_sd<-sqrt(sum(df$age_var)/2)

cohes_d<-diff(df$age_avg)/pooled_sd
cohes_d
# the age is a statistically significant factor affecting death and its effect size is very large 


# **************************************************************** #
## ******* Question 2: Are the death people men more?   ********* ##
# **************************************************************** #

## Descriptive Analysis
# Gender
# death

data <- read_excel("~/Desktop/Projects/R-Project/Second-R-Project/COVID19_line_list_data.xlsx")

data <- data %>% 
  mutate(death=if_else((death!="0" & death!="1"), "1", death))
unique(data$death)
class(data$death)


data <- data %>% 
  mutate(death=as.integer(death))
class(data$death)

data %>%
  group_by(gender) %>% 
  summarise(sum_death=sum(death),
            mean_death=mean(death))
unique(data$gender)

data <- data %>% 
  filter(gender %in% c("female" ,"male"))
unique(data$gender)

# Test of equality of death and lives between different genders 

t.test( death~gender , data=data) # there is a statistically significant difference between
# men and women in the rate of death.

# Cohen's effective size
df1 <- data %>% 
  summarise(avg_val=mean(death),
            var_val=var(death))

pooled_sd1<-sqrt(sum(df1$var_val)/2)
cohen_d1<-((df1$avg_val))/pooled_sd1
cohen_d1
# there is a statistically significant difference between
# men and women in the rate of death. But this difference is not important.
# It means that the we do not expect a very lorge effect size of gender on the death. 















