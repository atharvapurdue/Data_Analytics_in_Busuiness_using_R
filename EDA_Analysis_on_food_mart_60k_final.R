# importing data
CFM_data <- read.csv("/Users/Atharva/Desktop/AA_Masters_CS/Dr_Coronado/media prediction and its cost.csv")


# checking the top  columnns in each rows
head(CFM_data) 

# importing tidyverse
library(tidyverse)
library(tidyr)
# Check all column names 
colnames(CFM_data)

# making a transformation for getting  customer related information from datset
customer_information <- CFM_data[c('marital_status','gender','total_children','education','member_card','occupation','houseowner','avg..yearly_income','total_children','avg_cars_at.home.approx.')]


# print the avg. yearly income
CFM_data$avg..yearly_income

#We need to yearly income in a better format for analysis - so tidying the dataset
# Using TIDY data concepts, 



# looped if-else where the avg values are replaced by numerical values - for eg - $10k - 30K is replaced by 20 as a numeric value
# this new info is stored in column named avg_income
customer_information$avg_income = ifelse(customer_information$avg..yearly_income=='$10K - $30K',20,
                                            ifelse(customer_information$avg..yearly_income=='$30K - $50K',40,
                                                   ifelse(customer_information$avg..yearly_income=='$50K - $70K',60,
                                                       ifelse(customer_information$avg..yearly_income=='$70K - $90K',80,
                                                           ifelse(customer_information$avg..yearly_income=='$90K - $110K',100,
                                                               ifelse(customer_information$avg..yearly_income=='$110K - $130K',120,
                                                                    ifelse(customer_information$avg..yearly_income=='$130K - $150K',140,
                                                                        ifelse(customer_information$avg..yearly_income=='$150K +', 170,0))))))))



# looped if-else where the minimum_income - for eg - $10k - 30K is replaced by 10 as a numeric value
# this new info is stored in column named minimum_income
customer_information$minimum_income= ifelse(customer_information$avg..yearly_income=='$10K - $30K',10,
                                           ifelse(customer_information$avg..yearly_income=='$30K - $50K',30,
                                                  ifelse(customer_information$avg..yearly_income=='$50K - $70K',50,
                                                         ifelse(customer_information$avg..yearly_income=='$70K - $90K',70,
                                                                ifelse(customer_information$avg..yearly_income=='$90K - $110K',90,
                                                                       ifelse(customer_information$avg..yearly_income=='$110K - $130K',110,
                                                                              ifelse(customer_information$avg..yearly_income=='$130K - $150K',130,
                                                                                     ifelse(customer_information$avg..yearly_income=='$150K +', 150,0))))))))

# looped if-else where the minimum_income - for eg - $10k - 30K is replaced by 30 as a numeric value
# this new info is stored in column named maximum_income
customer_information$maximum_income= ifelse(customer_information$avg..yearly_income=='$10K - $30K',30,
                                             ifelse(customer_information$avg..yearly_income=='$30K - $50K',50,
                                                    ifelse(customer_information$avg..yearly_income=='$50K - $70K',70,
                                                           ifelse(customer_information$avg..yearly_income=='$70K - $90K',90,
                                                                  ifelse(customer_information$avg..yearly_income=='$90K - $110K',100,
                                                                         ifelse(customer_information$avg..yearly_income=='$110K - $130K',130,
                                                                                ifelse(customer_information$avg..yearly_income=='$130K - $150K',150,
                                                                                       ifelse(customer_information$avg..yearly_income=='$150K +', 200,0))))))))


# Descriptive Analysis: In order for the business to be ready for the day when all of its clients will get minimal wages, we are compiling a list of the minimum salary. 
# Perhaps events like a pandemic or a recession can have an impact on pay.
# Avg_income, minimum_income, and maximum_income are available. The business may utilize various income levels to evaluate consumer data and make predictions.
# sums that customers are going to spend.


# printing the actual information column
customer_information$avg_income
# printing the education column
customer_information$education




# changing level of education to numeric values for eg Partial High school = 1 , where as grad degree = 5
# storing these numeric values in a new column named numeric_education
customer_information$numeric_education = ifelse(customer_information$education=='Partial High School',1,
                                                ifelse(customer_information$education=='High School Degree',2,
                                                       ifelse(customer_information$education=='Partial College',3,
                                                              ifelse(customer_information$education=='Bachelors Degree',4,
                                                                     ifelse(customer_information$education=='Graduate Degree',5,0)))))


# Descriptive Analysis: The transformation described above will help us determine the customer's degree of education.
# In the future, we'll look for relationships between customer education (Col name: numeric education), average 
# income (Col name: average income), and the effects of media advertising (Col name: media type & Cost).

# printing the new numeric education column
customer_information$numeric_education


# bar plot for income level 
library(ggplot2)

# Most basic bar chart 
# TODO change the scale on x axis to better represent the data (To be done in the future.)
ggplot(customer_information, mapping = aes(x = avg_income)) + geom_bar()
# above graph shows the average income distribution of our customers

# with income as category
# TODO: need to sort the income in ascending order 
sorted_data <- customer_information[order(customer_information$avg_income),]

ggplot(sorted_data, mapping = aes(x = avg..yearly_income)) +  geom_bar()

#histograms

# Basic histogram
#distribution of education 1 == least educated 5 == most educated

ggplot(customer_information, mapping= aes(x=numeric_education),stat="count")+
  geom_histogram(color="black", fill="white")
#Plotting graph with education level of our customer 1 being the lowest and 5 being the highest
# 1 --> Partial High School
# 2 --> High School Degree
# 3 --> Partial College Degree
# 4 --> Bachelors Degree
# 5 --> Graduate Degree

#Distribution of total children in customers
ggplot(customer_information, mapping= aes(x=total_children),stat="count")+
  geom_histogram()


#TODO: Change colors
## geom_histogram(color="black", fill="white")
#p

#TODO: Improve on the representation of graphs
# using scatter plot to check education vs average income
# IT IS COMING AS EVENLY DISTRIBUTED RIGHT NOW ! ERROR!
#plot(customer_information$numeric_education,customer_information$avg_income, main="Education of customer vs Avg yearly income ", 
#    xlab="education ", ylab="Average Income", pch=19)

# plotting 
plot(CFM_data$avg_cars_at.home.approx..1,customer_information$avg_income, main="Average cars at home vs Avg yearly income ", 
     xlab="no of vehicles ", ylab="Average Income", pch=19)
#CFM_data[['education', 'member_card']]
# Average cars at home vs Average Income


         
#newdata <- subset(mydata, age >= 20 | age < 10,  select=c(ID, Weight))

#Histogram for avg yearly income with member card

#TODO Work In Progess
#ggplot(data=customer_information, aes(x=avg_income, member_card)) + geom_point() 
# ggplot(customer_information, mapping = aes(fill=member_card,x =member_card, y=avg_income )) + 
#  geom_bar(position='dodge',stat='identity')
       
# Education with gender
# changed level of education to numeric values for eg Partial High school = 1 , where as grad degree = 5 , took gender on X-axis and 
# Education level on Y-Axis and plotted the bar plot


ggplot(customer_information, mapping = aes(fill=numeric_education,x =gender, y= numeric_education)) + 
  geom_bar(position='dodge',stat='identity')
