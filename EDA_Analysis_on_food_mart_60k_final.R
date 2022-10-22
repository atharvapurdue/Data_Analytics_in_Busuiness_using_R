# importing data
CFM_data <- read.csv("media prediction and its cost.csv")


# checking the top  columns in each rows
head(CFM_data) 

# importing libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(rmarkdown)

# Check all column names 
colnames(CFM_data)


# making a transformation for getting  customer related information from dataset
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
#  scale_x_continuous(breaks = c(20,40,60,80,100,120,140,150),labels = c("20","40","60","80","100","120","140","150k+") )

# above graph shows the average income distribution of our customers

# with income as category
# TODO: need to sort the income in ascending order 
sorted_data <- customer_information[order(customer_information$avg_income),]

ggplot(sorted_data, mapping = aes(x = avg..yearly_income)) +  geom_bar()

#histograms

# Basic histogram
#distribution of education 1 == least educated 5 == most educated

ggplot(customer_information, mapping= aes(x=numeric_education),stat="count")+
  geom_histogram(color="black", fill="white",bins = 5 )
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



#New Pie Chart food category 
food_cat <- CFM_data %>% select(food_category) %>% group_by(food_category)%>% count()
food_cat<- arrange(food_cat)
piepercent<- round(100*food_cat$n/sum(food_cat$n), 1)
pie(food_cat$n,piepercent)



pie(food_cat$n, labels = food_cat$food_category, main = "Food Category pie chart",col = rainbow(length(food_cat$n)), radius = 1, cex = 0.6) 
#legend(1,0.7, food_cat$food_category, cex = 0.49,fill = rainbow(length(food_cat$n)))


#Correlation Matrix
library(corrplot)
store_data <- select(CFM_data,c('store_sales.in.millions.','store_cost.in.millions.','unit_sales.in.millions.','total_children','avg_cars_at.home.approx.','num_children_at_home','avg_cars_at.home.approx..1','SRP','gross_weight','net_weight','recyclable_package','low_fat','units_per_case','store_sqft','grocery_sqft','frozen_sqft','meat_sqft','coffee_bar','video_store','salad_bar','prepared_food','florist','cost'))
store_data
cor_data <- cor(store_data)
head(cor_data)
corrplot(cor_data,method="shade",tl.col = "black",title = "\n\n Correlation Plot Of Store Data",tl.srt=65,cl.length = 5,cl.align="l",tl.cex=0.95,  number.cex=0.8,col = colorRampPalette(c("yellow","lightblue","navyblue"))(100))
print(cor_data)

corrplot(cor_data,method="shade",tl.col = "black",tl.srt=65,cl.length = 5,cl.align="l",tl.cex=0.95,  number.cex=0.8,col = colorRampPalette(c("yellow","lightblue","navyblue"))(100))


# NEW: Count Plot Promotion Name
promotion_data <- CFM_data %>% select(promotion_name) %>% group_by(promotion_name)
promo_count<-promotion_data %>% group_by(promotion_name) 
promo_count
ggplot(promo_count, aes(x=promotion_name),color=promotion_name) + geom_bar(fill = '#003366') + theme(axis.text.x = element_text(angle = 90))


#New: avg_yearly_income
avg_income_count <- CFM_data %>% select(avg..yearly_income) %>% group_by(avg..yearly_income) %>% arrange(avg..yearly_income) 
order <- c("$10K - $30K","$30K - $50K","$50K - $70K","$70K - $90K","$90K - $110K","$110K - $130K","$130K - $150K","$150K +")
avg_income_count
ggplot(avg_income_count,aes(avg..yearly_income)) + geom_bar() + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Average Yearly Income")

#New: graph for education with gender
Ed_gender <- CFM_data %>% select(gender,education) %>% arrange(gender) %>% group_by(gender,education)
Ed_gender %>% count()
ggplot(Ed_gender, aes(x=education,fill=education)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~gender) + ggtitle(("Gender distribution in \nEducation Qualification"))


#new: Graph for occupation with gender
Ocp_gender <- CFM_data %>% select(gender,occupation)  %>% group_by(gender,occupation)
Ocp_gender %>% count()
ggplot(Ocp_gender, aes(x=occupation,fill=occupation)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90)) + facet_wrap(~gender) + ggtitle("Gender Distribution in Occupation")

#new: graph for occupation vs brandname
brand_ocp <- CFM_data %>% select(occupation,brand_name) %>% group_by(occupation)
brand_ocp %>% count()
ggplot(brand_ocp, aes(x=occupation,fill=brand_name)) + geom_bar() + ggtitle("Brand's favored by \n different Occupation")

#new: avg_yearly and member card
card_income <- CFM_data %>% select(member_card,avg..yearly_income) %>% group_by(member_card,avg..yearly_income)
card_income %>% count()
ggplot(card_income, aes(x=avg..yearly_income,fill=member_card)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90)) + ggtitle("Memebership distribution \n in different income ranges")


#new: occupation and education
ed_oc <- CFM_data %>% select(occupation,education) %>% group_by(occupation,education)
ed_oc %>% count()
ggplot(ed_oc, aes(x=occupation,fill=education)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90)) + scale_fill_hue(limits = c("Partial High School","High School Degree","Partial College","Bachelors Degree","Graduate Degree")) + ggtitle("occupation Vs Education")

#new: Avg_yearly Income with occupation
Income_ocup <- CFM_data %>% select(avg..yearly_income,occupation) %>% group_by(occupation,avg..yearly_income)
Income_ocup %>% count()
ggplot(Income_ocup,aes(x=avg..yearly_income,fill=occupation)) + geom_bar()+ theme(axis.text.x = element_text(angle = 90)) + ggtitle("Income Distribution and Occupation")

#new: Store_city and store_sales
#City_income <- CFM_data %>% select(store_city,store_sales.in.millions.) %>% group_by(store_city,store_sales.in.millions.)
#ggplot(City_income, aes(x=store_city),y=store_sales.in.millions.) + geom_bar()+ theme(axis.text.x = element_text(angle = 90))


# New: Store type and store sales
sales_type_rev <- tapply(CFM_data$store_sales.in.millions., CFM_data$store_type, FUN=sum)
sales_type_rev
p <- barplot(tapply(CFM_data$store_sales.in.millions., CFM_data$store_type, FUN=sum),ylab="in Millions",main = "Store Sales in Differnt Store Types")


#STORE_STATE AND STORE SALES
sales_state_rev <- tapply(CFM_data$store_sales.in.millions,CFM_data$store_state,FUN=sum)
sales_state_rev
barplot(sales_state_rev,main="Statewise Revenue(in Millions)",ylab="in Millions",las=2) 

#New: store_sales and city
sales_city <- CFM_data %>% select(store_city,store_sales.in.millions.)
sales_city
sales_city_rev <- tapply(CFM_data$store_sales.in.millions.,CFM_data$store_city,FUN=sum)
sales_city_rev
barplot(sales_city_rev,las=2,ylab="in Millions",xlab="city",main="Revenue state-wise")




# -----------------density + histogram combo plots ----------------

# Checking column names of customer information
colnames(customer_information)
# Checking column names of whole file
colnames(CFM_data)

# Density plot for cost with histogram and mean line
ggplot(CFM_data, aes(x=cost)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green', binwidth = 1.5) +
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(cost)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Cost') +
  theme(plot.title = element_text(hjust = 0.5))

# Store_sales
ggplot(CFM_data, aes(x=store_sales.in.millions.)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green', binwidth = 1)+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(store_sales.in.millions.)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Store Sales (in millions)') +
  theme(plot.title = element_text(hjust = 0.5))

# Store_cost
ggplot(CFM_data, aes(x=store_cost.in.millions.)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green', binwidth = 0.5)+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(store_cost.in.millions.)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Store Costs (in millions)') +
  theme(plot.title = element_text(hjust = 0.5))

# Unit_sales
ggplot(CFM_data, aes(x=unit_sales.in.millions.)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green', bins = 20)+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(unit_sales.in.millions.)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Product Sales (in millions)') +
  theme(plot.title = element_text(hjust = 0.5))

# Total_children
ggplot(CFM_data, aes(x=total_children)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='yellow')+
  geom_density(alpha=0.5, fill='red') +
  geom_vline(aes(xintercept=mean(total_children)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Total Attending Children') +
  theme(plot.title = element_text(hjust = 0.5))

# avg_cars_at_home
ggplot(CFM_data, aes(x=avg_cars_at.home.approx.)) +
  geom_histogram(aes(y=..density..), color = 'light blue', fill='blue')+
  geom_density(alpha=0.5, fill='magenta') +
  geom_vline(aes(xintercept=mean(avg_cars_at.home.approx.)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Average number of cars owned at a household') +
  theme(plot.title = element_text(hjust = 0.5))

# num_children_at_home
ggplot(CFM_data, aes(x=num_children_at_home)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='yellow')+
  geom_density(alpha=0.5, fill='orange') +
  geom_vline(aes(xintercept=mean(num_children_at_home)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Number of children in a household') +
  theme(plot.title = element_text(hjust = 0.5))

# avg_cars_at_home
ggplot(CFM_data, aes(x=avg_cars_at.home.approx..1)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(avg_cars_at.home.approx..1)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Approximated number of cars owned at a household') +
  theme(plot.title = element_text(hjust = 0.5))

# SRP
ggplot(CFM_data, aes(x=SRP)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(SRP)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Suggested Retail Price') +
  theme(plot.title = element_text(hjust = 0.5))

# gross_weight
ggplot(CFM_data, aes(x=gross_weight)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='orange')+
  geom_density(alpha=0.5, fill='light blue') +
  geom_vline(aes(xintercept=mean(gross_weight)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Gross Weight') +
  theme(plot.title = element_text(hjust = 0.5))

# net_weight
ggplot(CFM_data, aes(x=net_weight)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='orange')+
  geom_density(alpha=0.5, fill='light blue') +
  geom_vline(aes(xintercept=mean(net_weight)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Net Weight') +
  theme(plot.title = element_text(hjust = 0.5))

# recyclable_package
ggplot(CFM_data, aes(x=recyclable_package)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(recyclable_package)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of Recyclable/Non-Recyclable Package') +
  theme(plot.title = element_text(hjust = 0.5))


#low_fat
ggplot(CFM_data, aes(x=low_fat)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(low_fat)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of low_fat') +
  theme(plot.title = element_text(hjust = 0.5))

#units_per_case
ggplot(CFM_data, aes(x=units_per_case)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(units_per_case)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of units_per_case') +
  theme(plot.title = element_text(hjust = 0.5))

#store_sqft
ggplot(CFM_data, aes(x=store_sqft)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(store_sqft)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of store_sqft') +
  theme(plot.title = element_text(hjust = 0.5))

#grocery_sqft
ggplot(CFM_data, aes(x=grocery_sqft)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(grocery_sqft)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of grocery_sqft') +
  theme(plot.title = element_text(hjust = 0.5))

#frozen_sqft  -----------------------------------------------
ggplot(CFM_data, aes(x=frozen_sqft)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(frozen_sqft)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of frozen_sqft') +
  theme(plot.title = element_text(hjust = 0.5))


#meat_sqft   ------------------------------------------------
ggplot(CFM_data, aes(x=meat_sqft)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(meat_sqft)), color="blue",
             linetype="longdash") + geom_rug() +
  ggtitle('Data Distribution vs Probability Density Distribution of meat_sqft') +
  theme(plot.title = element_text(hjust = 0.5))


#coffee_bar
ggplot(CFM_data, aes(x=coffee_bar)) + 
  geom_histogram(aes(y=..density..), color = 'red', fill='green') +
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(coffee_bar)), color="blue",linetype="longdash") +
  ggtitle('Data Distribution vs Probability Density Distribution of coffee_bar') +
  theme(plot.title = element_text(hjust = 0.5))


#video_store
ggplot(CFM_data, aes(x=video_store)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(video_store)), color="blue",linetype="longdash") +
  ggtitle('Data Distribution vs Probability Density Distribution of video_store') +
  theme(plot.title = element_text(hjust = 0.5))


#salad_bar
ggplot(CFM_data, aes(x=salad_bar)) +
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(salad_bar)), color="blue",linetype="longdash")+
  ggtitle('Data Distribution vs Probability Density Distribution of salad_bar') +
  theme(plot.title = element_text(hjust = 0.5))

#prepared_food
ggplot(CFM_data, aes(x=prepared_food)) + 
  geom_histogram(aes(y=..density..), color = 'red', fill='green')+
  geom_density(alpha=0.5, fill='dark green') +
  geom_vline(aes(xintercept=mean(prepared_food)), color="blue", linetype="longdash") +
  ggtitle('Data Distribution vs Probability Density Distribution of prepared_food') +
  theme(plot.title = element_text(hjust = 0.5))

#store_sales.in.millions and store_cost.in.millions
ggplot(CFM_data, aes(x=store_sales.in.millions., y=store_cost.in.millions.))+
  ggtitle("Violin plot for store_sales.in.millions and store_cost.in.millions") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_violin(color='black' , fill = 'blue') 


dataB <- CFM_data[, c( "meat_sqft", "frozen_sqft")]

p <- ggplot(dataB, aes(meat_sqft, frozen_sqft))
p + geom_point()
# Add aesthetic mappings
p + geom_point(colour = "red", size = 3) + ggtitle("Relationship between meat area and frozen area in the mart")

dataC <- CFM_data[, c( "store_sqft", "grocery_sqft")]
q <- ggplot(dataC, aes(store_sqft, grocery_sqft))
q + geom_point(colour = "red", size = 3) + ggtitle("Relationship between store area and grocery area in the mart")

dataD <- CFM_data[, c( "gross_weight", "net_weight")]
q <- ggplot(dataD, aes(gross_weight, net_weight))
q + geom_point(colour = "red", size = 3) + ggtitle("Relationship between gross weight and net weight in the mart")

dataE <- CFM_data[, c( "gross_weight", "units_per_case")]
q <- ggplot(dataE, aes(gross_weight, units_per_case))
q + geom_point(colour = "red", size = 3) + ggtitle("Relationship between gross weight and units per case in the mart")



