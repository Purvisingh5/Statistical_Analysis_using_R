data=read.csv("pizza_dataset.csv",header=TRUE)
head(data)
mean(data$Size_rating)
mean(data$Quality_rating)
sd(data$Size_rating)
sd(data$Quality_rating)
var(data$Quality_rating)
var(data$Size_rating)
median(data$Pizza_size)

summary(data$Pizza_size)
table(data$Gender)
median(data$Quality_rating)
h0='mean size ordered is equal for all the restaurants'
h1='mean size ordered is equal for all the restaurants'
anova_result <- aov(Pizza_size ~ Restaurants, data =data)
res=summary(anova_result)
res
pvalue=res[[1]][1,5]
pvalue

if (pvalue>0.05){
  print('Accept h0')
  print(h0)
}else{
  print('Can not accept h0')
  print(h1)
}
#box plot
boxplot(table(data$Menu),horizontal=TRUE)

hist(data$Pizza_size, main="Pizza Size Distribution", xlab="Pizza Size")
barplot(table(data$Factors_Influencing), xlab="factor influencing", ylab="Count")
# Bar chart for Gender
barplot(table(data$Gender), col = "skyblue", main = "Bar Chart of Gender")
# Correlation matrix
#Tcor(data[, c("Pizza_size", "Size_rating", "Quality_rating")])
#the correlation coefficient ranges from -1 to 1, indicating the strength 
#and direction of the linear relationship between two variables. 
# A value of 1 indicates a perfect positive correlation,
#-1 indicates a perfect negative correlation, and 0 indicates no linear correlation.
table(data$Gender, data$Order_method)

# Chi-square test for independence between Gender and Like_it
h0='There is association between Gender and Order Method'
h1='There is no association between Gender and Order Method'
result=chisq.test(table(data$Gender, data$Order_method))
p_value=result$p.value
p_value
if (p_value>0.05){
  print('Accept h0')
  print(h0)
}else{
  print('Can not accept h0')
  print(h1)
}
#MLR multiple linear Regression:
#Back Ward Selection
data$Restaurants=factor(data$Restaurants,levels = c("Dominos","Pizza express","Pizza hut","Pappa johns","Other"),
                      labels = c("1","2","3","4","5"))
data$Restaurants=as.numeric(data$Restaurants)
set.seed(123)

model=lm(formula =Restaurants~Size_rating+Quality_rating+Pizza_size,data=data)
summary(model)
set.seed(123)
model2=lm(formula = Restaurants~Quality_rating+Pizza_size,data=data)
summary(model2)

#Forward Selection
model3=lm(formula = Restaurants~Pizza_size,data=data)
summary(model3)

model4=lm(formula = Restaurants~Size_rating,data=data)
summary(model4)

model5=lm(formula = Restaurants~Quality_rating,data=data)
summary(model5)

# Least square method
data$Gender=factor(data$Gender,levels = c("Male","Female"),labels=c("1","2"))
data$Gender=as.numeric(data$Gender)
str(data$Menu)
data$Menu=factor(data$Menu,levels=c("Margherita","Mushroom ","Other","Pepperoni","Vegies","Chicken"),
                    labels = c("1","2","3","4","5","6"))
data$Menu=as.numeric(data$Menu)
result=lm(Gender~Menu,data=data)
result
summary(result)
res=resid(result)
res
plot(fitted(result),res)
plot(density(res))

#Gender with Restaurants
result2=lm(Gender~Restaurants,data=data)
result2
summary(result2)
res1=resid(result2)
res1
plot(fitted(result2),res1)
plot(density(res1))

#pie chart for menu offered and ordered
data=read.csv("pizza_dataset.csv",header=TRUE)
table(data$Menu)
pie(table(data$Menu),labels = c("Chicken","Margherita","Mushroom ","Other","Pepperoni","Vegies"),
    col = c('maroon','mediumorchid4','chocolate1','blue','peru','salmon'),
    main = "Pie Chart of Menu_List")
table(data$Restaurants)
pie(table(data$Restaurants),col = c('blue','mediumorchid4','chocolate1','red','salmon'),
    labels = c('Dominos','Others','Pappa Johns','Pizza express','Pizza Hutt'), 
    main = "Pie Chart of Restaurants")
barplot(table(data$Order_method),col="chocolate3",main="Bar Chart of Ordering_Method")


library(moments)
kurtosis(data$Quality_rating) 
kurtosis(data$Size_rating)
#lepto-kurtic distribution

#skewness
skewness(data$Quality_rating) #Negatively skewness
skewness(data$Size_rating)

barplot(table(data$Menu), col="chocolate1", main = "Bar Chart of Menu")
barplot(table(data$Restaurants), col="mediumorchid4", main = "Bar Chart of Restaurants")


contingency_table=table(data$Gender,data$Like_it)
contingency_table

contingency_table=table(data$Gender,data$Menu)
contingency_table

contingency_table=table(data$Brand,data$Quality_rating)
contingency_table

contingency_table=table(data$Restaurants,data$Order_method)
contingency_table
contingency_table=table(data$Gender,data$Order_method)
contingency_table

#MANOVA 

h0='there are no significant differences among group'
h1='there are significant differences among group'
data$Menu=factor(data$Menu,levels=c("Margherita","Mushroom ","Other","Pepperoni","Vegies","Chicken"),
                 labels = c("1","2","3","4","5","6"))
data$Menu=as.numeric(data$Menu)
mano_res=manova(cbind(Pizza_size, Menu)~Restaurants, data=data)
wilks=summary(mano_res,test = 'Wilks')
wilks
pval=wilks$stats['Restaurants','Pr(>F)']
pval
LOS=0.05 #bydefault level of significance

if (pval>LOS){
  print('Accept h0')
  print(h0)
}else{
  print('can not accept h0')
  print(h1)
}

fcal=wilks$stats['Restaurants','approx F']
fcal
ftab=qf(0.95,8,210)
ftab
if (fcal<ftab){
  print('Accept h0')
  print(h0)
}else{
  print('can not accept h0')
  print(h1)
}

roy=summary(mano_res,test = 'Roy')
roy
pval=roy$stats['Restaurants','Pr(>F)']
pval
LOS=0.05 #by default level of significance

if (pval>LOS){
  print('Accept h0')
  print(h0)
}else{
  print(' can accept h0')
  print(h1)
}

fcal=roy$stats['Restaurants','approx F']
fcal
ftab=qf(0.95,4,106)
ftab
if (fcal<ftab){
  print('Accept h0')
  print(h0)
}else{
  print('can not accept h0')
  print(h1)
}
# Logistic Regression for Like_it column based on Menu and Pizza_size
# Convert "YES" to 1 and "NO" to 0 in the Like_it variable
data$Like_it <- ifelse(data$Like_it == "YES", 1, 0)
str(data$Like_it)

logi_res=glm(Like_it ~ Menu + Pizza_size, data = data, family = "binomial")
logi_res
prediction=predict(logi_res,type = 'response')
head(prediction)
table(data$Like_it,prediction>0.5)
accuracy=mean((prediction>0.5)==data$Like_it)
accuracy

mode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
result=mode(data$Quality_Rating)
result
result=mode(data$Size_rating)
result

# Example using ggplot2
library(ggplot2)
ggplot(data, aes(x = Id)) +geom_bar() +labs(title = "Distribution of Id values")

