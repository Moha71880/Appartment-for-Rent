install.packages("gridExtra")
install.packages("moments")
install.packages("dplyr")
install.packages("lubridate")
install.packages("rpart")
install.packages("randomForest")
install.packages("xgboost")
install.packages("caret")

library(readxl)
library(dplyr)
library(psych)
library(ggplot2)
library(gridExtra)
library(moments)
library(dplyr)
library(lubridate)
library(rpart)
library(randomForest)
library(xgboost)
library(caret)
library(leaflet)
library(corrplot)

#Firstly we have to clean our dataset: 

data = apartments_for_rent_classified_10K_csv

colnames(data)

selected_col=c("bathrooms","bedrooms","pets_allowed","price","square_feet","cityname","state","latitude","longitude")

data=data[selected_col]

data= data %>% mutate_all(~ ifelse(. == "null", NA, .))

data=data %>% filter(!is.na(latitude))

#Transformation data

str(data$bathrooms)
data$bathrooms=as.numeric(data$bathrooms)
data$price <- as.numeric(data$price)
data$bedrooms <- as.numeric(data$bedrooms)
data$square_feet <- as.numeric(data$square_feet)

mediane_bathrooms=median(data$bathrooms,na.rm = TRUE)

data$pets_allowed=replace(data$pets_allowed,data$pets_allowed=="None",values = 0 )
data$pets_allowed=replace(data$pets_allowed,data$pets_allowed=="Cats",values = 1 )
data$pets_allowed=replace(data$pets_allowed,data$pets_allowed=="Dogs",values = 1 )
data$pets_allowed=replace(data$pets_allowed,data$pets_allowed=="Cats,Dogs",values = 2 )
data$pets_allowed=as.numeric(data$pets_allowed)

median_data=data %>% summarise(across(everything(), median, na.rm = TRUE))


data$bathrooms=replace(data$bathrooms,list = is.na(x = data$bathrooms),1)
data$pets_allowed=replace(data$pets_allowed,is.na(data$pets_allowed),values = 3 )
data$bedrooms=replace(data$bedrooms,is.na(data$bedrooms),values = 2)

#Summary Statistics :

head(data)
summary(data)

variable_types <- sapply(data, class)
print(variable_types)

#After Cleaning, we can see the form of the distrubtion of the target variable :

# Create histogram and density plot
hist_density <- ggplot(data, aes(x=price)) +
  geom_histogram(aes(y=..density..), bins=30, fill="blue", alpha=0.5) +
  geom_density(colour="red", fill="red", alpha=0.2) +
  ggtitle("Histogram and Density Plot for price ") +
  xlab("price") +
  ylab("Density")

print(hist_density)


#We can cleary see we have an extrem value, We can't see the distrubition clearly, so we'll remove it to see the difference:

data <- subset(data, !price >= 11000)
data <- subset(data, !square_feet >= 5500)

# Create a new histogram without extrem value

hist_density <- ggplot(data, aes(x=price)) +
  geom_histogram(aes(y=..density..), bins=30, fill="blue", alpha=0.5) +
  geom_density(colour="red", fill="red", alpha=0.2) +
  ggtitle("Histogram and Density Plot for price ") +
  xlab("price") +
  ylab("Density")

print(hist_density)

par(mfrow = c(2, 2))  

#Histogram of the four quantitatives variables

hist(data$price, main = 'Histogram price', col = 'lightblue', border = 'black')
hist(data$bedrooms, main = 'Histogram bedrooms', col = 'lightgreen', border = 'black')
hist(data$bathrooms, main = 'Histogram bathrooms', col = 'lightcoral', border = 'black')
hist(data$square_feet, main = 'Histogram square_feet', col = 'lightyellow', border = 'black')

#Histogram of distribution of quantitative variables for different price quartiles

data$price_cat <- cut(data$price, quantile(data$price, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                      labels = c('Q1', 'Q2', 'Q3', 'Q4'), include.lowest = TRUE)

# Histogram of 'square_feet' conditioning by 'price'
hist_list <- lapply(levels(data$price_cat), function(cat) {
  subset_data <- subset(data, price_cat == cat)
  hist(subset_data$square_feet, main = paste('Histogram of square_feet for the', cat), 
       xlab = 'square_feet', ylab = 'Fréquence', col = 'lightblue', border = 'black')
})


par(mfrow = c(2, 2))  
# Histogram of 'bedrooms' conditioning by 'price'

hist_list <- lapply(levels(data$price_cat), function(cat) {
  subset_data <- subset(data, price_cat == cat)
  hist(subset_data$bedrooms, main = paste('Histogram of bedrooms for', cat), 
       xlab = 'bedrooms', ylab = 'Fréquence', col = 'lightblue', border = 'black')
})


par(mfrow = c(2, 2))


# Histogram of 'bathrooms' conditioning by 'price'
hist_list <- lapply(levels(data$price_cat), function(cat) {
  subset_data <- subset(data, price_cat == cat)
  hist(subset_data$bathrooms, main = paste('Histogram of bathrooms for the', cat), 
       xlab = 'bathrooms', ylab = 'Fréquence', col = 'lightblue', border = 'black')
})

par(mfrow = c(2, 2))

par(mfrow = c(1, 1))

# Calculate various statistics for 'price'
mean_price <- mean(data$price)
median_price <- median(data$price)
std_dev_price <- sd(data$price)
skewness_price <- skewness(data$price)
kurtosis_price <- kurtosis(data$price)

print(paste("Mean of 'price':", mean_price, "\n"))
print(paste("Median of 'price':", median_price, "\n"))
print(paste("Standard Deviation of 'price':", std_dev_price, "\n"))
print(paste("Skewness of 'price':", skewness_price, "\n"))
print(paste("Kurtosis of 'price':", kurtosis_price, "\n"))


#Use geographics variables:

#Transform latitude and longitude as numeric data
str(data)
data$longitude <- as.numeric(data$longitude)
data$latitude <- as.numeric(data$latitude)

#First map who represent apartment of the data set in USA with scale of price for red point

carte <- leaflet(data = data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    radius = sqrt(data$price) / 10, 
    color = "red",
    fillOpacity = 0.5,
    popup = ~paste("cityname : ", cityname, "<br> Price : $", price)
  )

carte

#The same carte with no scale of price for the red point, because it was superpose 

carte <- leaflet(data = data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    radius = sqrt(data$price) /sqrt(data$price) ,  # Utilise la racine carrée pour ajuster la taille des cercles
    color = "blue",
    fillOpacity = 0.5,
    popup = ~paste("cityname : ", cityname, "<br> Price : $", price)
  )

carte

#Now I want to do a map for each quartile of price

quartiles <- quantile(data$price, probs = c(0, 0.25, 0.5, 0.75, 1))


# Attribuate a color for each quartile
assigner_couleur <- function(price) {
  if (price <= quartiles[2]) {
    return("green")
  } else if (price <= quartiles[3]) {
    return("yellow")
  } else if (price <= quartiles[4]) {
    return("orange")
  } else {
    return("red")
  }
}

# Add a color column attribuate to each quartiles

data$couleur <- sapply(data$price, assigner_couleur)

cartes <- list()
for (i in 1:4) {
  new_data <- data[data$price >= quartiles[i] & data$price <= quartiles[i + 1], ]
  carte <- leaflet(data = new_data) %>%
    addTiles() %>%
    addCircleMarkers(
      ~longitude, ~latitude,
      radius = sqrt(data$price) /sqrt(data$price),
      color = ~couleur,
      fillOpacity = 0.7,
      popup = ~paste("cityname : ", cityname, "<br> Price : $", price)
    )
  cartes[[i]] <- carte
}

# Print the map of differents price quartiles
cartes[[1]]
cartes[[2]]
cartes[[3]]
cartes[[4]]


#An boxplot who distribuate price by State


library(ggplot2)
ggplot(data, aes(x = longitude, y = latitude, color = price)) +
  geom_point(size = 2) +
  scale_color_gradient(low = "red", high = "blue", name = "Prix") +
  labs(title = "Diagramme de dispersion des prix par emplacement", x = "Longitude", y = "Latitude")




#Boxplot Prices according to number of bedrooms/bathrooms and State

boxplot(price ~ state, data = data, col = "lightblue", main = "Distribution des prix par état", xlab = "state", ylab = "price")

boxplot(data$price ~ data$bedrooms, main = "Boxplot : Prices according to number of bedrooms",
        xlab = "Number of bedrooms", ylab = "Price")

boxplot(data$price ~ data$bathrooms, main = "Boxplot : Prices according to number of bathrooms",
        xlab = "Number of bathrooms", ylab = "Price")



#Linear regression simple

ylim <- c(0, 10000)
xlim <- c(0, 10000)

#Linear regression  between number of square feet and price

plot(data$square_feet, data$price, main = "Relationship between number of square feet and price", 
     xlab = "Square feet", ylab = "Price", 
     xlim = xlim, ylim = ylim, col = 'blue', pch = 16, cex= 0.5)

modele <- lm(price ~ square_feet, data = data)
abline(modele, col = 'red')

#Linear regression  between number of bedrooms and price


plot(data$bedrooms, data$price, main = "Relationship between number of bedrooms and price", 
     xlab = "Number of bedrooms", ylab = "Price",
     ylim = ylim, col = 'blue', pch = 16, cex= 0.5)

modele <- lm(price ~ bedrooms, data = data)
abline(modele, col = 'red')

#Linear regression  between number of bathrooms and price


plot(data$bathrooms, data$price, main = "Relationship between number of bathrooms and price", 
     xlab = "Number of bathrooms", ylab = "Price",
      ylim = ylim, col = 'blue', pch = 16, cex= 0.5)

modele <- lm(price ~ bathrooms, data = data)
abline(modele, col = 'red')


#Matrice correlation for numeric data:

matrice_correlation <- cor(data[, c("price", "bedrooms", "bathrooms", "square_feet")])
print (matrice_correlation)
corrplot(matrice_correlation, method = "circle", tl.col = "black")



#MODELISATION





# Modélisation avec la régression linéaire
modele <- lm(price ~ bathrooms + bedrooms + pets_allowed + square_feet + latitude + longitude + cityname + state, data = data)
print(modele)
# Résumé du modèle
summary(modele)

