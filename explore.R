library(stringr)
library(ggplot2)
library(RecordLinkage)

mcd = read.csv("menu.csv")
mcd$Item = as.character(mcd$Item)

mcd$Serving.Size = as.character(mcd$Serving.Size)
mcd$Serving.Size = gsub(".*\\((.*)\\).*", "\\1", mcd$Serving.Size)
mcd$Serving.Size[str_detect(mcd$Serving.Size, "oz")] = paste(as.integer(29.5735*as.numeric(gsub("([0-9]+).*$", "\\1", mcd$Serving.Size[str_detect(mcd$Serving.Size, "oz")]))), "ml")
mcd$food = as.factor(grepl("g", mcd$Serving.Size))
mcd$drink = as.factor(grepl("ml", mcd$Serving.Size))
mcd$Serving.Size[mcd$food == TRUE] = as.numeric(gsub(" g", "", mcd$Serving.Size[mcd$food == TRUE]))
mcd$Serving.Size[mcd$drink == TRUE] = as.numeric(gsub(" ml", "", mcd$Serving.Size[mcd$drink == TRUE]))
mcd$Serving.Size = as.numeric(mcd$Serving.Size)

#Aggregate daily percentage on category 
category_nutrition = aggregate(cbind(Total.Fat....Daily.Value., Saturated.Fat....Daily.Value., Cholesterol....Daily.Value., Sodium....Daily.Value., Carbohydrates....Daily.Value., Dietary.Fiber....Daily.Value., Vitamin.A....Daily.Value., Vitamin.C....Daily.Value., Calcium....Daily.Value., Iron....Daily.Value.) ~ Category, data = mcd, FUN = mean)

daily_intake = as.data.frame(colSums(category_nutrition[,c(2:11)]))
colnames(daily_intake) = c("% Daily Intake")

#Carbs - Violin Plot
ggplot(mcd, aes(x = factor(Category), y = Carbohydrates....Daily.Value.)) +
  geom_violin(aes(fill = factor(Category))) + 
  geom_boxplot(width = 0.01) + 
  ggtitle("Violin plot -- Carbohydrates") + 
  labs(x = "Category", y = "% Carbs of daily recommended")

#Vitamin A - Violin Plot
ggplot(mcd, aes(x = factor(Category), y = Vitamin.A....Daily.Value.)) +
  geom_violin(aes(fill = factor(Category))) + 
  geom_boxplot(width = 0.01) + 
  ggtitle("Violin plot -- Vitamin A") + 
  labs(x = "Category", y = "% Vitamin A of daily recommended")


#Calcium - Violin Plot  
ggplot(mcd, aes(x = factor(Category), y = Calcium....Daily.Value.)) +
  geom_violin(aes(fill = factor(Category))) + 
  geom_boxplot(width = 0.01) + 
  ggtitle("Violin plot -- Calcium") + 
  labs(x = "Category", y = "% Calcium of daily recommended")

#Food worth a day
mcd_percentage = mcd[, c( "Item", "Category", "Total.Fat....Daily.Value.", "Saturated.Fat....Daily.Value.", "Cholesterol....Daily.Value.", "Sodium....Daily.Value.", "Carbohydrates....Daily.Value.", "Dietary.Fiber....Daily.Value.", "Vitamin.A....Daily.Value.", "Vitamin.C....Daily.Value.", "Calcium....Daily.Value.", "Iron....Daily.Value.")]

for (i in 3:length(names(mcd_percentage))){
  mcd_percentage = mcd_percentage[mcd_percentage[[names(mcd_percentage)[i]]] < 100,]
}

mcd_percentage$rowsum = rowSums(mcd_percentage[, 3:12])

category_min_sum = aggregate(rowsum ~ Category, data = mcd_percentage, FUN = min)
mcd_min_selects = merge(category_min_sum, mcd_percentage, by = c("Category", "rowsum"))
colSums(mcd_min_selects[, 4:13])

category_max_sum = aggregate(rowsum ~ Category, data = mcd_percentage, FUN = max)
mcd_max_selects = merge(category_max_sum, mcd_percentage, by = c("Category", "rowsum"))
colSums(mcd_max_selects[, 4:13])

#Cholestrol more than a day's share
mcd[mcd$Cholesterol....Daily.Value. > 100, cbind("Item", "Cholesterol....Daily.Value.")]

#Top Calorific items
head(mcd[with(mcd, order(by = -mcd$Calories)),"Item"])

mcd$cals_per_unit = mcd$Calories/mcd$Serving.Size

mcd_food = subset(mcd, mcd$food == TRUE)
head(mcd_food[with(mcd_food, order(by = -mcd_food$cals_per_unit)), "Item"]) #Top cals per gram
head(mcd_food[with(mcd_food, order(by = mcd_food$cals_per_unit)), "Item"]) #Low cals per gram

mcd_drink = subset(mcd, mcd$drink == TRUE)
head(mcd_drink[with(mcd_drink, order(by = -mcd_drink$cals_per_unit)), "Item"]) #Top cals per gram
head(mcd_drink[with(mcd_drink, order(by = mcd_drink$cals_per_unit)), "Item"]) #Low cals per gram

calories_agg = aggregate(Calories ~ Category, data = mcd, FUN = mean)
#Assuming the recommended 2000 calories intake
recommended_cal = 2000
paste("Smoothies & Shakes calorific percentage --", round(calories_agg$Calories[calories_agg$Category == "Smoothies & Shakes"]/recommended_cal*100, 2))
paste("Coffee & Tea calorific percentage --", round(calories_agg$Calories[calories_agg$Category == "Coffee & Tea"]/recommended_cal*100, 2))

#SIMILAR ITEMS
get_similarity = function(x, y){
  return(x %*% y / sqrt(x%*%x * y%*%y))
}

mcd_percentage[, 3:12] = mcd_percentage[, 3:12]/mcd_percentage$rowsum
mcd_percentage[is.na(mcd_percentage)] = 0

sim_df = data.frame()

for (i in 1:nrow(mcd_percentage)){
  print(i)
  for (j in i+1:nrow(mcd_percentage)){
    sim = get_similarity(as.vector(as.matrix(mcd_percentage[i, 3:12])), as.vector(as.matrix(mcd_percentage[j, 3:12])))
    
    if (!is.na(sim) & sim > 0.997 & sim < 1 & adist(mcd_percentage$Item[i], mcd_percentage$Item[j]) > 6){
      sim_df = rbind(sim_df, cbind(mcd_percentage$Item[i], mcd_percentage$Item[j], as.numeric(sim)))
    }
  }
}

sim_df$V3 = as.numeric(as.character(sim_df$V3))
