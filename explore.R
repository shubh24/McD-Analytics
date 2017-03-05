library(stringr)
library(ggplot2)

mcd = read.csv("menu.csv")
mcd$Item = as.character(mcd$Item)

mcd$Serving.Size = as.character(mcd$Serving.Size)
mcd$Serving.Size = gsub(".*\\((.*)\\).*", "\\1", mcd$Serving.Size)
mcd$Serving.Size[str_detect(mcd$Serving.Size, "oz")] = paste(as.integer(29.5735*as.numeric(gsub("([0-9]+).*$", "\\1", mcd$Serving.Size[str_detect(mcd$Serving.Size, "oz")]))), "ml")

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

#play with calories