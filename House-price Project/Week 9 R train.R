library(tidyverse)
library(broom)
library(ggplot2)
install.packages("stargaze")
library(stargazer)
library(tidyverse)
library(dplyr)

# download the dat 

dat <- readRDS("data/train.rds")



#Linear Models 

mod1 <- lm(SalePrice ~ AdjSalePrice, data = dat)

mod2 <- lm(AdjSalePrice ~ SqFtLot, data = dat)

mod3 <- lm(AdjSalePrice ~ SqFtTotLiving, data = dat)

mod4 <- lm(AdjSalePrice ~ SqFtFinBasement, data = dat)

m1 <- stargazer(mod1, type = "latex", title = "Summary")

m2 <- stargazer(mod3, type = "text", title = "Summary 2")

#Bedrooms and bathrooms 


#Visual SalePrice and AdjSalePrice
ggplot(data = dat, aes(x = SalePrice, y = AdjSalePrice, group = as.factor(PropertyType))) + 
  geom_point(aes(color = as.factor(PropertyType))) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sale price")

#Visual 2 

ggplot(data = dat, aes(x = SqFtTotLiving, y = AdjSalePrice, group = as.factor(PropertyType))) + 
  geom_point(aes(color = as.factor(PropertyType))) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sale price")

#Predict/Augment 
stargazer(mod1,mod3)
dat_add <- augment(mod1, mod3)


m4 <- stargazer(mod1, mod3, type = "html", title = "Summary for both variables")


#week 10 
#Grants or money should not be used as an categorical/interval, since it is continuous 
#Model Matrix 

?model.matrix

#Building Grade column 
#LQC, linear, quadratic and cubical 

#Using location - zipcode 
#  group_by(ZipCode) %>%

#using best model to predict/estimate with the left of the residuals, and 

#predictor variables in your dataset

summary(dat$SqFtTotLiving, rm.na = TRUE)

ZipGroup <- dat$ZipCode 
ZipFactor <- as.factor(dat$ZipCode)

View(ZipFactor)
mod5 <- lm(SqFtLot  ~ SqFtTotLiving + ZipGroup, data = dat)
stargazer(mod5, type = "latex")

#Residuals
m_residuals <- resid(mod5)

View(m_residuals)

m_residuals <- resid(ZipFactor)

#plot residuals 
plot(m_residuals)

#Residuals of the model in a separate object


#Natasha code 

zip_group <- dat %>%
  group_by(ZipCode) %>%
  summarise(med_price = median(SalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         ZipGroup = ntile(cumul_count, 5))
dat <- dat %>%
  left_join(select(zip_group, ZipCode, ZipGroup), by = "ZipCode")
mod4 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + ZipGroup, data = dat)

stargazer(mod4, type = "latex")


#Plot Brandon 

pairs(~dat$SqFtLot + dat$SqFtTotLiving + dat$SalePrice +
        dat$AdjSalePrice, na.rm=TRUE,
      col = "darkgreen", pch = 19,
      labels = c("$SqFtLot",
                 "$SqFtTotLiving",
                 "$SalePrice",
                 "AdjSalePrice"),
      main = "Relationship between Square Feet of Lot, Square Feet of Total Living Space,
      Sale Prices, Adjusted Sale Prices")


#Marcus

# Predictor variable : Year built

dat %>%
  group_by(YrBuilt) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  ggplot(aes(as.factor(reorder(YrBuilt, n)), n)) +
  geom_col() +
  coord_flip() +
  xlab("Year Built")

yr_built <- dat %>%
  group_by(YrBuilt) %>%
  summarise(med_price = median(AdjSalePrice),
            count = n()) %>%
  arrange(med_price) %>%
  mutate(cumul_count = cumsum(count),
         YrBuilt = ntile(cumul_count, 5))

dat <- dat %>%
  left_join(select(yr_built, YrBuilt, YrBuilt), by = "YrBuilt")

mod6 <- lm(AdjSalePrice ~ SqFtTotLiving + BldgGrade + YrBuilt, data = dat)

stargazer(mod6, type = "latex")


res <- resid(ZipFactor)
