library(tidyverse)
library(caret)

getwd()
list.files()

train_house <- read_csv('../train.csv')
test_house <- read_csv('../test.csv')

train_house <- train_house %>% 
  mutate(Notes = 'Train')
test_house <- test_house %>% 
  mutate(Notes = 'Test')

df_full <- train_house %>% 
  bind_rows(test_house)

df_full<- df_full %>% 
  mutate_if(is.factor, as.character)

df_full<- df_full %>% 
  mutate(Remodel = if_else(YearBuilt == YearRemodAdd, 'Y', 'N'))

df_full<- df_full %>%
  mutate(MSSubClass = as.factor(MSSubClass),
         OverallQual = as.factor(OverallQual),
         OverallCond = as.factor(OverallCond))

df_full <- df_full %>% 
  mutate(HouseAgeOld = YrSold - YearBuilt,
         HouseAgeNew = YrSold - YearRemodAdd)

summary(df_full)


# Replace NA --------------------------------------------------------------

is.na(df_full) %>% colSums()

## LotFrontage ===

df_full$LotFrontage[is.na(df_full$LotFrontage)] <- 0

## Alley ===

df_full$Alley[is.na(df_full$Alley)] <- 'None'

## MasVnrArea ===

df_full %>% 
  select(MasVnrType, MasVnrArea) %>% 
  filter(MasVnrType == 'None', MasVnrArea != 0)

df_full$MasVnrArea[df_full$MasVnrType == 'None'] <- 0

## BsmtQual ===

df_full$BsmtQual[df_full$BsmtQual == 'None'] <- NA

## BsmtQual ===

df_full$BsmtQual[is.na(df_full$BsmtQual)] <- 'None'

## BsmtQual ===

df_full$BsmtQual[is.na(df_full$BsmtQual)] <- 'None'

df_full %>% 
  filter(is.na(BsmtQual1)|
         is.na(BsmtCond1)|
         is.na(BsmtExposure1)) %>% 
  select(BsmtQual1, BsmtCond1, BsmtExposure1) %>% view()

df_full <- df_full %>% 
  mutate(BsmtQual1 = if_else((is.na(BsmtQual)&is.na(BsmtCond)&is.na(BsmtExposure)), 'None', BsmtQual),
         BsmtCond1 = if_else((is.na(BsmtQual)&is.na(BsmtCond)&is.na(BsmtExposure)), 'None', BsmtCond),
         BsmtExposure1 = if_else((is.na(BsmtQual)&is.na(BsmtCond)&is.na(BsmtExposure)), 'None', BsmtExposure))

df_full %>% 
  count(BsmtQual1, BsmtCond1, BsmtExposure1) %>% 
  filter(BsmtQual)

df_bsmt <- df_full %>% 
  select(BsmtQual1, BsmtCond1, BsmtExposure1) %>% 
  mutate_if(is.character, as.factor)
  
TrainDummBsmt <- dummyVars(~., df_bsmt)
df_bsmt_dumm <- predict(TrainDummBsmt, df_bsmt)

preprocess_bsmt <- preProcess(df_bsmt_dumm, method = 'bagImpute')
df_bsmt_imputed <- predict(preprocess_bsmt, df_bsmt_dumm)

# Explore -----------------------------------------------------------------

# MSSubClass ---

table(df_full$MSSubClass)

train_house %>% 
  ggplot(aes(as.factor(MSSubClass), SalePrice)) +
  geom_boxplot()

anova_massubclass <- aov(lm(SalePrice ~ as.factor(MSSubClass), train_house))
summary(anova_massubclass)
TukeyHSD(anova_massubclass) %>% plot()

# MSZoning ---

table(df_full$MSZoning, useNA = 'ifany')

train_house %>% 
  ggplot(aes(as.factor(MSZoning), SalePrice)) +
  geom_boxplot()

aov(lm(SalePrice ~ as.factor(MSZoning), train_house)) %>% summary()

# Lot Frontage ---

