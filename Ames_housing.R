library(mice)
library(e1071)
library(caret)
library(viridisLite)
library(corrgram)



tr<-read.csv("C:/Users/User/Desktop/Spring2018/Ames Housing/train.csv")
test<-read.csv("C:/Users/User/Desktop/Spring2018/Ames Housing/test.csv")


plot(SalePrice~.,tr)



num_tr <- subset(tr, select= c(Id,MSSubClass, LotFrontage, LotArea,OverallQual,OverallCond,
                               YearBuilt, YearRemodAdd, MasVnrArea, BsmtFinSF1, BsmtFinSF2,
                               BsmtUnfSF, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, LowQualFinSF,
                               GrLivArea, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath,
                               BedroomAbvGr, KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageYrBlt,
                               GarageCars, GarageArea, WoodDeckSF, OpenPorchSF, EnclosedPorch,
                               X3SsnPorch, ScreenPorch, PoolArea, MiscVal, MoSold, YrSold,
                               SalePrice))

cor_mat<- cor(num_tr, use = "complete.obs", method = "pearson")

sort(cor_mat[, "SalePrice"])

median_output <- tapply(as.numeric(tr$SalePrice),  list(tr$OverallQual), median)

median_output_n <- tapply(as.numeric(tr$SalePrice),  list(tr$Neighborhood), median)

tr$Neighborhood_Exp <-0
tr$Neighborhood_Exp[tr$Neighborhood %in% c("NoRidge","NridgHt","Somerst","StoneBr","Timber","218000")]<-1

tr$Fireplaces_Yes <-0
tr$Fireplaces_Yes[!(tr$Fireplaces==0)]<-1

tr$KitchenQual_Ex <-0
tr$KitchenQual_Ex[tr$KitchenQual %in% c("Ex")]<-1

tr$KitchenQual_Gd <-0
tr$KitchenQual_Gd[tr$KitchenQual %in% c("Gd")]<-1

tr$BsmtQual_Ex <-0
tr$BsmtQual_Ex[tr$BsmtQual %in% c("Ex")]<-1
#
tr$BsmtQual_Gd <-0
tr$BsmtQual_Gd[tr$BsmtQual %in% c("Gd")]<-1

tr$Exterior1st_Exp <-0
tr$Exterior1st_Exp[tr$Exterior1st %in% c("CemntBd","VinylSd","Stone","ImStucc")]<-1

tr$MasVnrType_Stone <-0
tr$MasVnrType_Stone[tr$MasVnrType %in% c("Stone")]<-1

tr$MasVnrType_BrkF <-0
tr$MasVnrType_BrkF[tr$MasVnrType %in% c("BrkFace")]<-1


#model <-lm(SalePrice~OverallQual+ X1stFlrSF+BsmtQual_Gd+BsmtQual_Ex +KitchenQual_Gd+KitchenQual_Ex+Fireplaces_Yes+TotalBsmtSF+ GarageArea+GarageCars+GrLivArea+LotArea+Neighborhood_Exp, tr)
model <-lm(SalePrice~OverallQual+WoodDeckSF+MasVnrType_BrkF+MasVnrType_Stone+YearRemodAdd +X2ndFlrSF+BsmtFinSF1+Exterior1st_Exp+X1stFlrSF+BsmtQual_Gd+BsmtQual_Ex +KitchenQual_Gd+KitchenQual_Ex+Fireplaces_Yes+TotalBsmtSF+ GarageArea+GarageCars+GrLivArea+LotArea+Neighborhood_Exp, tr)

prediction <- predict(model, tr)

cost <-0

for(i in  1:length(prediction)){
  cost = cost + (prediction[i]-tr$SalePrice[i])^2
}

rmse <- (cost/length(prediction))^(1/2)
rmse

#for test

test$Neighborhood_Exp <-0
test$Neighborhood_Exp[test$Neighborhood %in% c("NoRidge","NridgHt","Somerst","StoneBr","Timber","218000")]<-1

test$Fireplaces_Yes <-0
test$Fireplaces_Yes[!(test$Fireplaces==0)]<-1

test$KitchenQual_Ex <-0
test$KitchenQual_Ex[test$KitchenQual %in% c("Ex")]<-1

test$KitchenQual_Gd <-0
test$KitchenQual_Gd[test$KitchenQual %in% c("Gd")]<-1

test$BsmtQual_Ex <-0
test$BsmtQual_Ex[test$BsmtQual %in% c("Ex")]<-1

test$BsmtQual_Gd <-0
test$BsmtQual_Gd[test$BsmtQual %in% c("Gd")]<-1

test$Exterior1st_Exp <-0
test$Exterior1st_Exp[test$Exterior1st %in% c("CemntBd","VinylSd","Stone","ImStucc")]<-1

test$MasVnrType_Stone <-0
test$MasVnrType_Stone[test$MasVnrType %in% c("Stone")]<-1

test$MasVnrType_BrkF <-0
test$MasVnrType_BrkF[test$MasVnrType %in% c("BrkFace")]<-1

prediction_test <- predict(model, test)

write.csv(prediction_test,"C:/Users/User/Desktop/Spring2018/Ames Housing/prediction.csv")

