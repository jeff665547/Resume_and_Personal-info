#設定工作路徑(檔案放置的地方)
setwd("C:/Users/jeff/Desktop/final report") #將\改成/才有辦法在R裡面跑

#step0.1 先將excel檔轉成csv檔以便資料讀取
data <- read.csv("Percentage body fat.csv")

#step0.2 快速瀏覽data長的樣子，讓我們對data又初步的認識
head(data)
###各個欄位的資料(從pdf檔案複製過來的)
#    pbfb: Percent body fat using Brozek's equation
#    pbfs: Percent body fat using Siri's equation 
#    density: body mass density (gm/cm^3) 
#    ages: Age (yrs)
#    weights: Body Weight (kg)
#    heights: Body Height (cm)
#    bmi: Body Mass Index
#    fatfreew: Fat Free Weight = 
#              (1 - fraction of body fat) * Weight, using Brozek's formula (kg) 
#    neckc: Neck circumference (cm)
#    chestc: Chest circumference (cm)
#    abdomanc: Abdomen circumference (cm)
#             "at the umbilicus and level with the iliac crest" 
#    hipc: Hip circumference (cm)
#    thighc: Thigh circumference (cm)
#    kneec: Knee circumference (cm)
#    anklec: Ankle circumference (cm)
#    bicepc: Extended biceps circumference (cm)
#    forearmc: Forearm circumference (cm)
#    wristc: Wrist circumference (cm)

#matrix 矩陣 陣列
matrix(c(1,2,3,4,5,6), 2, 3)

#vector 向量
id1 <- c(22, 63, 160)
id2 <- c(23, 66, 120)

sum(matrix(c(1,2,3,4,5,6), 2, 3))
c(1,"AA",3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)[2]
data[, 2]

#step 1 資料清理
#step 1.1 檢查遺失值
sum(is.na(data))  #sum(matrix(c(1,2,3,4), 2, 2))概念  無遺失值

#step 1.2 檢查資料分布情形以及合理性(boxplot)
#e.g. circumference, age, weights, heights, bmi 應該都要>0, percent相關變數應該介於0~100之間
par(mfrow = c(1,1))
boxplot(data[, 2:10]) 
boxplot(data[, 11:19]) 

data$id[data$pbfb == 0]  #id 182
data$id[data$pbfs == 0]  #id 182
data$id[data$density == 0]  #null
#結論: id 182 的pdfb和pdfs都是0 不確定是否合理

#step 1.3 計算各個欄位(變數)的平均數、標準差、最大值、中位數、最小值
mean = round(apply(data[, -1], 2, mean), 3)
std = round(apply(data[, -1], 2, sd), 3)
maximum = round(sapply(data[, -1], max), 3)
median = round(sapply(data[, -1, ], median), 3)
minimum = round(sapply(data[, -1], min), 3)

summary_table = data.frame(mean, std, maximum, median, minimum)
write.csv(summary_table, "summary_table.csv")

#step 1.4 檢查資料分布情形以及合理性(histogram)
par(mfrow = c(3,6))
for (i in c(2: dim(data)[2])){
  hist(data[, i], xlab = colnames(data)[i], main = colnames(data)[i])
}

hist(data$pbfb, xlab = "pbfb", main = "pbfb")
hist(data$pbfs, xlab = "pbfs", main = "pbfs")
hist(data$density, xlab = "density", main = "density") #疑似有一筆 < 1
hist(data$ages, xlab = "ages", main = "ages")
hist(data$weights, xlab = "weights", main = "weights") #疑似有一筆 > 160
hist(data$heights, xlab = "heights", main = "heights") #疑似有一筆 < 80
hist(data$bmi, xlab = "bmi", main = "bmi")
hist(data$fatfreew, xlab = "fatfreew", main = "fatfreew") #疑似有一筆 > 100
hist(data$neckc, xlab = "neckc", main = "neckc") #疑似有一筆 > 50
hist(data$chestc, xlab = "chestc", main = "chestc") #疑似有一筆 > 130
hist(data$abdomenc, xlab = "abdomenc", main = "abdomenc") #疑似有一筆 > 140
hist(data$hipc, xlab = "hipc", main = "hipc") #疑似有一筆 > 140
hist(data$thighc, xlab = "thighc", main = "thighc")
hist(data$kneec, xlab = "kneec", main = "kneec")
hist(data$ankelc, xlab = "ankelc", main = "ankelc")
hist(data$biceps, xlab = "biceps", main = "biceps")   #疑似有一筆 > 42
hist(data$forearmc, xlab = "forearmc", main = "forearmc")
hist(data$wristc, xlab = "wristc", main = "wristc")

#step 1.5 check data by using histogram results
data$id[data$density < 1] #id: 216
data$id[data$weights > 160] #id: 39
data$id[data$heights < 80] #id: 42
data$id[data$fatfreew > 100] #id: 39
data$id[data$neckc > 50] #id: 39
data$id[data$chestc > 130] #id: 39
data$id[data$abdomenc > 140] #id: 39
data$id[data$biceps > 42] #id: 39
#結論: id 39確實存在，但這筆資料是outlier，為了能夠更精確地建群體的模型把id 39給刪除

#step 1.6 check bmi
new_bmi <- data$weights/((data$heights/100)^2)
margin_bmi <- round(abs(data$bmi - new_bmi), 2)
sum(margin_bmi > 0.2)

data$bmi[margin_bmi > 0.2]
new_bmi[margin_bmi > 0.2]

data$id[margin_bmi > 2] #id 42, 163, 221 有問題，差距有點大

#step 1.7 check fatfreew
new_fatfreew <- (1- data$pbfb/100)*data$weights
margin_fatfreew <- round(abs(data$fatfreew - new_fatfreew ), 2)
sum(margin_fatfreew > 0.2)

data$fatfreew[margin_fatfreew > 0.2]
new_fatfreew[margin_fatfreew > 0.2]

data$id[margin_fatfreew > 2] #id 221 有問題，差距有點大
#結論: id 42, 163, 221 這3筆資料的bmi還有fatfreew和我們計算的結果存在著差距，
#我們懷疑有資料輸入錯誤的情形發生，因此保險起見我們將此3筆data也刪除

#step 1.8 刪除上述提到的5筆資料，並且重新跑一次敘述統計表
new_data <-  data[-c(39, 42, 163, 182, 221), ]
dim(new_data)
dim(data)


new_mean = round(apply(new_data[, -1], 2, mean), 3)
new_std = round(apply(new_data[, -1], 2, sd), 3)
new_maximum = round(sapply(new_data[, -1], max), 3)
new_median = round(sapply(new_data[, -1, ], median), 3)
new_minimum = round(sapply(new_data[, -1], min), 3)

new_summary_table = data.frame(new_mean, new_std, new_maximum, new_median, new_minimum)
write.csv(new_summary_table, "new_summary_table.csv")



#step 2 Model selection (avoid the multicollinearity)
#step 2.1 Model Evaluation Tools
#install.packages("car")
library(car) #VIF(VIF > 5 delete)
#Root MSE  越小越好
RMSE = function(e, o){
  sqrt(mean((e - o)^2))
}
#R square for test data  越大越好
R2 = function(e, o){
  1 - (sum((e - o)^2)/sum((o - mean(o))^2))
}
###############################################################################
#Data Split (training, testing)
set.seed(1234)
test <- sample(1:247, 47)
test_data = new_data[test, ]
train_data = new_data[-test, ]

#Correlaton of variables
correlation_matrix = round(cor(train_data[, -c(1, 2)]), 2)
rownames(correlation_matrix)[abs(correlation_matrix[, 1]) > 0.5]
#Y: pbfs (pbfb) 
#X: (ages), density, abdomenc, bmi, chestc, weights, hipc, thighc, neckc, biceps, forearmc, wristc (corresponds to the level of correlation)
correlation_matrix[,"density"]  #rho > 0.7: bmi, abdomenc
correlation_matrix[,"abdomenc"] #rho < 0.7: ages, heights, fatfreew, ankelc, biceps, forearmc, wristc
correlation_matrix[,"ages"]
correlation_matrix[,"fatfreew"] #rho < 0.7: ages, ankelc, biceps, forearmc, wristc
correlation_matrix[,"abdomenc"]
correlation_matrix[,"biceps"]   #rho < 0.7: ages, ankelc, fatfreew, wristc

#step 2.A Model selection(Precise Accuracy Version)
#Full Model
model_full <- lm(pbfs ~ . -id - pbfb - pbfs, data = train_data)
summary(model_full)                            #Traing: adj R^2:0.985, p-value: sig
vif(model_full)                                #VIF > 5: density, weights, heights, bmi, fatfreew, abdomenc, chestc, hipc, thighc
predictions <- predict(model_full, test_data)  #Testing Performance:
RMSE(predictions, test_data$pbfs)              #RMSE: 0.6645
R2(predictions, test_data$pbfs)                #R2: 0.9949

#Model 1 從Full model那堆變數(VIF > 5)中選一個留下來(和Y相關性最高的)
model1A <- lm(pbfs ~ density + ages + neckc + biceps + kneec + forearmc + wristc + ankelc, data = train_data)
summary(model1A)                            #Traing: adj R^2:0.9707, p-value: sig
vif(model1A)
predictions <- predict(model1A, test_data)  #Testing Performance: 
RMSE(predictions, test_data$pbfs)          #RMSE: 0.5302
R2(predictions, test_data$pbfs)            #R2: 0.9967

#Model 2 刪除不顯著的變數
model2A <- lm(pbfs ~ density, data = train_data)
summary(model2A)                            #Traing: adj R^2:0.9692, p-value: sig
predictions <- predict(model2A, test_data)  #Testing Performance: 
RMSE(predictions, test_data$pbfs)          #RMSE: 0.2574
R2(predictions, test_data$pbfs)            #R2: 0.9995



#step 2.B Model selection(Esaily Obtain Varables Version)
#Full Model 
model_full <- lm(pbfs ~ . -id - pbfb - pbfs - density - fatfreew, data = train_data)
summary(model_full)                            #Traing: adj R^2:0.7197, p-value: sig
vif(model_full)     #VIF > 5: weights, heights, bmi, chestc, abdomenc, hipc, thighc
predictions <- predict(model_full, test_data)  #Testing Performance:
RMSE(predictions, test_data$pbfs)              #RMSE: 3.9687
R2(predictions, test_data$pbfs)                #R2: 0.7859

#Model 1 從Full model那堆變數(VIF > 5)中選一個留下來(和Y相關性最高的)
model1B <- lm(pbfs ~ ages + abdomenc + neckc + biceps + kneec + forearmc + wristc + ankelc, data = train_data)
summary(model1B)                            #Traing: adj R^2:0.7094, p-value: sig
vif(model1B)
predictions <- predict(model1B, test_data)  #Testing Performance: 
RMSE(predictions, test_data$pbfs)          #RMSE: 3.924
R2(predictions, test_data$pbfs)            #R2: 0.788

#Model 2 從Model 1那堆變數中刪除與abdomenc間相關性高的變數( > 0.7)
model2B <- lm(pbfs ~ ages + abdomenc + biceps + forearmc + wristc + ankelc, data = train_data)
summary(model2B)                            #Traing: adj R^2:0.7094, p-value: sig
vif(model2B)
predictions <- predict(model2B, test_data)  #Testing Performance: 
RMSE(predictions, test_data$pbfs)          #RMSE: 4.015
R2(predictions, test_data$pbfs)            #R2: 0.778

#Model 3 刪除不顯著的變數
model3B <- lm(pbfs ~ ages + abdomenc + wristc, data = train_data)
summary(model3B)                            #Traing: adj R^2:0.7092, p-value: sig
vif(model3B)
predictions <- predict(model3B, test_data)  #Testing Performance:
RMSE(predictions, test_data$pbfs)          #RMSE: 4.001
R2(predictions, test_data$pbfs)            #R2: 0.78


#Step 3 Diagnostic of goodness of fit
#Step 3.1 Homoscedasticity
#residual plots
#plot residuals vs. X (density)
plot(train_data$density, model2A$residuals, ylab="Residuals", xlab="Density", main = "Residual Plot") 
abline(0, 0)
#可以看到有三個資料點超過正負3的range，過於極端

model2A$residuals[model2A$residuals > 3]
model2A$residuals[model2A$residuals < -5]
#此三個資料點id分別是48 76和96，若將這兩筆資料移除會讓此模型更符合假設，預測力也會更好

new_train = train_data[-which(train_data$id %in% c(48, 76, 96)), ]
newmodel2A <- lm(pbfs ~ density, data = new_train)
summary(newmodel2A)                            #Traing: adj R^2:0.9993, p-value: sig
predictions <- predict(newmodel2A, test_data)  #Testing Performance: 
RMSE(predictions, test_data$pbfs)              #RMSE: 0.1928
R2(predictions, test_data$pbfs)                #R2: 0.9995


#plot residuals vs. fitted values
plot(model3B$fitted.values, model3B$residuals, ylab="Residuals", xlab="Fitted Values", main = "Residual Plot") 
abline(0, 0)

#Step 3.2 Normality of Residuals
#QQ plot
qqPlot(model2A, main="QQ Plot")
qqPlot(newmodel2A, main="QQ Plot")
qqPlot(model3B, main="QQ Plot")

#Distribution of studentized residuals
sresid <- studres(newmodel2A) 
hist(sresid, xlab = "Residuals", freq = FALSE, main="Distribution of Studentized Residuals")
xfit <- seq(min(sresid),max(sresid), length=400) 
yfit <- dnorm(xfit) 
lines(xfit, yfit) 

sresid <- studres(model3B) 
hist(sresid, xlab = "Residuals", freq = FALSE, main="Distribution of Studentized Residuals")
xfit <- seq(min(model3B$residuals),max(model3B$residuals),length=400) 
yfit <- dnorm(xfit) 
lines(xfit, yfit) 

#Test?
