df<-read.csv("c:/Bankruptcy/data/bankruptcy_dum.csv")
head(df)

library(dplyr)
library(car)
model<-lm(amt~., data = df)
summary(model)

#library(VIM)
#library(mice)

#win.graph()
#md.pattern(df)

#aggr(df, prop = F, numbers = T)

#matrixplot(df)

# 스케일링
library(reshape)

df_scaled<-as.data.frame(scale(df[0:37])) 
df_scaled

library(car)
model<-lm(amt~ ., data = df_scaled)
summary(model)


#meltData <- melt(df_scaled)
#win.graph()
#boxplot(data=meltData, value~variable)
                 
#library(caret)
#meltData <-melt(df[4:22])

#win.graph()
#boxplot(data=meltData, value~variable)

#prep <- preProcess(df[4:22], c("center", "scale"))
#df_scaled <- predict(prep, df[4:22])
#head(df_scaled)

#meltData <- melt(df_scaled)
#win.graph()
#boxplot(data=meltData, value~variable)

#range: 0~1 정규화
library(caret)
prep <- preProcess(df[1:37], c("range"))
df_scaled1 <- predict(prep, df[1:37])
head(df_scaled1)

library(car)
model<-lm(amt~ ., data = df_scaled1)
summary(model)
#meltData <- melt(df_scaled1)
#win.graph()
#boxplot(data=meltData, value~variable)
 


outlierTest(model) # 아웃라이어 확인 

model2<-lm(amt~ ., data = df_scaled[c(-7764,-7310,-5178,-7204,-7431,-627,-7215,
                             -7561,-6849,-6277)]) 
summary(model2)  

#vif(model2)

#win.graph()
#influencePlot(model)

 
reduced <-step(model, direction = "backward")

summary(reduced)
 

library(party)

model4 <- ctree(amt ~ .-amt_class , data=df)
model4

summary(model4)
 

library(randomForest)
set.seed(1)

model5 <- randomForest(amt ~ .-amt_class , data=df, mtry = floor(sqrt(ncol(df))), ntree = 5, importance = T)
model5

summary(model5)

























         
