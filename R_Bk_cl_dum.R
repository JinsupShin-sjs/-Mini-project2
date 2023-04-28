df<-read.csv("c:/Bankruptcy/data/bankruptcy_dum.csv")
head(df)

df<-df %>% select(-amt)
head(df)

library(dplyr)
library(car)
model<-lm(amt_class~., data = df)
summary(model)

#library(VIM)
#library(mice)

#win.graph()
#md.pattern(df)

#aggr(df, prop = F, numbers = T)

#matrixplot(df)

# 스케일링
library(reshape)

df_scaled<-as.data.frame(scale(df[0:36])) 
df_scaled

library(car)
model<-lm(amt_class~ ., data = df_scaled)
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
prep <- preProcess(df[1:36], c("range"))
df_scaled1 <- predict(prep, df[1:36])
head(df_scaled1)

library(car)
model<-lm(amt_class~ ., data = df_scaled1)
summary(model)
#meltData <- melt(df_scaled1)
#win.graph()
#boxplot(data=meltData, value~variable)
 


outlierTest(model) # 아웃라이어 확인 

model2<-lm(amt_class~ ., data = df_scaled1[c(-6572,-5844,-7559,-4024)]) 
summary(model2)  

#win.graph()
#influencePlot(model)

 
reduced <-step(model, direction = "backward")

summary(reduced)


#vif(model2)


library(party)

model4 <- ctree(amt_class ~ ., data=df_scaled1)
model4

summary(model4)



library(randomForest)
set.seed(1)

model5 <- randomForest(amt_class ~ . , data=df_scaled1, mtry = floor(sqrt(ncol(df))), ntree = 5, importance = T)
model5

summary(model5)

























         
