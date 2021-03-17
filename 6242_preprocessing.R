library(dplyr)

#read in training data
df1 <- read.csv("train_refined1.csv",header=TRUE,sep=",")
df2 <- read.csv("train_refined2.csv",header=TRUE,sep=",")
df3 <- read.csv("train_refined3.csv",header=TRUE,sep=",")
df4 <- read.csv("train_refined4.csv",header=TRUE,sep=",")

#stack
total <- rbind(df1,df2,df3,df4)

#get rid of columns 1,2,3,7,8
total_2 <- subset(total,select=c(4,5,6,9,10))

#FULL_TIME_POSITION column - Y to 1, N to 0
total_2 <- total_2 %>%
  mutate(FULL_TIME = ifelse(FULL_TIME_POSITION=="Y",1,0))

#remove FULL_TIME_POSITION column
total_2$FULL_TIME_POSITION <- NULL

total_2$SOC_NAME <- as.factor(total_2$SOC_NAME)
total_2$STATE <- as.factor(total_2$STATE)

#test dataset
df_test <- read.csv("test_refined.csv",header=TRUE,sep=",")
df_test$STATE <- df_test$WORKSITE_STATE
df_test$WORKSITE_STATE <- NULL
df_test$FULL_TIME1 <- df_test$FULL_TIME
df_test$FULL_TIME <- NULL
df_test$FULL_TIME <- df_test$FULL_TIME1
df_test$FULL_TIME1 <- NULL
df_test <- df_test %>%
  mutate(CERTIFIED = ifelse(CASE_STATUS=="CERTIFIED",1,0))
df_test$CASE_STATUS <- NULL
df_test$PW_UNIT_OF_PAY <- NULL

df_test$SOC_NAME <- as.factor(df_test$SOC_NAME)
df_test$STATE <- as.factor(df_test$STATE)

test_data1 <- df_test[(df_test$SOC_NAME %in% total_2$SOC_NAME),]

test_data1$FULL_TIME <- NULL

#FULL_TIME_POSITION column - Y to 1, N to 0
test_data1 <- test_data1 %>%
  mutate(FULL_TIME = ifelse(FULL_TIME_POSITION=="Y",1,0))

test_data1$FULL_TIME_POSITION <- NULL

#export to csv
write.csv(total_2,"train.csv")
write.csv(test_data1,"test.csv")