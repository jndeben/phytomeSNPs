library('dplyr')
# install.packages('caret')
library('caret')

setwd("D:/j/Documents/julia/")

dataset = read.csv('Phytome_data_T1234.csv')

dataset_t4 = dataset[dataset$Timepoint == 4, ]

dataset_t4_TM = dataset_t4[!is.na(dataset_t4$Adducts), ]
# dataset_t4_TM = dataset_t4_TM[, 1:45]
# dataset_t4_TM = dataset_t4_TM[, c("Sex", "BMI", "Age",  "GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT", "Average_TM_Median_Change", 
#                                       subset(colnames(dataset_t4_TM), grepl('Average_TM_Median_Change|TM_', colnames(dataset_t4_TM))))]
# dataset_t4_atnc = dataset_t4_atnc[, c("GSTM1",  "GSTT1" (remove 4 atnc), "COMT", "NQO1" (remove variant), "XRCC1", "MGMT" (wout variant), "ATNC_Change")]
dataset_t4_TM = dataset_t4_TM[, c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT", "Average_TM_Median_Change")]
# dataset_t4_TM = dataset_t4_TM[, c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT", "Average_TM_Median_Change")]
# dataset_t4_TM = dataset_t4_TM[, !grepl('Average_TM_Median_Change_|TM_Change_T4T2', colnames(dataset_t4_TM))]

dataset_t4_TM[, c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT")] = 
  dataset_t4_TM[, c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT")] %>% 
  lapply(as.factor)

dummies <- dummyVars(Average_TM_Median_Change ~ ., data = dataset_t4_TM)
dataset_t4_TM_var = predict(dummies, newdata = dataset_t4_TM)
# dataset_t4_TM_var = dataset_t4_TM_var[, -ncol(dataset_t4_TM_var)]

# dataset_t4_TM = cbind.data.frame(dataset_t4_TM_var, dataset_t4_TM[, setdiff(colnames(dataset_t4_TM), c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT"))])
dataset_t4_TM = cbind.data.frame(dataset_t4_TM_var, Average_TM_Median_Change = dataset_t4_TM[, 'Average_TM_Median_Change']) %>% 
  as.data.frame()

duplicated_columns <- duplicated(as.list(dataset_t4_TM))
colnames(dataset_t4_TM[duplicated_columns])
dataset_t4_TM = dataset_t4_TM[!duplicated_columns]

dataset_t4_TM = dataset_t4_TM %>% 
  na.omit()

dataset_t4_TM = dataset_t4_TM[, !grepl('99', colnames(dataset_t4_TM))]
dataset_t4_TM = dataset_t4_TM[, !grepl('NQO1.2|MGMT.2', colnames(dataset_t4_TM))]

nzv <- nearZeroVar(dataset_t4_TM)
if (length(nzv) > 0) {
  dataset_t4_TM <- dataset_t4_TM[, -nzv]
}

cor_dataset_t4_TM = cor(dataset_t4_TM)
highlyCorDescr = findCorrelation(cor_dataset_t4_TM)
if (length(highlyCorDescr) > 0) {
  dataset_t4_TM = dataset_t4_TM[, -highlyCorDescr]
}


saveRDS(dataset_t4_TM, 'phytome_curated_TM_wout_outliers_wout_highcorr.rds')


# Server part -------------------------------------------------------------



library('h2o')
library(dplyr)
h2o.init()

dataset_t4_TM = readRDS('phytome_curated_TM_wout_outliers_wout_highcorr.rds')

dataset_t4_TM$Average_TM_Median_Change = as.factor(dataset_t4_TM$Average_TM_Median_Change < mean(dataset_t4_TM$Average_TM_Median_Change)) # mean(dataset_t4_TM$Average_TM_Median_Change) = -6.55

# colnames(dataset_t4_TM)[ncol(dataset_t4_TM)] = "Average_TM_Median_Change"

# dataset_t4_TM = dataset_t4_TM[, !grepl('MGMT.0|MGMT.2|MGMT.99', colnames(dataset_t4_TM))]
# dataset_t4_TM$MGMT.1 = as.factor(dataset_t4_TM$MGMT.1)

training_samples = rownames(dataset_t4_TM) %>% sample(nrow(dataset_t4_TM)*0.8)

training_data = dataset_t4_TM[training_samples, ]
testing_data = dataset_t4_TM[!(row.names(dataset_t4_TM) %in% training_samples), ]


train <- as.h2o(training_data)
test <- as.h2o(testing_data)
whole_dtst = as.h2o(dataset_t4_TM)

# convert columns to factors

# set the predictor and response columns

y <- "Average_TM_Median_Change"
x <- setdiff(names(train), y)

# train[, y] <- as.factor(train[, y])
# test[, y] <- as.factor(test[, y])

# build a GLM model
aml <- h2o.automl(x = x, y = y,
                  training_frame = whole_dtst,
                  max_models = 525,
                  project_name = 'Average_TM_Median_Change')

# View the AutoML Leaderboard
lb <- aml@leaderboard %>% as.data.frame()
print(lb, n = 5)

write.csv(x = lb, file = 'leaderboard_aML_TM_wout_outliers_wout_highcorr.csv')
# predict using the GLM model and the testing dataset

# predict_train <- h2o.predict(object = aml, newdata = train)
# predict_train_df = predict_train %>% as.data.frame()
# pred_train_df= cbind.data.frame(predict_train_df, training_data$Average_TM_Median_Change)
# 
# pred_train_df %>% cor
# cor.test(pred_train_df[,1], pred_train_df[, 2])
# 
# 
# predict <- h2o.predict(object = aml, newdata = test)
# predict_df = predict %>% as.data.frame()
# predict_df$predict = as.factor(predict_df$`TRUE.` > 0.5)
# pred_test_df= cbind(predict_df, testing_data$Average_TM_Median_Change)
# 
# confusionMatrix(pred_test_df$predict, testing_data$Average_TM_Median_Change)
# cor.test(as.numeric(pred_test_df$predict), as.numeric(pred_test_df$`testing_data$Average_TM_Median_Change`))


varimp = h2o.varimp(aml@leader) %>% as.data.frame()
write.csv(x = varimp, 'variable_importance_leader_aML_TM_wout_outliers_wout_highcorr.csv')

saveRDS(object = aml, 'H2OAutoML_TM_wout_outliers_wout_highcorr.rds')


# barplot(a$relative_importance, names.arg = a$variable, las =2, cex.names=0.5)
# dev.off()
