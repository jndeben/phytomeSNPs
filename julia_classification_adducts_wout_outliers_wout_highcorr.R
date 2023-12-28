library('dplyr')
# install.packages('caret')
library('caret')

setwd("D:/j/Documents/julia/")

dataset = read.csv('Phytome_data_T1234.csv')

dataset_t4 = dataset[dataset$Timepoint == 4, ]

dataset_t4_adducts = dataset_t4[!is.na(dataset_t4$Adducts), ]
# dataset_t4_adducts = dataset_t4_adducts[, 1:45]
# dataset_t4_adducts = dataset_t4_adducts[, c("Sex", "BMI", "Age",  "GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT", "Adducts_Change", 
#                                       subset(colnames(dataset_t4_adducts), grepl('Adducts_Change|TM_', colnames(dataset_t4_adducts))))]
dataset_t4_adducts = dataset_t4_adducts[, c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT", "Adducts_Change")]

# dataset_t4_adducts = dataset_t4_adducts[, c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT", "Adducts_Change")]
# dataset_t4_adducts = dataset_t4_adducts[, !grepl('Adducts_Change_|TM_Change_T4T2', colnames(dataset_t4_adducts))]

dataset_t4_adducts[, c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT")] = 
  dataset_t4_adducts[, c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT")] %>% 
  lapply(as.factor)

dummies <- dummyVars(Adducts_Change ~ ., data = dataset_t4_adducts)
dataset_t4_adducts_var = predict(dummies, newdata = dataset_t4_adducts)
# dataset_t4_adducts_var = dataset_t4_adducts_var[, -ncol(dataset_t4_adducts_var)]

# dataset_t4_adducts = cbind.data.frame(dataset_t4_adducts_var, dataset_t4_adducts[, setdiff(colnames(dataset_t4_adducts), c("GSTM1",  "GSTT1", "COMT", "NQO1", "XRCC1", "MGMT"))])
dataset_t4_adducts = cbind.data.frame(dataset_t4_adducts_var, Adducts_Change = dataset_t4_adducts[, 'Adducts_Change']) %>% 
  as.data.frame()

duplicated_columns <- duplicated(as.list(dataset_t4_adducts))
colnames(dataset_t4_adducts[duplicated_columns])
dataset_t4_adducts = dataset_t4_adducts[!duplicated_columns]

dataset_t4_adducts = dataset_t4_adducts %>% 
  na.omit()

dataset_t4_adducts = dataset_t4_adducts[, !grepl('99', colnames(dataset_t4_adducts))]
dataset_t4_adducts = dataset_t4_adducts[, !grepl('NQO1.2|MGMT.2', colnames(dataset_t4_adducts))]

nzv <- nearZeroVar(dataset_t4_adducts)
if (length(nzv) > 0) {
  dataset_t4_adducts <- dataset_t4_adducts[, -nzv]
}

cor_dataset_t4_adducts = cor(dataset_t4_adducts)
highlyCorDescr = findCorrelation(cor_dataset_t4_adducts)
if (length(highlyCorDescr) > 0) {
  dataset_t4_adducts = dataset_t4_adducts[, -highlyCorDescr]
}


saveRDS(dataset_t4_adducts, 'phytome_curated_adducts_wout_outliers_wout_highcorr.rds')


# Server part -------------------------------------------------------------



library('h2o')
library(dplyr)
h2o.init()

dataset_t4_adducts = readRDS('phytome_curated_adducts_wout_outliers_wout_highcorr.rds')

dataset_t4_adducts$Adducts_Change = as.factor(dataset_t4_adducts$Adducts_Change < mean(dataset_t4_adducts$Adducts_Change)) # mean(dataset_t4_adducts$Adducts_Change) = -6.55

# colnames(dataset_t4_adducts)[ncol(dataset_t4_adducts)] = "Adducts_Change"

# dataset_t4_adducts = dataset_t4_adducts[, !grepl('MGMT.0|MGMT.2|MGMT.99', colnames(dataset_t4_adducts))]
# dataset_t4_adducts$MGMT.1 = as.factor(dataset_t4_adducts$MGMT.1)

training_samples = rownames(dataset_t4_adducts) %>% sample(nrow(dataset_t4_adducts)*0.8)

training_data = dataset_t4_adducts[training_samples, ]
testing_data = dataset_t4_adducts[!(row.names(dataset_t4_adducts) %in% training_samples), ]


train <- as.h2o(training_data)
test <- as.h2o(testing_data)
whole_dtst = as.h2o(dataset_t4_adducts)

# convert columns to factors

# set the predictor and response columns

y <- "Adducts_Change"
x <- setdiff(names(train), y)

# train[, y] <- as.factor(train[, y])
# test[, y] <- as.factor(test[, y])

# build a GLM model
aml <- h2o.automl(x = x, y = y,
                  training_frame = whole_dtst,
                  max_models = 525,
                  project_name = 'Adducts_Change')

# View the AutoML Leaderboard
lb <- aml@leaderboard %>% as.data.frame()
print(lb, n = 5)

write.csv(x = lb, file = 'leaderboard_aML_adducts_wout_outliers_wout_highcorr.csv')
# predict using the GLM model and the testing dataset

# predict_train <- h2o.predict(object = aml, newdata = train)
# predict_train_df = predict_train %>% as.data.frame()
# pred_train_df= cbind.data.frame(predict_train_df, training_data$Adducts_Change)
# 
# pred_train_df %>% cor
# cor.test(pred_train_df[,1], pred_train_df[, 2])
# 
# 
# predict <- h2o.predict(object = aml, newdata = test)
# predict_df = predict %>% as.data.frame()
# predict_df$predict = as.factor(predict_df$`TRUE.` > 0.5)
# pred_test_df= cbind(predict_df, testing_data$Adducts_Change)
# 
# confusionMatrix(pred_test_df$predict, testing_data$Adducts_Change)
# cor.test(as.numeric(pred_test_df$predict), as.numeric(pred_test_df$`testing_data$Adducts_Change`))


varimp = h2o.varimp(aml@leader) %>% as.data.frame()
write.csv(x = varimp, 'variable_importance_leader_aML_adducts_wout_outliers_wout_highcorr.csv')

saveRDS(object = aml, 'H2OAutoML_adducts_wout_outliers_wout_highcorr.rds')


# barplot(a$relative_importance, names.arg = a$variable, las =2, cex.names=0.5)
# dev.off()
