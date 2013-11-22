# combine several model's prediction using gbm
source('gbm_utils.R')


# 3m 3m 0m 1m + submission
load(file="../data/predictall.gbm.basic.3m3m0m1m.heavy.6.5k.RData")
basic <- data_p

# 1m 1m 4m 1m + submission, not the same data
#load(file="../data/gbm.ndcg.exp.cross.0.49498.1m.all_prediction.RData")
#cross <- data_p

# lr data
lr <- read.csv('../data/predictall.lrbatch.1k.csv', na.string = "NULL", header=F)

# double check they match
dim(basic)
dim(lr)

#dim(cross)
#sum((basic$srch_id == cross$srch_id) & (basic$prop_id == cross$prop_id))
#train <- read.csv('../data/train.csv', na.string = "NULL")
#sum(basic$srch_id[1:6000000] == train$srch_id[1:6000000])
#sum(basic$prop_id[1:6000000] == train$prop_id[1:6000000])
#sum((basic$srch_id[6000001:7000000] == train$srch_id[(dim(train)[1]-1000000+1):dim(train)[1]]) & (basic$prop_id[6000001:7000000] == train$prop_id[(dim(train)[1]-1000000+1):dim(train)[1]]))

data <- data.frame("srch_id" = basic$srch_id, "prop_id" = basic$prop_id, "score" = basic$score, "basic" = basic$pred, "lr" = lr$V1)

data <- split_data(data, 0, 4000000, 4000000, 0, 1917530, 6622629)
# train on 3m
gbm_formula <- "score ~ basic+lr"

train_sample <- data[(data$split==1) | (data$split==3), ]
train_sample2 <- train_sample
train_sample[1:4000000,] <- train_sample2[4000001:8000000,]
train_sample[4000001:8000000,] <- train_sample2[1:4000000,]

gbm.ndcg <- gbm(as.formula(gbm_formula), data=train_sample, train.fraction=0.5, n.trees=2000, interaction.depth=3, n.minobsinnode=20, shrinkage=0.005, bag.fraction=0.5, verbose=TRUE, cv.folds=0, keep.data=TRUE, n.cores=16, distribution=list(name="pairwise", metric="ndcg", max.rank=38, group="srch_id")) # was n.trees=3000, interaction.depth=8

best.iter.ndcg <- gbm.perf(gbm.ndcg, method='test')
title('Training of pairwise model with ndcg metric')

summary(gbm.ndcg, n.trees=best.iter.ndcg, main='pairwise (ndcg)')
save(gbm.ndcg, file="../Models/gbm.ndcg.combine.4m.RData")

# generate prediction for test
test <- data[data$split == 4, ]
predict.test <- predict(gbm.ndcg, test, best.iter.ndcg)
my_ndcg(test$score, test$srch_id, -predict.test, 38)
# 0.5111782
# 0.5096955


submission <- data[data$split == 5, ]
predict.submission <- predict(gbm.ndcg, submission, best.iter.ndcg)
submission_p <- data.frame("srch_id" = submission$srch_id, "prop_id" = submission$prop_id, "pred" = predict.submission)
submission_p <- submission_p[with(submission_p, order(srch_id, -pred)), ]
submission_p <- subset(submission_p, select = -c(pred))
names(submission_p) <- c("SearchId","PropertyId")#,"Prediction")
write.table(submission_p, "../Submissions/submssision_gbm.combine.4m.3.5k.csv", sep = ",", row.names=FALSE, quote=FALSE)

