# no feature engineering at all
source('gbm_utils.R')

offset <- 1
num_train <- num_validate <- 3000000
num_test <- 1000000
num_stats <- 0
num_submission <- 6622629 #min(0, 6622629)

train <- read.csv('../data/train.csv', na.string = "NULL")#, nrows=100000)
test <- read.csv('../data/test.csv', na.string = "NULL")#, nrows=100000)

# compute expected counts and discretize for contiguous
train$score <- pmin(5.0, train$booking_bool * 5 + train$click_bool)
prior <- mean(train$score)
test$score <- -1

train <- subset(train, select = -c(position, click_bool, gross_bookings_usd, booking_bool))
data<-rbind(train, test)

data <- split_data(data, offset, num_train, num_validate, num_stats, num_test, num_submission)

dim(data)

data$date_time_dist <- as.numeric(strftime(as.Date(data$date_time) + data$srch_booking_window, format = "%j"))

gbm_formula <- "score ~ date_time_dist+site_id+visitor_location_country_id+visitor_hist_starrating+visitor_hist_adr_usd+prop_country_id+prop_id+prop_starrating+prop_review_score+prop_brand_bool+prop_location_score1+prop_location_score2+prop_log_historical_price+price_usd+promotion_flag+srch_destination_id+srch_length_of_stay+srch_booking_window+srch_adults_count+srch_children_count+srch_room_count+srch_saturday_night_bool+srch_query_affinity_score+orig_destination_distance+comp1_rate+comp1_inv+comp1_rate_percent_diff+comp2_rate+comp2_inv+comp2_rate_percent_diff+comp3_rate+comp3_inv+comp3_rate_percent_diff+comp4_rate+comp4_inv+comp4_rate_percent_diff+comp5_rate+comp5_inv+comp5_rate_percent_diff+comp6_rate+comp6_inv+comp6_rate_percent_diff+comp7_rate+comp7_inv+comp7_rate_percent_diff+comp8_rate+comp8_inv+comp8_rate_percent_diff"

train_sample <- data[(data$split==1) | (data$split==3), ]
gbm.ndcg <- gbm(as.formula(gbm_formula), data=train_sample, train.fraction=num_train/(num_train+num_validate), n.trees=3000, interaction.depth=8, n.minobsinnode=20, shrinkage=0.005, bag.fraction=0.5, verbose=TRUE, cv.folds=0, keep.data=TRUE, n.cores=16, distribution=list(name="pairwise", metric="ndcg", max.rank=38, group="srch_id"))

best.iter.ndcg <- gbm.perf(gbm.ndcg, method='test')
title('Training of pairwise model with ndcg metric')

summary(gbm.ndcg, n.trees=best.iter.ndcg, main='pairwise (ndcg)')
save(gbm.ndcg, file="../Models/gbm.ndcg.basic.heavy.RData")

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
#submission_p <- subset(submission_p, select = -c(pred))
names(submission_p) <- c("SearchId","PropertyId","Prediction")
write.table(submission_p, "../Submissions/submssision_gbm.basic.heavy.csv", sep = ",", row.names=FALSE, quote=FALSE)

