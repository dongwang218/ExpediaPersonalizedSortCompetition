source('gbm_utils.R')

offset <- 1
num_train <- num_validate <- 3000000
num_test <- 1000000
num_stats <- 2917530
num_submission <- 6622629 #min(0, 6622629)
# 10 to 40
prior_cnt <- max(100, min(300, as.integer((num_train+num_stats) / 2000000 * 300)))
drop_cnt <- 50

read_train <- min(9917530, offset-1+num_train+num_validate+num_test+num_stats)
read_test <- max(1, num_submission)

#train <- read.csv('../data/train.csv', na.string = "NULL")#, nrows=read_train)
#test <- read.csv('../data/test.csv', na.string = "NULL")#, nrows=read_test)
load(file='../data/train.RData')
load(file='../data/test.RData')

# compute expected counts and discretize for contiguous
train$score <- pmin(5.0, train$booking_bool * 5 + train$click_bool)
prior <- mean(train$score)
test$score <- -1

train <- subset(train, select = -c(position, click_bool, gross_bookings_usd, booking_bool))
data<-rbind(train, test)

data <- split_data(data, offset, num_train, num_validate, num_stats, num_test, num_submission)
dim(data)

data$date_time_dist <- as.numeric(strftime(as.Date(data$date_time) + data$srch_booking_window, format = "%W"))

# too many
data$prop_id_exp <- my_exp1(data, "prop_id", "score", data$split < 3, prior_cnt, prior, 0.2, drop_cnt)


# many
data$srch_destination_id_exp <- my_exp1(data, "srch_destination_id", "score", data$split < 3, prior_cnt, prior, 0.2, drop_cnt)


save(data, file = "../data/train_test_disc_simple.RData")

#exp_names = names(data)[grep(".*(prop_brand_bool|srch_saturday_night_bool|_exp)$", names(data))]
exp_names = names(data)[grep(".*_exp$", names(data))]
gbm_formula <- paste("score ~ date_time_dist+site_id+visitor_location_country_id+visitor_hist_starrating+visitor_hist_adr_usd+prop_country_id+prop_id+prop_starrating+prop_review_score+prop_brand_bool+prop_location_score1+prop_location_score2+prop_log_historical_price+price_usd+promotion_flag+srch_destination_id+srch_length_of_stay+srch_booking_window+srch_adults_count+srch_children_count+srch_room_count+srch_saturday_night_bool+srch_query_affinity_score+orig_destination_distance+comp1_rate+comp1_inv+comp1_rate_percent_diff+comp2_rate+comp2_inv+comp2_rate_percent_diff+comp3_rate+comp3_inv+comp3_rate_percent_diff+comp4_rate+comp4_inv+comp4_rate_percent_diff+comp5_rate+comp5_inv+comp5_rate_percent_diff+comp6_rate+comp6_inv+comp6_rate_percent_diff+comp7_rate+comp7_inv+comp7_rate_percent_diff+comp8_rate+comp8_inv+comp8_rate_percent_diff+",  paste(exp_names, collapse="+"))

train_sample <- data[(data$split==1) | (data$split==3), ]

gbm.ndcg <- gbm(as.formula(gbm_formula), data=train_sample, train.fraction=0.5, n.trees=3000, interaction.depth=8, n.minobsinnode=20, shrinkage=0.005, bag.fraction=0.5, verbose=TRUE, cv.folds=0, keep.data=TRUE, n.cores=16, distribution=list(name="pairwise", metric="ndcg", max.rank=38, group="srch_id"))

best.iter.ndcg <- gbm.perf(gbm.ndcg, method='test')
title('Training of pairwise model with ndcg metric')

summary(gbm.ndcg, n.trees=best.iter.ndcg, main='pairwise (ndcg)')
save(gbm.ndcg, file="../Models/gbm.ndcg.exp.no_cross.heavy.simple.RData")

# generate prediction for test
test <- data[data$split == 4, ]
predict.test <- predict(gbm.ndcg, test, best.iter.ndcg)
my_ndcg(test$score, test$srch_id, -predict.test, 38)

if (num_submission > 0) {
submission <- data[data$split == 5, ]
predict.submission <- predict(gbm.ndcg, submission, best.iter.ndcg)
submission_p <- data.frame("srch_id" = submission$srch_id, "prop_id" = submission$prop_id, "pred" = predict.submission)
submission_p <- submission_p[with(submission_p, order(srch_id, -pred)), ]
#submission_p <- subset(submission_p, select = -c(pred))
names(submission_p) <- c("SearchId","PropertyId","Prediction")
write.table(submission_p, "../Submissions/submssision_gbm.ndcg.exp.no_cross.heavy.simple.csv", sep = ",", row.names=FALSE, quote=FALSE)
}