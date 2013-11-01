source('gbm_utils.R')

offset <- 1000000
num_train <- num_validate <- 1000000
num_test <- 1000000
num_stats <- 4000000
num_submission <- 6622629 #min(0, 6622629)
# 10 to 40
prior_cnt <- max(10, min(60, as.integer((num_train+num_stats) / 2000000 * 60)))

read_train <- min(9917530, offset+num_train+num_validate+num_test+num_stats)
read_test <- max(1, num_submission)

train <- read.csv('../data/train.csv', na.string = "NULL", nrows=read_train)
test <- read.csv('../data/test.csv', na.string = "NULL", nrows=read_test)

# compute expected counts and discretize for contiguous
train$score <- pmin(5.0, train$booking_bool * 5 + train$click_bool)
prior <- mean(train$score)
test$score <- -1

train <- subset(train, select = -c(position, click_bool, gross_bookings_usd, booking_bool))
data<-rbind(train, test)

data <- split_data(data, offset, num_train, num_validate, num_stats, num_test, num_submission)
dim(data)

data$date_time_dist <- as.numeric(strftime(as.Date(data$date_time) + data$srch_booking_window, format = "%j"))
data$date_time_exp <- my_exp1(data, "date_time_dist", "score", data$split < 3, prior_cnt, prior)
 
data$site_id_exp <- my_exp1(data, "site_id", "score", data$split < 3, prior_cnt, prior)

# 230
data$visitor_location_country_id_exp <- my_exp1(data, "visitor_location_country_id", "score", data$split < 3, prior_cnt, prior)

# 323, 1 to 5
data$visitor_hist_starrating_dist <- my_dist(data, "visitor_hist_starrating", method="distance")
data$visitor_hist_starrating_exp <- my_exp1(data, "visitor_hist_starrating_dist", "score", data$split < 3, prior_cnt, prior)

# max price 2768.93 
data$visitor_hist_adr_usd_dist <- my_dist(data, "visitor_hist_adr_usd")
data$visitor_hist_adr_usd_exp <- my_exp1(data, "visitor_hist_adr_usd_dist", "score", data$split < 3, prior_cnt, prior)

# 230
data$prop_country_id_exp <- my_exp1(data, "prop_country_id", "score", data$split < 3, prior_cnt, prior)

# too many
data$prop_id_exp <- my_exp1(data, "prop_id", "score", data$split < 3, prior_cnt, prior)

# "0" "1" "2" "3" "4" "5"
data$prop_starrating_exp <- my_exp1(data, "prop_starrating", "score", data$split < 3, prior_cnt, prior)
# "0"   "1"   "1.5" "2"   "2.5" "3"   "3.5" "4"   "4.5" "5"
data$prop_review_score_exp <- my_exp1(data, "prop_review_score", "score", data$split < 3, prior_cnt, prior)
# keep brand_bool

#<400 score, 0 to 7
data$prop_location_score1_dist <- my_dist(data, "prop_location_score1")
data$prop_location_score1_exp <- my_exp1(data, "prop_location_score1_dist", "score", data$split < 3, prior_cnt, prior)
# many 0 to 1
data$prop_location_score2_dist <- my_dist(data, "prop_location_score2")
data$prop_location_score2_exp <- my_exp1(data, "prop_location_score2_dist", "score", data$split < 3, prior_cnt, prior)

# 390 0 to 6.21
data$prop_log_historical_price_dist <- my_dist(data, "prop_log_historical_price", method="distance")
data$prop_log_historical_price_exp <- my_exp1(data, "prop_log_historical_price_dist", "score", data$split < 3, prior_cnt, prior)

data$price_usd_dist <- my_dist(data, "price_usd")
data$price_usd_exp <- my_exp1(data, "price_usd_dist", "score", data$split < 3, prior_cnt, prior)

# keep promotion_flag, cross with brand

# many
data$srch_destination_id_exp <- my_exp1(data, "srch_destination_id", "score", data$split < 3, prior_cnt, prior)

# 1 to 59
data$srch_length_of_stay_exp <- my_exp1(data, "srch_length_of_stay", "score", data$split < 3, prior_cnt, prior)
# 0 to 498
data$srch_booking_window_dist <- my_dist(data, "srch_booking_window")
data$srch_booking_window_exp <- my_exp1(data, "srch_booking_window_dist", "score", data$split < 3, prior_cnt, prior)

# cross this with prop_id
data$srch_adults_count_exp <- my_exp1(data, "srch_adults_count", "score", data$split < 3, prior_cnt, prior)
data$srch_children_count_exp <- my_exp1(data, "srch_children_count", "score", data$split < 3, prior_cnt, prior)
data$srch_room_count_exp <- my_exp1(data, "srch_room_count", "score", data$split < 3, prior_cnt, prior)

# keep srch_saturday_night_bool

# tiny probability
data$srch_query_affinity_score_exp <- 10 ** data$srch_query_affinity_score

data$orig_destination_distance_dist <- my_dist(data, "orig_destination_distance")
data$orig_destination_distance_exp <- my_exp1(data, "orig_destination_distance_dist", "score", data$split < 3, prior_cnt, prior)

data$comp1_rate_exp <- my_exp1(data, "comp1_rate", "score", data$split < 3, prior_cnt, prior)
data$comp1_inv_exp <- my_exp1(data, "comp1_inv", "score", data$split < 3, prior_cnt, prior)
data$comp1_rate_percent_diff_dist <- my_dist(data, "comp1_rate_percent_diff")
data$comp1_rate_percent_diff_exp <- my_exp1(data, "comp1_rate_percent_diff_dist", "score", data$split < 3, prior_cnt, prior)

data$comp2_rate_exp <- my_exp1(data, "comp2_rate", "score", data$split < 3, prior_cnt, prior)
data$comp2_inv_exp <- my_exp1(data, "comp2_inv", "score", data$split < 3, prior_cnt, prior)
data$comp2_rate_percent_diff_dist <- my_dist(data, "comp2_rate_percent_diff")
data$comp2_rate_percent_diff_exp <- my_exp1(data, "comp2_rate_percent_diff_dist", "score", data$split < 3, prior_cnt, prior)

data$comp3_rate_exp <- my_exp1(data, "comp3_rate", "score", data$split < 3, prior_cnt, prior)
data$comp3_inv_exp <- my_exp1(data, "comp3_inv", "score", data$split < 3, prior_cnt, prior)
data$comp3_rate_percent_diff_dist <- my_dist(data, "comp3_rate_percent_diff")
data$comp3_rate_percent_diff_exp <- my_exp1(data, "comp3_rate_percent_diff_dist", "score", data$split < 3, prior_cnt, prior)

data$comp4_rate_exp <- my_exp1(data, "comp4_rate", "score", data$split < 3, prior_cnt, prior)
data$comp4_inv_exp <- my_exp1(data, "comp4_inv", "score", data$split < 3, prior_cnt, prior)
data$comp4_rate_percent_diff_dist <- my_dist(data, "comp4_rate_percent_diff")
data$comp4_rate_percent_diff_exp <- my_exp1(data, "comp4_rate_percent_diff_dist", "score", data$split < 3, prior_cnt, prior)

data$comp5_rate_exp <- my_exp1(data, "comp5_rate", "score", data$split < 3, prior_cnt, prior)
data$comp5_inv_exp <- my_exp1(data, "comp5_inv", "score", data$split < 3, prior_cnt, prior)
data$comp5_rate_percent_diff_dist <- my_dist(data, "comp5_rate_percent_diff")
data$comp5_rate_percent_diff_exp <- my_exp1(data, "comp5_rate_percent_diff_dist", "score", data$split < 3, prior_cnt, prior)

data$comp6_rate_exp <- my_exp1(data, "comp6_rate", "score", data$split < 3, prior_cnt, prior)
data$comp6_inv_exp <- my_exp1(data, "comp6_inv", "score", data$split < 3, prior_cnt, prior)
data$comp6_rate_percent_diff_dist <- my_dist(data, "comp6_rate_percent_diff")
data$comp6_rate_percent_diff_exp <- my_exp1(data, "comp6_rate_percent_diff_dist", "score", data$split < 3, prior_cnt, prior)

data$comp7_rate_exp <- my_exp1(data, "comp7_rate", "score", data$split < 3, prior_cnt, prior)
data$comp7_inv_exp <- my_exp1(data, "comp7_inv", "score", data$split < 3, prior_cnt, prior)
data$comp7_rate_percent_diff_dist <- my_dist(data, "comp7_rate_percent_diff")
data$comp7_rate_percent_diff_exp <- my_exp1(data, "comp7_rate_percent_diff_dist", "score", data$split < 3, prior_cnt, prior)

data$comp8_rate_exp <- my_exp1(data, "comp8_rate", "score", data$split < 3, prior_cnt, prior)
data$comp8_inv_exp <- my_exp1(data, "comp8_inv", "score", data$split < 3, prior_cnt, prior)
data$comp8_rate_percent_diff_dist <- my_dist(data, "comp8_rate_percent_diff")
data$comp8_rate_percent_diff_exp <- my_exp1(data, "comp8_rate_percent_diff_dist", "score", data$split < 3, prior_cnt, prior)

# total num of rate, inv
data$num_comp_rate <- ifelse(is.na(data$comp1_rate), 0, data$comp1_rate) + 
                      ifelse(is.na(data$comp2_rate), 0, data$comp2_rate) + 
                      ifelse(is.na(data$comp3_rate), 0, data$comp3_rate) + 
                      ifelse(is.na(data$comp4_rate), 0, data$comp4_rate) + 
                      ifelse(is.na(data$comp5_rate), 0, data$comp5_rate) + 
                      ifelse(is.na(data$comp6_rate), 0, data$comp6_rate) + 
                      ifelse(is.na(data$comp7_rate), 0, data$comp7_rate) + 
                      ifelse(is.na(data$comp8_rate), 0, data$comp8_rate)
data$num_comp_inv <- ifelse(is.na(data$comp1_inv), 0, data$comp1_inv) + 
                      ifelse(is.na(data$comp2_inv), 0, data$comp2_inv) + 
                      ifelse(is.na(data$comp3_inv), 0, data$comp3_inv) + 
                      ifelse(is.na(data$comp4_inv), 0, data$comp4_inv) + 
                      ifelse(is.na(data$comp5_inv), 0, data$comp5_inv) + 
                      ifelse(is.na(data$comp6_inv), 0, data$comp6_inv) + 
                      ifelse(is.na(data$comp7_inv), 0, data$comp7_inv) + 
                      ifelse(is.na(data$comp8_inv), 0, data$comp8_inv)
data$num_comp_rate <- ifelse(data$num_comp_rate == 0, NA, data$num_comp_rate)
data$num_comp_inv <- ifelse(data$num_comp_inv == 0, NA, data$num_comp_inv)
data$num_comp_rate_exp <- my_exp1(data, "num_comp_rate", "score", data$score >= 0, 10, prior)
data$num_comp_inv_exp <- my_exp1(data, "num_comp_inv", "score", data$score >= 0, 10, prior)

# cross features
if (TRUE) {
data$prop_id_date_time_exp <- my_exp2(data, "date_time_dist", "prop_id", "score", data$split < 3, prior_cnt, prior)
data$prop_id_srch_children_count_exp <- my_exp2(data, "prop_id", "srch_children_count", "score", data$split < 3, prior_cnt, prior)
data$prop_id_vistor_location_country_id_exp <- my_exp2(data, "prop_id", "visitor_location_country_id", "score", data$split < 3, prior_cnt, prior)
data$prop_id_visitor_hist_starrating_exp <- my_exp2(data, "prop_id", "visitor_hist_starrating_dist", "score", data$split < 3, prior_cnt, prior)
data$prop_id_visitor_hist_adr_usd_exp <- my_exp2(data, "prop_id", "visitor_hist_adr_usd_dist", "score", data$split < 3, prior_cnt, prior)
data$prop_id_srch_length_of_stay_exp <- my_exp2(data, "prop_id", "srch_length_of_stay", "score", data$split < 3, prior_cnt, prior)
data$prop_id_promotion_flag_exp <- my_exp2(data, "prop_id", "promotion_flag", "score", data$split < 3, prior_cnt, prior)
data$prop_id_srch_destination_id_exp <- my_exp2(data, "prop_id", "srch_destination_id", "score", data$split < 3, prior_cnt, prior)

data$visitor_hist_starrating_promotion_flag_exp <- my_exp2(data, "visitor_hist_starrating_dist", "promotion_flag", "score", data$split < 3, prior_cnt, prior)

# compute avg of comp rate, inv, percent_diff

data$prop_starrating_visitor_hist_starrating_exp <- my_exp2(data, "prop_starrating", "visitor_hist_starrating_dist", "score", data$split < 3, prior_cnt, prior)

data$prop_log_historical_price_visitor_hist_adr_usd_exp <- my_exp2(data, "prop_log_historical_price_dist", "visitor_hist_adr_usd_dist", "score", data$split < 3, prior_cnt, prior)
data$price_usd_visitor_hist_adr_usd_exp <- my_exp2(data, "price_usd_dist", "visitor_hist_adr_usd_dist", "score", data$split < 3, prior_cnt, prior)
data$num_comp_rate_visitor_hist_adr_usd_exp <- my_exp2(data, "num_comp_rate", "visitor_hist_adr_usd_dist", "score", data$split < 3, prior_cnt, prior)

data$prop_review_score_prop_location_score1_exp <- my_exp2(data, "prop_review_score", "prop_location_score1_dist", "score", data$split < 3, prior_cnt, prior)
}
save(data, file = "../data/train_test_disc.RData")

#exp_names = names(data)[grep(".*(prop_brand_bool|srch_saturday_night_bool|_exp)$", names(data))]
exp_names = names(data)[grep(".*_exp$", names(data))]
gbm_formula <- paste("score ~ date_time_dist+site_id+visitor_location_country_id+visitor_hist_starrating+visitor_hist_adr_usd+prop_country_id+prop_id+prop_starrating+prop_review_score+prop_brand_bool+prop_location_score1+prop_location_score2+prop_log_historical_price+price_usd+promotion_flag+srch_destination_id+srch_length_of_stay+srch_booking_window+srch_adults_count+srch_children_count+srch_room_count+srch_saturday_night_bool+srch_query_affinity_score+orig_destination_distance+random_bool+comp1_rate+comp1_inv+comp1_rate_percent_diff+comp2_rate+comp2_inv+comp2_rate_percent_diff+comp3_rate+comp3_inv+comp3_rate_percent_diff+comp4_rate+comp4_inv+comp4_rate_percent_diff+comp5_rate+comp5_inv+comp5_rate_percent_diff+comp6_rate+comp6_inv+comp6_rate_percent_diff+comp7_rate+comp7_inv+comp7_rate_percent_diff+comp8_rate+comp8_inv+comp8_rate_percent_diff+",  paste(exp_names, collapse="+"))

train_sample <- data[(data$split==1) | (data$split==3), ]

gbm.ndcg <- gbm(as.formula(gbm_formula), data=train_sample, train.fraction=0.5, n.trees=3000, interaction.depth=8, n.minobsinnode=20, shrinkage=0.005, bag.fraction=0.5, verbose=TRUE, cv.folds=0, keep.data=TRUE, n.cores=16, distribution=list(name="pairwise", metric="ndcg", max.rank=38, group="srch_id"))

best.iter.ndcg <- gbm.perf(gbm.ndcg, method='test')
title('Training of pairwise model with ndcg metric')

summary(gbm.ndcg, n.trees=best.iter.ndcg, main='pairwise (ndcg)')
save(gbm.ndcg, file="../Models/gbm.ndcg.exp.no_cross.RData")

# generate prediction for test
test <- data[data$split == 4, ]
predict.test <- predict(gbm.ndcg, test, best.iter.ndcg)
my_ndcg(test$score, test$srch_id, -predict.test, 38)

if (num_submission > 0) {
submission <- data[data$split == 5, ]
predict.submission <- predict(gbm.ndcg, submission, best.iter.ndcg)
submission_p <- data.frame("srch_id" = submission$srch_id, "prop_id" = submission$prop_id, "pred" = predict.submission)
submission_p <- submission_p[with(submission_p, order(srch_id, -pred)), ]
submission_p <- subset(submission_p, select = -c(pred))
names(submission_p) <- c("SearchId","PropertyId")
write.table(submission_p, "../Submissions/submssision_gbm.ndcg.exp.no_cross.csv", sep = ",", row.names=FALSE, quote=FALSE)
}