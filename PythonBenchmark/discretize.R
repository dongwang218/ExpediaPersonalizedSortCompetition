my_exp1<-function(data, col, y, prior_cnt, prior){
  d2<-data[, c(col, y)]
  names(d2)<-c("f", "a")
  sum1<-sqldf("select f, sum(1) as cnt, sum(a) as suma from d2 group by 1")
  tmp1<-sqldf("select b.cnt, b.suma from d2 a left join sum1 b on a.f=b.f")
  tmp1$exp<-with(tmp1, (suma + prior * prior_cnt)/(cnt+prior_cnt))
  tmp1$exp[is.na(d2$f)] <- NA
  return(tmp1$exp)
}

# we could use the info package to discretize mdl
my_dist<-function(data, col, k=100, method="quantile") {
  dc <- data[, col]
  if (method == "quantile") {
    # equal frequency
    dq <- quantile(dc, na.rm=TRUE, probs=seq(0, 1, length=k))
  } else {
    # equal distance
    m1 <- min(dc, na.rm=TRUE)
    m2 <- max(dc, na.rm=TRUE)
    dq <- seq(m1, m2, length = k)
  }

  dq <- unique(dq)
  dq <- c(-Inf, dq, Inf)
  # replace the last with Inf and the first with -Inf
  dist <- cut(dc, breaks = dq, labels=seq(1, length(dq)-1))
  return (dist)
}


train <- read.csv('../data/train.csv', na.string = "NULL") #, nrows=10000)

# compute expected counts and discretize for contiguous
train$score <- pmin(5.0, train$booking_bool * 5 + train$click_bool)
prior <- mean(train$score)

train$date_time_dist <- as.numeric(strftime(as.Date(train$date_time) + train$srch_booking_window, format = "%j"))
train$date_time_exp <- my_exp1(train, "date_time_dist", 10, prior)
 
train$site_id_exp <- my_exp1(train, "site_id", "score", 10, prior)

# 230
train$visitor_location_country_id_exp <- my_exp1(train, "visitor_location_country_id", "score", 10, prior)

# 323, 1 to 5
train$visitor_hist_starrating_dist <- my_dist(train, "visitor_hist_starrating", method="distance")
train$visitor_hist_starrating_exp <- my_exp1(train, "visitor_hist_starrating_dist", "score", 10, prior)

# max price 2768.93 
train$visitor_hist_adr_usd_dist <- my_dist(train, "visitor_hist_adr_usd")
train$visitor_hist_adr_usd_exp <- my_exp1(train, "visitor_hist_adr_usd_dist", "score", 10, prior)

# 230
train$prop_country_id_exp <- my_exp1(train, "prop_country_id", "score", 10, prior)

# too many
train$prop_id_exp <- my_exp1(train, "prop_id", "score", 10, prior)

# "0" "1" "2" "3" "4" "5"
train$prop_starrating_exp <- my_exp1(train, "prop_starrating", "score", 10, prior)
# "0"   "1"   "1.5" "2"   "2.5" "3"   "3.5" "4"   "4.5" "5"
train$prop_review_score_exp <- my_exp1(train, "prop_review_score", "score", 10, prior)
# keep brand_bool

#<400 score, 0 to 7
train$prop_location_score1_dist <- my_dist(train, "prop_location_score1")
train$prop_location_score1_exp <- my_exp1(train, "prop_location_score1_dist", "score", 10, prior)
# many 0 to 1
train$prop_location_score2_dist <- my_dist(train, "prop_location_score2")
train$prop_location_score2_exp <- my_exp1(train, "prop_location_score2_dist", "score", 10, prior)

# 390 0 to 6.21
train$prop_log_historical_price_dist <- my_dist(train, "prop_log_historical_price", method="distance")
train$prop_log_historical_price_exp <- my_exp1(train, "prop_log_historical_price_dist", "score", 10, prior)

train$price_usd_dist <- my_dist(train, "price_usd")
train$price_usd_exp <- my_exp1(train, "price_usd_dist", "score", 10, prior)

# keep promotion_flag, cross with brand

# many
train$srch_destination_id_exp <- my_exp1(train, "srch_destination_id", "score", 10, prior)

# 1 to 59
train$srch_length_of_stay_exp <- my_exp1(train, "srch_length_of_stay", "score", 10, prior)
# 0 to 498
train$srch_booking_window_dist <- my_dist(train, "srch_booking_window")
train$srch_booking_window_exp <- my_exp1(train, "srch_booking_window_dist", "score", 10, prior)

# cross this with prop_id
train$srch_adults_count_exp <- my_exp1(train, "srch_adults_count", "score", 10, prior)
train$srch_children_count_exp <- my_exp1(train, "srch_children_count", "score", 10, prior)
train$srch_room_count_exp <- my_exp1(train, "srch_room_count", "score", 10, prior)

# keep srch_saturday_night_bool

# tiny probability
train$srch_query_affinity_score_exp <- 10 ** train$srch_query_affinity_score

train$orig_destination_distance_dist <- my_dist(train, "orig_destination_distance")
train$orig_destination_distance_exp <- my_exp1(train, "orig_destination_distance_dist", "score", 10, prior)

train$comp1_rate_exp <- my_exp1(train, "comp1_rate", "score", 10, prior)
train$comp1_inv_exp <- my_exp1(train, "comp1_inv", "score", 10, prior)
train$comp1_rate_percent_diff_dist <- my_dist(train, "comp1_rate_percent_diff")
train$comp1_rate_percent_diff_exp <- my_exp1(train, "comp1_rate_percent_diff_dist", "score", 10, prior)

train$comp2_rate_exp <- my_exp1(train, "comp2_rate", "score", 10, prior)
train$comp2_inv_exp <- my_exp1(train, "comp2_inv", "score", 10, prior)
train$comp2_rate_percent_diff_dist <- my_dist(train, "comp2_rate_percent_diff")
train$comp2_rate_percent_diff_exp <- my_exp1(train, "comp2_rate_percent_diff_dist", "score", 10, prior)

train$comp3_rate_exp <- my_exp1(train, "comp3_rate", "score", 10, prior)
train$comp3_inv_exp <- my_exp1(train, "comp3_inv", "score", 10, prior)
train$comp3_rate_percent_diff_dist <- my_dist(train, "comp3_rate_percent_diff")
train$comp3_rate_percent_diff_exp <- my_exp1(train, "comp3_rate_percent_diff_dist", "score", 10, prior)

train$comp4_rate_exp <- my_exp1(train, "comp4_rate", "score", 10, prior)
train$comp4_inv_exp <- my_exp1(train, "comp4_inv", "score", 10, prior)
train$comp4_rate_percent_diff_dist <- my_dist(train, "comp4_rate_percent_diff")
train$comp4_rate_percent_diff_exp <- my_exp1(train, "comp4_rate_percent_diff_dist", "score", 10, prior)

train$comp5_rate_exp <- my_exp1(train, "comp5_rate", "score", 10, prior)
train$comp5_inv_exp <- my_exp1(train, "comp5_inv", "score", 10, prior)
train$comp5_rate_percent_diff_dist <- my_dist(train, "comp5_rate_percent_diff")
train$comp5_rate_percent_diff_exp <- my_exp1(train, "comp5_rate_percent_diff_dist", "score", 10, prior)

train$comp6_rate_exp <- my_exp1(train, "comp6_rate", "score", 10, prior)
train$comp6_inv_exp <- my_exp1(train, "comp6_inv", "score", 10, prior)
train$comp6_rate_percent_diff_dist <- my_dist(train, "comp6_rate_percent_diff")
train$comp6_rate_percent_diff_exp <- my_exp1(train, "comp6_rate_percent_diff_dist", "score", 10, prior)

train$comp7_rate_exp <- my_exp1(train, "comp7_rate", "score", 10, prior)
train$comp7_inv_exp <- my_exp1(train, "comp7_inv", "score", 10, prior)
train$comp7_rate_percent_diff_dist <- my_dist(train, "comp7_rate_percent_diff")
train$comp7_rate_percent_diff_exp <- my_exp1(train, "comp7_rate_percent_diff_dist", "score", 10, prior)

train$comp8_rate_exp <- my_exp1(train, "comp8_rate", "score", 10, prior)
train$comp8_inv_exp <- my_exp1(train, "comp8_inv", "score", 10, prior)
train$comp8_rate_percent_diff_dist <- my_dist(train, "comp8_rate_percent_diff")
train$comp8_rate_percent_diff_exp <- my_exp1(train, "comp8_rate_percent_diff_dist", "score", 10, prior)

save(train, file = "../data/train_disc.RData")
write.csv(train, "../data/train_disc.csv")

exp_names = names(train)[grep(".*(prop_brand_bool|srch_saturday_night_bool|_exp)$", names(train))]
gbm_formula <- paste("score ~", paste(exp_names, collapse="+"))

train_sample <- train[1:200000, ]
gbm.ndcg <- gbm(as.formula(gbm_formula), data=train_sample, train.fraction=0.5, n.trees=5000, interaction.depth=8, n.minobsinnode=20, shrinkage=0.005, bag.fraction=0.5, verbose=TRUE, cv.folds=0, keep.data=TRUE, n.cores=16, distribution=list(name="pairwise", metric="ndcg", max.rank=38, group="srch_id"))

best.iter.ndcg <- gbm.perf(gbm.ndcg, method='test')
title('Training of pairwise model with ndcg metric')