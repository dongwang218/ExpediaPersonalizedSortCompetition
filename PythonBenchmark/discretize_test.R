options(java.parameters="-Xmx10g")

options(gsubfn.engine = "R")
require(irlba)
require(sqldf)
require(gbm)
require(randomForest)
require(extraTrees)
require(glmnet)

my_exp1<-function(data, col, y, filter, prior_cnt, prior){
  d2<-data[, c(col, y)]
  names(d2)<-c("f", "a")
  d2$filter <- filter
  sum1<-sqldf("select f, sum(1) as cnt, sum(a) as suma from d2 where filter=1 group by 1")
  tmp1<-sqldf("select b.cnt, b.suma from d2 a left join sum1 b on a.f=b.f")
  tmp1$exp<-with(tmp1, (suma + prior * prior_cnt)/(cnt+prior_cnt))
  tmp1$exp[is.na(d2$f)] <- NA
  return(tmp1$exp)
}

my_exp2<-function(data, col1, col2, y, filter, prior_cnt, prior){
  d2<-data[, c(col1, col2, y)]
  names(d2)<-c("f1", "f2", "a")
  d2$filter <- filter
  sum1<-sqldf("select f1, f2, sum(1) as cnt, sum(a) as suma from d2 where filter=1 group by 1,2")
  tmp1<-sqldf("select b.cnt, b.suma from d2 a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  tmp1$exp<-with(tmp1, (suma + prior * prior_cnt)/(cnt+prior_cnt))
  tmp1$exp[is.na(d2$f1) | is.na(d2$f2)] <- NA
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

my_ndcg<-function(rel, idx, orderby, k=1000){
  d<-data.frame(rel=rel, idx=idx, one=1, o=orderby)
  d<-d[order(d$idx, -d$rel),]
  d$co<-with(d, unlist(tapply(one, idx, cumsum)))
  d$v<-with(d, (2^rel-1)/(log(co+1, 2)))
  #print(head(d, 20))
  dcg0<-with(d[d$co<=k,], unlist(tapply(v, idx, sum)))
  d<-d[order(d$idx, d$o, d$rel),] # get the worse score when there
  d$co<-with(d, unlist(tapply(one, idx, cumsum)))
  d$v<-with(d, (2^rel-1)/(log(co+1, 2)))
  #print(head(d, 20))
  #dcg<-sum(d$v[d$co<=k])
  dcg<-with(d[d$co<=k,], unlist(tapply(v, idx, sum)))
  return(mean(dcg[dcg0 > 0]/dcg0[dcg0 > 0]))
}

train <- read.csv('../data/train.csv', na.string = "NULL", nrows=200000)
test <- read.csv('../data/test.csv', na.string = "NULL", nrows=100000)

# compute expected counts and discretize for contiguous
train$score <- pmin(5.0, train$booking_bool * 5 + train$click_bool)
prior <- mean(train$score)
test$score <- -1

train <- subset(train, select = -c(position, click_bool, gross_bookings_usd, booking_bool))
data<-rbind(train, test)

data$date_time_dist <- as.numeric(strftime(as.Date(data$date_time) + data$srch_booking_window, format = "%j"))
data$date_time_exp <- my_exp1(data, "date_time_dist", "score", data$score >= 0, 10, prior)
 
data$site_id_exp <- my_exp1(data, "site_id", "score", data$score >= 0, 10, prior)

# 230
data$visitor_location_country_id_exp <- my_exp1(data, "visitor_location_country_id", "score", data$score >= 0, 10, prior)

# 323, 1 to 5
data$visitor_hist_starrating_dist <- my_dist(data, "visitor_hist_starrating", method="distance")
data$visitor_hist_starrating_exp <- my_exp1(data, "visitor_hist_starrating_dist", "score", data$score >= 0, 10, prior)

# max price 2768.93 
data$visitor_hist_adr_usd_dist <- my_dist(data, "visitor_hist_adr_usd")
data$visitor_hist_adr_usd_exp <- my_exp1(data, "visitor_hist_adr_usd_dist", "score", data$score >= 0, 10, prior)

# 230
data$prop_country_id_exp <- my_exp1(data, "prop_country_id", "score", data$score >= 0, 10, prior)

# too many
data$prop_id_exp <- my_exp1(data, "prop_id", "score", data$score >= 0, 10, prior)

# "0" "1" "2" "3" "4" "5"
data$prop_starrating_exp <- my_exp1(data, "prop_starrating", "score", data$score >= 0, 10, prior)
# "0"   "1"   "1.5" "2"   "2.5" "3"   "3.5" "4"   "4.5" "5"
data$prop_review_score_exp <- my_exp1(data, "prop_review_score", "score", data$score >= 0, 10, prior)
# keep brand_bool

#<400 score, 0 to 7
data$prop_location_score1_dist <- my_dist(data, "prop_location_score1")
data$prop_location_score1_exp <- my_exp1(data, "prop_location_score1_dist", "score", data$score >= 0, 10, prior)
# many 0 to 1
data$prop_location_score2_dist <- my_dist(data, "prop_location_score2")
data$prop_location_score2_exp <- my_exp1(data, "prop_location_score2_dist", "score", data$score >= 0, 10, prior)

# 390 0 to 6.21
data$prop_log_historical_price_dist <- my_dist(data, "prop_log_historical_price", method="distance")
data$prop_log_historical_price_exp <- my_exp1(data, "prop_log_historical_price_dist", "score", data$score >= 0, 10, prior)

data$price_usd_dist <- my_dist(data, "price_usd")
data$price_usd_exp <- my_exp1(data, "price_usd_dist", "score", data$score >= 0, 10, prior)

# keep promotion_flag, cross with brand

# many
data$srch_destination_id_exp <- my_exp1(data, "srch_destination_id", "score", data$score >= 0, 10, prior)

# 1 to 59
data$srch_length_of_stay_exp <- my_exp1(data, "srch_length_of_stay", "score", data$score >= 0, 10, prior)
# 0 to 498
data$srch_booking_window_dist <- my_dist(data, "srch_booking_window")
data$srch_booking_window_exp <- my_exp1(data, "srch_booking_window_dist", "score", data$score >= 0, 10, prior)

# cross this with prop_id
data$srch_adults_count_exp <- my_exp1(data, "srch_adults_count", "score", data$score >= 0, 10, prior)
data$srch_children_count_exp <- my_exp1(data, "srch_children_count", "score", data$score >= 0, 10, prior)
data$srch_room_count_exp <- my_exp1(data, "srch_room_count", "score", data$score >= 0, 10, prior)

# keep srch_saturday_night_bool

# tiny probability
data$srch_query_affinity_score_exp <- 10 ** data$srch_query_affinity_score

data$orig_destination_distance_dist <- my_dist(data, "orig_destination_distance")
data$orig_destination_distance_exp <- my_exp1(data, "orig_destination_distance_dist", "score", data$score >= 0, 10, prior)

data$comp1_rate_exp <- my_exp1(data, "comp1_rate", "score", data$score >= 0, 10, prior)
data$comp1_inv_exp <- my_exp1(data, "comp1_inv", "score", data$score >= 0, 10, prior)
data$comp1_rate_percent_diff_dist <- my_dist(data, "comp1_rate_percent_diff")
data$comp1_rate_percent_diff_exp <- my_exp1(data, "comp1_rate_percent_diff_dist", "score", data$score >= 0, 10, prior)

data$comp2_rate_exp <- my_exp1(data, "comp2_rate", "score", data$score >= 0, 10, prior)
data$comp2_inv_exp <- my_exp1(data, "comp2_inv", "score", data$score >= 0, 10, prior)
data$comp2_rate_percent_diff_dist <- my_dist(data, "comp2_rate_percent_diff")
data$comp2_rate_percent_diff_exp <- my_exp1(data, "comp2_rate_percent_diff_dist", "score", data$score >= 0, 10, prior)

data$comp3_rate_exp <- my_exp1(data, "comp3_rate", "score", data$score >= 0, 10, prior)
data$comp3_inv_exp <- my_exp1(data, "comp3_inv", "score", data$score >= 0, 10, prior)
data$comp3_rate_percent_diff_dist <- my_dist(data, "comp3_rate_percent_diff")
data$comp3_rate_percent_diff_exp <- my_exp1(data, "comp3_rate_percent_diff_dist", "score", data$score >= 0, 10, prior)

data$comp4_rate_exp <- my_exp1(data, "comp4_rate", "score", data$score >= 0, 10, prior)
data$comp4_inv_exp <- my_exp1(data, "comp4_inv", "score", data$score >= 0, 10, prior)
data$comp4_rate_percent_diff_dist <- my_dist(data, "comp4_rate_percent_diff")
data$comp4_rate_percent_diff_exp <- my_exp1(data, "comp4_rate_percent_diff_dist", "score", data$score >= 0, 10, prior)

data$comp5_rate_exp <- my_exp1(data, "comp5_rate", "score", data$score >= 0, 10, prior)
data$comp5_inv_exp <- my_exp1(data, "comp5_inv", "score", data$score >= 0, 10, prior)
data$comp5_rate_percent_diff_dist <- my_dist(data, "comp5_rate_percent_diff")
data$comp5_rate_percent_diff_exp <- my_exp1(data, "comp5_rate_percent_diff_dist", "score", data$score >= 0, 10, prior)

data$comp6_rate_exp <- my_exp1(data, "comp6_rate", "score", data$score >= 0, 10, prior)
data$comp6_inv_exp <- my_exp1(data, "comp6_inv", "score", data$score >= 0, 10, prior)
data$comp6_rate_percent_diff_dist <- my_dist(data, "comp6_rate_percent_diff")
data$comp6_rate_percent_diff_exp <- my_exp1(data, "comp6_rate_percent_diff_dist", "score", data$score >= 0, 10, prior)

data$comp7_rate_exp <- my_exp1(data, "comp7_rate", "score", data$score >= 0, 10, prior)
data$comp7_inv_exp <- my_exp1(data, "comp7_inv", "score", data$score >= 0, 10, prior)
data$comp7_rate_percent_diff_dist <- my_dist(data, "comp7_rate_percent_diff")
data$comp7_rate_percent_diff_exp <- my_exp1(data, "comp7_rate_percent_diff_dist", "score", data$score >= 0, 10, prior)

data$comp8_rate_exp <- my_exp1(data, "comp8_rate", "score", data$score >= 0, 10, prior)
data$comp8_inv_exp <- my_exp1(data, "comp8_inv", "score", data$score >= 0, 10, prior)
data$comp8_rate_percent_diff_dist <- my_dist(data, "comp8_rate_percent_diff")
data$comp8_rate_percent_diff_exp <- my_exp1(data, "comp8_rate_percent_diff_dist", "score", data$score >= 0, 10, prior)

# cross features

data$prop_id_date_time_exp <- my_exp2(data, "date_time_dist", "prop_id", "score", data$score >= 0, 10, prior)
data$prop_id_srch_children_count_exp <- my_exp2(data, "prop_id", "srch_children_count", "score", data$score >= 0, 10, prior)
data$prop_id_vistor_location_country_id_exp <- my_exp2(data, "prop_id", "visitor_location_country_id", "score", data$score >= 0, 10, prior)
data$prop_id_visitor_hist_starrating_exp <- my_exp2(data, "prop_id", "visitor_hist_starrating_dist", "score", data$score >= 0, 10, prior)
data$prop_id_visitor_hist_adr_usd_exp <- my_exp2(data, "prop_id", "visitor_hist_adr_usd_dist", "score", data$score >= 0, 10, prior)
data$prop_id_srch_length_of_stay_exp <- my_exp2(data, "prop_id", "srch_length_of_stay", "score", data$score >= 0, 10, prior)
data$prop_id_promotion_flag_exp <- my_exp2(data, "prop_id", "promotion_flag", "score", data$score >= 0, 10, prior)
data$visitor_hist_starrating_promotion_flag_exp <- my_exp2(data, "visitor_hist_starrating_dist", "promotion_flag", "score", data$score >= 0, 10, prior)

# compute avg of comp rate, inv, percent_diff

data$prop_starrating_visitor_hist_starrating_exp <- my_exp2(data, "prop_starrating", "visitor_hist_starrating_dist", "score", data$score >= 0, 10, prior)

data$prop_log_historical_price_visitor_hist_adr_usd_exp <- my_exp2(data, "prop_log_historical_price_dist", "visitor_hist_adr_usd_dist", "score", data$score >= 0, 10, prior)
data$price_usd_visitor_hist_adr_usd_exp <- my_exp2(data, "price_usd_dist", "visitor_hist_adr_usd_dist", "score", data$score >= 0, 10, prior)

data$prop_review_score_prop_location_score1_exp <- my_exp2(data, "prop_review_score", "prop_location_score1_dist", "score", data$score >= 0, 10, prior)

#save(data, file = "../data/train_test_disc_cross.RData")
#write.csv(data, "../data/train_test_disc.csv")

exp_names = names(data)[grep(".*(prop_brand_bool|srch_saturday_night_bool|_exp)$", names(data))]
gbm_formula <- paste("score ~", paste(exp_names, collapse="+"))

real_train <- data[data$score >= 0, ]
train_sample <- real_train[1:min(dim(real_train)[1], 200000), ]
gbm.ndcg <- gbm(as.formula(gbm_formula), data=train_sample, train.fraction=0.5, n.trees=10000, interaction.depth=8, n.minobsinnode=20, shrinkage=0.005, bag.fraction=0.5, verbose=TRUE, cv.folds=0, keep.data=TRUE, n.cores=16, distribution=list(name="pairwise", metric="ndcg", max.rank=38, group="srch_id"))

best.iter.ndcg <- gbm.perf(gbm.ndcg, method='test')
title('Training of pairwise model with ndcg metric')

summary(gbm.ndcg, n.trees=best.iter.ndcg, main='pairwise (ndcg)')

# generate prediction for test


total <- dim(real_train)[1]
test <- data[total-100000:total, ]
predict.ndcg <- predict(gbm.ndcg, test, best.iter.ndcg)
my_ndcg(test$score, test$srch_id, -predict.ndcg, 38)
# 0.5111782

#submission <- data.frame("src_id" = test$srch_id, "prop_id" = test$prop_id, "pred" = predict.ndcg)
#write.csv(submission, "../Submissions/submssision_gbm.ndcg.exp.no_cross.csv")

#save(gbm.ndcg, file="../Models/gbm.ndcg.exp.no_cross.RData")
#ndcg38.loss <- gbm.loss(y=test$score, predict.ndcg, w=rep(1,dim(test)[1]), offset=NA, dist=list(name='pairwise', metric="ndcg"),                                  baseline=0, group=test$srch_id, max.rank=38)