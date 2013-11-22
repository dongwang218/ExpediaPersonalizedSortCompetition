require(RSofia)
require(gbm)

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

sample_svm <- read.svmlight('../data/split_train_sample.svm')
sample_csv <- read.csv('../data/split_train_sample.csv', header=TRUE)
gbm1 <- gbm.fit(sample_svm$data, sample_svm$labels, distribution=list(name="pairwise", metric="ndcg", max.rank=38), group=sample_csv$srch_id)

pred <- predict(gbm1, sample_svm$data, type="response", n.trees=100)

sample_csv$rel <- pmin(5, (sample_csv$click_bool + sample_csv$booking_bool*5))
my_ndcg(sample_csv$rel, sample_csv$srch_id, -pred, 38)

# real test
sample2_svm <- read.svmlight('../data/split_train_sample2.svm')
sample2_csv <- read.csv('../data/split_train_sample2.csv', header=TRUE)
pred2 <- predict(gbm1, sample2_svm$data, type="response", n.trees=100)
sample2_csv$rel <- pmin(5, (sample2_csv$click_bool + sample2_csv$booking_bool*5))
my_ndcg(sample2_csv$rel, sample2_csv$srch_id, -pred2, 38)

# larger
sample_svm <- read.svmlight('../data/split_train.svm')
sample_csv <- read.csv('../data/split_train_1m.csv', header=TRUE)
gbm2 <- gbm.fit(sample_svm$data, sample_svm$labels, distribution=list(name="pairwise", metric="ndcg", max.rank=38), group=sample_csv$srch_id)

sample2_svm <- read.svmlight('../data/split_test.svm')
sample2_csv <- read.csv('../data/split_test.csv', header=TRUE)
pred2 <- predict(gbm2, sample2_svm$data, type="response", n.trees=100)
sample2_csv$rel <- pmin(5, (sample2_csv$click_bool + sample2_csv$booking_bool*5))
my_ndcg(sample2_csv$rel, sample2_csv$srch_id, -pred2, 38)

# offline check on ndcg
write.csv(pred2, '../data/split_test-gbm-r-pred.csv')
# avg ndcg 0.234379, num srch 79891