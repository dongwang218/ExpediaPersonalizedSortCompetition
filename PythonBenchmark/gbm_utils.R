#options(java.parameters="-Xmx10g")

options(gsubfn.engine = "R")
require(irlba)
require(sqldf)
require(gbm)
#require(randomForest)
#require(extraTrees)
#require(glmnet)

#shrank and randomized leave-one-out average actual for categorical variables
my_exp1<-function(data, col, y, filter, prior_cnt, prior, r_k=1.0/5, drop_cnt=0){
  r_k <- r_k * prior
  d2<-data[, c(col, y)]
  names(d2)<-c("f", "a")
  d2$filter <- filter
  sum1<-sqldf("select f, sum(1) as cnt, sum(a) as suma from d2 where filter=1 group by 1")
  tmp1<-sqldf("select b.cnt, b.suma from d2 a left join sum1 b on a.f=b.f")
  if (drop_cnt > 0) {
    tmp1$cnt <- ifelse(tmp1$cnt < drop_cnt, NA, tmp1$cnt)
  }
  tmp1$cnt[filter] <- tmp1$cnt[filter]-1
  tmp1$suma[filter] <- tmp1$suma[filter] - d2$a[filter]

  tmp1$exp<-with(tmp1, (suma + prior * prior_cnt)/(cnt+prior_cnt))
  if (r_k > 0.0) {
    tmp1$exp[filter]<-tmp1$exp[filter]*(1+(runif(sum(filter))-0.5)*r_k)  
  }
  tmp1$exp[is.na(d2$f)] <- NA
  return(tmp1$exp)
}

my_exp2<-function(data, col1, col2, y, filter, prior_cnt, prior, r_k=1.0/5, drop_cnt=0){
  r_k <- r_k * prior
  d2<-data[, c(col1, col2, y)]
  names(d2)<-c("f1", "f2", "a")
  d2$filter <- filter
  sum1<-sqldf("select f1, f2, sum(1) as cnt, sum(a) as suma from d2 where filter=1 group by 1,2")
  tmp1<-sqldf("select b.cnt, b.suma from d2 a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  if (drop_cnt > 0) {
    tmp1$cnt <- ifelse(tmp1$cnt < drop_cnt, NA, tmp1$cnt)
  }
  tmp1$cnt[filter] <- tmp1$cnt[filter]-1
  tmp1$suma[filter] <- tmp1$suma[filter] - d2$a[filter]

  tmp1$exp<-with(tmp1, (suma + prior * prior_cnt)/(cnt+prior_cnt))
  if (r_k > 0.0) {
    tmp1$exp[filter]<-tmp1$exp[filter]*(1+(runif(sum(filter))-0.5)*r_k)  
  }
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

# split into, only train and stats are used to compute statistics
# split 1      3             2             4           5
# num_train, num_validate, num_stats, ... num_test, num_submission
# also get rid of stuff in between
split_data <- function(data, offset, num_train, num_validate, num_stats, num_test, num_submission) {
  data <- data[offset:dim(data)[1], ]
  data$split <- 0
  data[1:num_train, c("split")] <- 1
  data[(num_train+1):(num_train+num_validate), c("split")] <- 3
  if (num_stats > 0) {
     data[(num_train+num_validate+1):(num_train+num_validate+num_stats), c("split")] <- 2
  }
  total <- dim(data)[1]
  if (num_test > 0) {
    data[(total-num_test-num_submission+1):(total-num_submission), c("split")] <- 4
  }
  if (num_submission > 0) {
    data[(total-num_submission+1):total, c("split")] <- 5
  }
  # get rid of split == 0
  data <- data[data$split > 0, ]
  return(data)
}