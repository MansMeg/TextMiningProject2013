library("topicmodels")
data("AssociatedPress", package = "topicmodels")
lda <- LDA(AssociatedPress[1:20,], k = 2, method = "Gibbs", control =
             list(best = FALSE, thin = 10, iter = 30, burnin = 0))
lda

str(lda@fitted[[1]])

