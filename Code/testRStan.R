schools_code <- '
  data {
    int<lower=0> J; // number of schools 
real y[J]; // estimated treatment effects
real<lower=0> sigma[J]; // s.e. of effect estimates 
}
parameters {
real mu; 
real<lower=0> tau;
real eta[J];
}
transformed parameters {
real theta[J];
for (j in 1:J)
theta[j] <- mu + tau * eta[j];
}
model {
eta ~ normal(0, 1);
y ~ normal(theta, sigma);
}
'

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(model_code = schools_code, data = schools_dat, 
            iter = 1000, chains = 4)

fit2 <- stan(fit = fit, data = schools_dat, iter = 10000, chains = 4)

plot(fit2)
print(fit2)

la <- extract(fit2, permuted = TRUE) # return a list of arrays 
str(la)
mu <- la$mu 
plot(mu,type="l")
a <- extract(fit2, permuted = FALSE) 
str(a)
