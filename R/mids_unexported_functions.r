# # For test purposes

gimme_some_data <- function(n=1000){
   z <- factor(sample(LETTERS[1:5], n, TRUE), levels=LETTERS[1:5])
   x <- round(as.numeric(z) / (1+rbinom(n, 2, 0.5)) + runif(n, 0, 3.7), 1)
   foo <- exp(1-ifelse(z %in% LETTERS[1:2], 0, ifelse(z %in% LETTERS[c(3,5)], 2, 5)) + 0.4*x)
   y <- rbinom(n, 1, prob =  (1-foo/(1+foo))/2)
   df <- data.frame(y=y, x=x, z=z)
   df$x[sample(1:n, size = max(0.09*n, 3), FALSE)] <- NA
   df$z[sample(1:n, size = max(0.04*n, 4), FALSE)] <- NA
   df
}
