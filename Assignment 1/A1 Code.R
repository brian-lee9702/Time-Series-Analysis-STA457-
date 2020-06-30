##

x <- 1:10
y <- 0:10
y[1]=20
for (val in 1:10) {
  x[val] <- rnorm(1, mean=0.03, sd=0.005)
}
tot <- sum(x)
for (ct in 1:10) {
  list_ctr = 1:ct
  value = 20*exp(sum(x[list_ctr]))
  y[ct+1] = value
}
print(y)
x_list = 0:10
plot(x_list, y, type = 'o', main="Simulated Price of Asset 
	for the Next 10 Years", xlab="years in the future", ylab="asset price")

##
values = 1:2000
for (ct in 1:2000) {
  samp = rnorm(10, mean=0.03, sd=0.005)
  tot = sum(samp)
  values[ct] = 20*exp(tot)
}
print(mean(values))