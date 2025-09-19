# Stat-699-codes-Fall-2025
# This is the code for Chatterjee's correlation coeffiecient done in R .

set.seed(104)
library(dplyr)

# Case : Without ties.
x = runif(40)  # Data generation.
y = -3*x + 1
df.x.y = data.frame(x,y)

n = 20
new.df.x.y = df.x.y %>% slice_sample(n=20, replace = FALSE)   # No ties; sampling without replacement from data frame.

order.new.df = new.df.x.y[order(new.df.x.y$x),]  # Sorting the data frame by X.

#new.df.1 = new.df.x.y %>% arrange(x)

rank.y = rank(order.new.df$y) # getting ranks of Y_(i)
new.df.x.y = bind_cols(order.new.df,as.data.frame((rank.y)))
colnames(new.df.x.y)[3] = "r.i" 

print(new.df.x.y)

m = nrow(new.df.x.y) - 1
diff = c() 
for ( i in 1:m) {
  v = abs(rank.y[i+1]-rank.y[i])
  diff = c(diff,v)
}
cat("The consecutive rank differences of y values :", diff, "\n")
#print(diff)

# Correlation coeffiecient computation.
n = nrow(new.df.x.y)
num = 3*sum(diff)
den = n**2 - 1

zi.corr = 1 - (num/den)
cat("Correlation coefficient :", zi.corr,"\n")

# Case: With ties.
x1 = runif(40)  # Data generation.
y1 = -3*x1 + 1
df.x1.y1 = data.frame(x1,y1)

n = 20
new.df.x1.y1 = df.x1.y1 %>% slice_sample(n=20, replace = TRUE)   #Ties; sampling with replacement from data frame.

order.new.df1 = new.df.x1.y1[order(new.df.x1.y1$x1),]  # Sorting the data frame by X.

#new.df.1 = new.df.x.y %>% arrange(x)

rank.y1 = rank(order.new.df1$y1) # getting ranks of Y_(i) ; # ri = number of j such that Y_(j) <= Y_(i).
new.df.x1.y1 = bind_cols(order.new.df1, as.data.frame((rank.y1)))
colnames(new.df.x1.y1)[3] = "r1.i"

print(new.df.x1.y1)

# li = number of j such that Y_(j) >= Y_(i)
li = n - rank.y1

new.df.1 = bind_cols(new.df.x1.y1, as.data.frame(li)) # Appending column of li.
print(new.df.1)

m = nrow(new.df.x1.y1) - 1
diff.1 = c() 
for ( i in 1:m) {
  v.1 = abs(rank.y1[i+1]-rank.y1[i])
  diff.1 = c(diff.1,v.1)
}
cat("The consecutive rank differences of y values :", diff.1, "\n")
#print(diff)

# Correlation coeffiecient computation.

n = nrow(new.df.x1.y1)
num.1 = n*sum(diff.1)

l = li*(n-li)
l.sum = sum(l)
den.1 = 2*l.sum

zi.corr1 = 1 - (num.1/den.1)

cat("Correlation coefficient :", zi.corr1,"\n")

# Following are some simulations for increasing sample size and different types of y.

# Y = poly(x) , sample size = 50 , with ties.

x1 = runif(100, 3, 7)
y1 = (x1^2) - 5*x1 + 6

df.x1.y1 = data.frame(x1,y1)

n = 50
new.df.x1.y1 = df.x1.y1 %>% slice_sample(n=50, replace = TRUE)   #Ties; sampling with replacement from data frame.

order.new.df1 = new.df.x1.y1[order(new.df.x1.y1$x1),]  # Sorting the data frame by X.

#new.df.1 = new.df.x.y %>% arrange(x)

rank.y1 = rank(order.new.df1$y1) # getting ranks of Y_(i) ; # ri = number of j such that Y_(j) <= Y_(i).
new.df.x1.y1 = bind_cols(order.new.df1, as.data.frame((rank.y1)))
colnames(new.df.x1.y1)[3] = "r1.i"

print(new.df.x1.y1)

# li = number of j such that Y_(j) >= Y_(i)
li = n - rank.y1

new.df.1 = bind_cols(new.df.x1.y1, as.data.frame(li)) # Appending column of li.
print(new.df.1)

m = nrow(new.df.x1.y1) - 1
diff.1 = c() 
for ( i in 1:m) {
  v.1 = abs(rank.y1[i+1]-rank.y1[i])
  diff.1 = c(diff.1,v.1)
}
cat("The consecutive rank differences of y values :", diff.1, "\n")
#print(diff)

n = nrow(new.df.x1.y1)
num.1 = n*sum(diff.1)

l = li*(n-li)
l.sum = sum(l)
den.1 = 2*l.sum

zi.corr1 = 1 - (num.1/den.1)

cat("Correlation coefficient :", zi.corr1,"\n")
#print(zi.corr)

# Y = poly(x) , sample size = 50 , without ties.

x = runif(100, 3, 7)
y = (x^2) - 5*x + 6

df.x.y = data.frame(x,y)

n = 50
new.df.x.y = df.x.y %>% slice_sample(n=50, replace = FALSE)   # No ties; sampling without replacement from data frame.

order.new.df = new.df.x.y[order(new.df.x.y$x),]  # Sorting the data frame by X.

rank.y = rank(order.new.df$y) # getting ranks of Y_(i)
new.df.x.y = bind_cols(order.new.df,as.data.frame((rank.y)))  # Updating data frame, appending column of ranks.
colnames(new.df.x.y)[3] = "r.i"  # Renaming column of ranks.

print(new.df.x.y)

m = nrow(new.df.x.y) - 1
diff = c() 
for ( i in 1:m) {
  v = abs(rank.y[i+1]-rank.y[i])
  diff = c(diff,v)
}
cat("The consecutive rank differences of y values :", diff, "\n")

n = nrow(new.df.x.y)
num = 3*sum(diff)
den = n**2 - 1

zi.corr = 1 - (num/den)

cat("Correlation coefficient :", zi.corr,"\n")  

#==============================================================================

# n = 50, Y not a function of X, without ties. Sampling from overlapping uniform distribution.

x = runif(65,2,4)
y = runif(65,1,2.5)

df.x.y = data.frame(x,y)

n = 50
new.df.x.y = df.x.y %>% slice_sample(n=50, replace = FALSE)   # No ties; sampling without replacement from data frame.

order.new.df = new.df.x.y[order(new.df.x.y$x),]  # Sorting the data frame by X.

rank.y = rank(order.new.df$y) # getting ranks of Y_(i)
new.df.x.y = bind_cols(order.new.df,as.data.frame((rank.y)))  # Updating data frame, appending column of ranks.
colnames(new.df.x.y)[3] = "r.i"  # Renaming column of ranks.

print(new.df.x.y)

m = nrow(new.df.x.y) - 1
diff = c() 
for ( i in 1:m) {
  v = abs(rank.y[i+1]-rank.y[i])
  diff = c(diff,v)
}
cat("The consecutive rank differences of y values :", diff, "\n")
#print(diff)

n = nrow(new.df.x.y)
num = 3*sum(diff)
den = n**2 - 1

zi.corr = 1 - (num/den)

cat("Correlation coefficient :", zi.corr,"\n")  


