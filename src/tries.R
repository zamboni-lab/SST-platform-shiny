
a = data.frame(one = 1:10, two = rep(c(0,1),5), three = 1:10 + runif(10))

b = a[a["two"] == 0, 3]

b[b>2]

