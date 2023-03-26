my_data = read.csv("~/Desktop/BAN_602_Case_3.csv", header = TRUE)
head(my_data)
colnames(my_data) <- c('T2005','AZ100')

summary(my_data)
mean(my_data$T2005)
sd(my_data$T2005)
var(my_data$T2005)
median(my_data$T2005)
quantile(my_data$T2005)
range(my_data$T2005)
max(my_data$T2005)
min(my_data$T2005)

mean(my_data$AZ100)
sd(my_data$AZ100)
var(my_data$AZ100)
quantile(my_data$AZ100)

# Conducting a hypothesis test on a condition that both T2005 and AZ100 are equally accurate
# Considering Ho <- T2005 and AZ100 are equally accurate (u1=u2)
# Ha <- T2005 and AZ100 are not eqally accurate (u1!=u2)
x <- unlist(my_data$T2005)
print(x)
y <- unlist(my_data$AZ100)
print(y)
#Finding the no. of rows
n <- nrow(my_data)
n 

# Significance Level
alpha <- 0.05

x1 <- mean(my_data$T2005)
x2 <- mean(my_data$AZ100)

v1 <- var(my_data$T2005)
v2 <- var(my_data$AZ100)

# Degree of freedom
df <- ((v1/n)+(v2/n))^2/((1/(n-1))*(v1/n)^2+(1/(n-1)*(v2/n)^2))
df

# Test statistic
# T test
t <- ((x1-x2)-0)/sqrt((v1/n)+(v2/n))
t
t_alpha <- qt((alpha/2),30,lower.tail= FALSE)
t_alpha
t.test(y, x,
       alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)


#F test
var.test(y, x, ratio = 1,
         alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95)


# As p-value > alpha we cannot reject Ho
# We can conclude that T2005 and AZ100 are equally accurate (have equal means)
