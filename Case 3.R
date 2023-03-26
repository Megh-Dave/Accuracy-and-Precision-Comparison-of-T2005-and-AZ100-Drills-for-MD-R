my_data = read.csv("~/Desktop/BAN_602_Case_3.csv", header = TRUE)
head(my_data)
colnames(my_data) <- c('T2005','AZ100')

summary(BAN_602_Case_3)
mean(BAN_602_Case_3$T2005)
sd(BAN_602_Case_3$T2005)
var(BAN_602_Case_3$T2005)
median(BAN_602_Case_3$T2005)
quantile(BAN_602_Case_3$T2005)
range(BAN_602_Case_3$T2005)
max(BAN_602_Case_3$T2005)
min(BAN_602_Case_3$T2005)

mean(BAN_602_Case_3$AZ100)
sd(BAN_602_Case_3$AZ100)
var(BAN_602_Case_3$AZ100)
quantile(BAN_602_Case_3$AZ100)

# Conducting a hypothesis test on a condition that both T2005 and AZ100 are equally accurate
# Considering Ho <- T2005 and AZ100 are equally accurate (u1=u2)
# Ha <- T2005 and AZ100 are not eqally accurate (u1!=u2)
x <- unlist(BAN_602_Case_3$T2005)
print(x)
y <- unlist(BAN_602_Case_3$AZ100)
print(y)
#Finding the no. of rows
n <- nrow(BAN_602_Case_3)
n 

# Significance Level
alpha <- 0.05

x1 <- mean(BAN_602_Case_3$T2005)
x2 <- mean(BAN_602_Case_3$AZ100)

v1 <- var(BAN_602_Case_3$T2005)
v2 <- var(BAN_602_Case_3$AZ100)

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