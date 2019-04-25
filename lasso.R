install.packages('glmnet')
library(glmnet)

data = read.csv("attributes_gfa_set AM to 1.csv", header = TRUE, sep = ",")
data$Class = as.factor(data$Class)
x = model.matrix(Class~.,data)[,-1]

y = data$Class
train = sample(1:nrow(x), nrow(x)*0.9) ##default replace = FALSE

test = (-train)

y.test = y[test]

grid = 10^seq(10,-2,length=100)  ##a descending sequence 

lasso.mod = glmnet(x[train,],y[train],family="binomial", alpha = 1,lambda = grid)


cv.out = cv.glmnet(x[train,],y[train],alpha=1, family="binomial")

par(mar=c(1,1,1,1))
plot(cv.out)

bestlam = cv.out$lambda.min

lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,], family="binomial", type="class")

mean(lasso.pred == y[test])
table(lasso.pred,y.test)

out=glmnet(x,y,alpha=1,lambda=grid, family="binomial")

lasso.coef = predict(out,type="coefficients",s=bestlam, family="binomial")[1:146,]

index <- sapply(lasso.coef@i, function(x) x+1)

coeffi_table <- data.frame("var" = lasso.coef@Dimnames[[1]][index],"coef" = lasso.coef@x) %>% filter(var != "(Intercept)") %>% arrange(desc(abs(coef)))

coeffi_table
