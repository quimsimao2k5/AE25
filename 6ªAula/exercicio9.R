library(ISLR2)
library(dplyr)
library(glmnet)

set.seed(7)

idx <- sample(seq_len(nrow(College)), size = floor(nrow(College) * 0.75))

#a)
train <- College[idx,]; teste <- College[-idx,]
dim(train)
dim(test)


#b)
modelo <- lm(Grad.Rate ~., data = train)
summary(modelo)

pre_ols <- predict(modelo,teste)

rmse <- function(a,b) sqrt(mean((a-b)^2))

basico <- rmse(pre_ols,teste$Grad.Rate)

#c)
Xtr <- model.matrix(Grad.Rate ~ ., train)[,-1]; ytr <- train$Grad.Rate
Xte <- model.matrix(Grad.Rate ~ ., test)[,-1];  yte <- test$Grad.Rate
cv_ridge2 <- cv.glmnet(Xtr, ytr, alpha = 0)
cv_lasso2 <- cv.glmnet(Xtr, ytr, alpha = 1)
pr_ridge <- predict(cv_ridge2, newx = Xte, s = "lambda.min")
pr_lasso <- predict(cv_lasso2, newx = Xte, s = "lambda.min")
rmse <- function(a,b) sqrt(mean((a-b)^2))
res <- tibble(
  modelo = c("ridge","lasso","linear"),
  RMSE = c(rmse(yte, pr_ridge[,1]), rmse(yte, pr_lasso[,1]), basico)
)
res
