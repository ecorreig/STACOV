# En aquest fitxer implemento les funcions de lasso, ridge regression i elastic net en general
# Ho separo en dues funcions, una per regressió logística i l'altra per regressió lineal

require(glmnet)

logistic_EN = function(df, train, y_name, alpha=1, weights=FALSE, plots=TRUE, lambda="1se"){

  # Funció: Calcula logístic lasso, ridge regression i elastic net segons el valor d'alpha que li passem.
  # Arguments: dataframe, on només hi ha les variables que farem servir, inclosa la predita
  #            train: els índex de les dades d'entrenament
  #            y_index: índex o nom de la variable predita
  #            alpha: regula ridge regression - elastic net - lasso; per defecte és lasso
  #            weights: pesos, en cas que n'hi hagi
  #            plots: volem el dibuix de les lambdes i els coeficients del model? per defecte sí
  #            lambda: quina lambda voldrem a l'hora de regularitzar els models: per defete models petits (1se)
  # Retorn: una llista amb:
  #            Coeficients: els coeficients no nuls del model
  #            Preiccions: les prediccions pel subgrup test
  #            Encert: l'encert del model en les dades test

  if (sum(is.na(df)>0)){
    stop("Hi ha missings al dataframe, si us plau treu-los")
  }

  test=(-train)

  x <- df[, !names(df) %in% y_name]
  y <- df[, y_name]
  data <-cbind(x,y)
  x_train <- x[train,]
  y_train <- data$y[train]
  x_test <- x[test,]
  y_test <- data$y[test]

  if (!weights){
    model <- model.matrix(y~., data=data[train,])
    fit <- cv.glmnet(model,y_train, alpha=alpha, family="binomial")
    }
  else {
    model <- model.matrix(y~. - weights, data=data[train,])
    fit <- cv.glmnet(model,y_train,alpha=alpha, family="binomial", weights = data$weights[train])
    }

  if(plots){plot(fit)}

  if (lambda=="1se"){
    lam = "lambda.1se"
    lambda_min = fit$lambda.1se
  }
  else {
    lam= "lambda.min"
    lambda_min = fit$lambda.min
  }

  cc = predict(fit, type='coefficients', s = lam)
  coefs = cc[1:dim(cc)[1],]
  ffc = coefs[coefs!=0]

  if (!weights){
    newX <- model.matrix(~.,data=x_test)
  }
  else {
    newX <- model.matrix(~.-weights,data=x_test, weights = x_test$weights)
  }

  fit_test<-predict(fit,s=lambda_min, newx=newX, type='response')
  fit.pred=rep(0,nrow(x_test))
  fit.pred[fit_test >.5]=1
  ttt=table(fit.pred, y_test)
  rend = (ttt[1]+ttt[4])/sum(ttt)

  returns = list(ffc, fit_test, rend)
  names(returns) = c("Coeficients", "Prediccions", "Encert")

  return(returns)
}

regressive_EN = function(df, alpha=1, train, y_index, weights=FALSE, plots=TRUE, lambda="1se"){

  # Funció: Calcula linear lasso, ridge regression i elastic net segons el valor d'alpha que li passem.
  # Arguments: dataframe, on només hi ha les variables que farem servir, inclosa la predita
  #            train: els índex de les dades d'entrenament
  #            y_index: índex o nom de la variable predita
  #            alpha: regula ridge regression - elastic net - lasso; per defecte és lasso
  #            weights: pesos, en cas que n'hi hagi
  #            plots: volem el dibuix de les lambdes i els coeficients del model? per defecte sí
  #            lambda: quina lambda voldrem a l'hora de regularitzar els models: per defete models petits (1se)
  # Retorn: una llista amb:
  #            Coeficients: els coeficients no nuls del model
  #            Preiccions: les prediccions pel subgrup test

  if (sum(is.na(df)>0)){
    stop("Hi ha missings al dataframe, si us plau treu-los")
  }

  test=(-train)

  x<- df[,-c(y_index)]
  y<- df[,y_index]
  data<-cbind(x,y)
  x_train <- x[train,]
  y_train <- data$y[train]
  x_test <- x[test,]
  y_test <- data$y[test]

  if (!weights){
    model <- model.matrix(y~., data=data[train,])
    fit <- cv.glmnet(model,y_train, alpha=alpha, family="gaussian")
  }
  else {
    model <- model.matrix(y~. - weights, data=data[train,])
    fit <- cv.glmnet(model,y_train,alpha=alpha, family="gaussian", weights = weights)
  }

  if(plots){plot(fit)}

  if (lambda=="1se"){
    lam = "lambda.1se"
    lambda_min = fit$lambda.1se
  }
  else {
    lam= "lambda.min"
    lambda_min = fit$lambda.min
  }

  cc = predict(fit, type='coefficients', s = lam)
  coefs = cc[1:dim(cc)[1],]
  ffc = coefs[coefs!=0]

  if (!weights){
    newX <- model.matrix(~.,data=x_test)
  }
  else {
    newX <- model.matrix(~.-weights,data=x_test, weights = x_test$weights)
  }

  fit_test<-predict(fit,s=lambda_min, newx=newX, type='response')

  returns = list(ffc, fit_test)
  names(returns) = c("Coeficients", "Prediccions")

  return(returns)
}
