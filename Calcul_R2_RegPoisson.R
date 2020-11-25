### Input 
dt2 <- read.csv("All_Variables_Commune_ALD2.csv", header = T, sep = ",")
head(dt2); dim(dt2)
which(is.na(dt2$total.Club.sportif)) ### NA values in Clubs sportifs
which(is.na(dt2$occurrence))
## enlever les NA valeurs
inadt2 <- (unique (unlist (lapply (dt2, function (x) which (is.na (x)))))); length(inadt2)
inp <- dt2[-inadt2,]; head(inp); dim(inp)

inp$TauxOcc <- inp$occurrence/inp$Population*1000
inp$TauxClub <- inp$total.Club.sportif/inp$Population*1000

##########################
### Regression Linear
regressor1 = lm(formula = TauxOcc ~ 
                  TauxBac  + TauxChomage + Salaire + TauxOuvrier + TauxClub 
                +   StatutCommune
                ,data = inp)
summary(regressor1) # Multiple R-squared:  0.9001
### Regression de Poisson
regressor2 = glm(formula = occurrence ~ 
                   TauxBac + TauxChomage + Salaire + TauxOuvrier + TauxClub 
                 + StatutCommune 
                 + offset(log(Population))
                 , data = inp, family = poisson(link=log))
summary(regressor2) #AIC: 2917.4


##########################
set.seed(123)
dataset=inp
split = sample(seq_len(nrow(dataset)), size = floor(0.8*nrow(dataset)))
training_set = dataset[split,]
test_set = dataset[-split,]

regressorB = glm(formula = occurrence ~ 
                   TauxBac + TauxChomage + Salaire + TauxOuvrier + TauxClub 
                 + StatutCommune 
                 + offset(log(Population))
                 , data = training_set, family = poisson(link=log))
summary(regressorB)

# Calculation R2
# avec le nombre d'ALD-MDS
y_pred = predict(regressorB, newdata = test_set, type = "response")
SSE <- sum((test_set$occurrence - y_pred)^2); SSE
SST <- sum((test_set$occurrence - mean(test_set$occurrence))^2); SST
1-SSE/SST 

y_pred = predict(regressorB, newdata = training_set, type = "response")
SSE <- sum((training_set$occurrence - y_pred)^2); SSE
SST <- sum((training_set$occurrence - mean(training_set$occurrence))^2); SST
1-SSE/SST 

# avec le taux d'ALD-MDS
y_pred = predict(regressorB, newdata = test_set, type = "response")
SSE <- sum((test_set$occurrence/test_set$Population - y_pred/test_set$Population)^2); SSE
SST <- sum((test_set$occurrence/test_set$Population - mean(test_set$occurrence/test_set$Population))^2); SST
1-SSE/SST 

y_pred = predict(regressorB, newdata = training_set, type = "response")
SSE <- sum((training_set$occurrence/training_set$Population - y_pred/training_set$Population)^2); SSE
SST <- sum((training_set$occurrence/training_set$Population - mean(training_set$occurrence/training_set$Population))^2); SST
1-SSE/SST 









