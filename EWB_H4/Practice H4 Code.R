testmosaic <- mosaic()

#tests for whether sex, class, or age influenced survival#
mosaic(Survived ~ ., data = Titanic)

mosaic(Survived ~ Sex + Age, data = titanic)

?mosaic

Titanic


mosaic(~pclass + Gender + survived, data=titanic, main = "Survival on the Titanic")
?mosaic

mosaic(~ Sex + Age + Survived, data = Titanic,
       main = "Survival on the Titanic", shade = TRUE, legend = TRUE
       
       
       ##Question 3###
       #Create mosaic plots for categorical variables
       library(vcd)
       mosaic(lowbwt~smoker, data=my.data)
       
#Question 3#
library(vcd)
mosaic(survived~Gender, data = titanic)
mosaic(survived~pclass, data = titanic)
mosaic(survived~Residence, data = titanic)

titanic
library(popbio)
vartitanic <- titanic %>% select(1, 3, 5, 9, 15, 2)
vartitanic
titanona<-drop_na(vartitanic)
titanona

logi.hist.plot(titanona$age, titanona$survived, boxp=FALSE,type="hist",col="gray", xlabel="Passenger Age")
logi.hist.plot(titanona$fare, titanona$survived, boxp=FALSE,type="hist",col="gray", xlabel="Fare Price")


mosaic(survived~fare, data = titanic)

#my parameters
Gender
pclass
Residence
age
fare


#Question 4
```{r Selecting the best model}
library(bestglm)
titanona
#The code did not appreciate tibble format#
titanewna <- as.data.frame(titanona)
titanewna
bestglm(titanewna,IC="AIC",family=binomial)
```
#Question 5
```{r Fitting the best model, including class, residence, age, and gender}
titamodel <- glm(survived~pclass+Residence+age+Gender, data = titanewna)
summary.lm(titamodel)
```

#Question 6
```{r Purposeful selection, testing each candidate parameter}
univariate.pclass <- glm(survived~pclass, data=titanewna, family=binomial(link="logit"))
summary(univariate.pclass)
#significant#

univariate.age <- glm(survived~age, data=titanewna, family=binomial(link="logit"))
summary(univariate.age)
#not significant#

univariate.Residence <- glm(survived~Residence, data=titanewna, family=binomial(link="logit"))
summary(univariate.Residence)
#significant#

univariate.fare <- glm(survived~fare, data=titanewna, family=binomial(link="logit"))
summary(univariate.fare)
#significant#

univariate.Gender <- glm(survived~Gender, data=titanewna, family=binomial(link="logit"))
summary(univariate.Gender)
#significant#

titapurpose <- glm(survived~)
```

#Question 
```{r Summary}

```

