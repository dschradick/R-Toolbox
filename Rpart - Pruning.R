########## R-PART ANALYSE
library(rpart)
library(rpart.utils)
library(rpart.plot)
library(gmodels)
library(dplyr)
library(visNetwork)


### Daten einlesen
titanic <- read.csv("~/Documents/Data/titanic.csv")
titanic <- select(titanic, Survived, Age, Embarked, Sex, SibSp, Parch, Fare)
set.seed(0) ; 
index_train <- sample(1:nrow(titanic), 2 / 3 * nrow(titanic))
train_set <- titanic[index_train, ] ; 
test_set <- titanic[-index_train, ]


### Coarse classification
titanic <- titanic %>% 
  mutate(age_group = case_when(Age < 15 ~ 'KID',
                               Age < 35 ~ 'ADULT',
                               T ~ 'SENIOR'))
titanic$Age <- NULL

### Daten betrachten
CrossTable(titanic$Survived)
# prop.r = row proportions
CrossTable(titanic$Survived,titanic$age_group, prop.r = T, prop.c = F, prop.t = F, prop.chisq = F)


### Fitten
forumula <- factor(Survived) ~ .
fit <- rpart(forumula, data=train_set)
visTree(fit)

### Plotten
# http://oldemarrodriguez.com/yahoo_site_admin/assets/docs/Ploting_a_Tree_with_prp.234140400.pdf
# Einfacher plot
plot(fit) ; text(fit)
# besser
# par(mfrow=c(1,2))
# branch.type=5 => dicke der kante = anzahl der beobachtungen

prp(fit,
    extra = 102,              # Inhalt der Konten
                              # 100 = Prozent von Cases
                              # 1 = Anzahl der cases in den leaves, 2 = Classification Rate
    type = 2,                 # Position von split entscheidung 2 = unter knoten 
    fallen.leaves = T,
    compress = T,             # komprimierte darstellung
    yesno = 1,                # yes/no einblenden
    box.col=c("pink",         
              "palegreen3"),
    #box.palette=c("auto"),   # Farben
    #varlen=3,                # Länge des Variablennamens
    nn=F)                     # Node-Id anzeigen (zum zurordnen der Regeln)


# Variable Importance
fit$variable.importance

### Klassen-Imbalance
# Undersampling, prior, loss-matrix => siehe Scoring2.R

#### Pruning
## 1. Bestimmen von complexity parameter von Baum mit minimaler cv-error-rate
#plotcp(fit)   # cv-error-rate als Funktion des complexity parameter
printcp(fit)  # Identifikation für welchen complexity parameter cv-error-rate minimal
index <- which.min(tree_prior$cptable[ , "xerror"]) 
tree_min <- tree_prior$cptable[index, "CP"]

## 2. Pruning durchführen
fit.pruned <- prune(fit, cp = tree_min)
prp(fit.pruned,fallen.leaves = T,
    extra = 102,  
    type = 1,     
    compress = T, 
    yesno = T,    
    box.palette=c("auto"), 
    varlen=3,     
    nn=F)       


### Auswertung des Modells
fit.pruned$predict.proba <- predict(fit.pruned, newdata = test_set)[,2]
fit.pruned$predict.class <- predict(fit.pruned, newdata = test_set, type='class')
(conf.matrix <- table(test_set$Survived, fit.pruned$predict.class))
(accuracy <- sum(diag(conf.matrix)) / nrow(test_set))
(sensitivity <- caret::sensitivity(conf.matrix))
(specificity <- caret::specificity(conf.matrix))
(roc <- pROC::roc(test_set$Survived, fit.pruned$predict.proba))
par(mfrow=c(1,1)); plot(roc)
