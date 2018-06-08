########## LIME + KERAS
library(MASS)
library(lime)
library(titanic)
library(rpart)
library(rpart.plot)
library(keras)


### Daten laden
cols <- c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare" )
titanic <- read.csv("~/R/data/titanic.csv")[cols]
titanic <- titanic[complete.cases(titanic),]
train <- titanic[1:nrow(titanic),]

### Referenz
## RPart als Referenz
rpart.plot(rpart(Survived ~ ., train))

## Logistsiche Regression als Referenz
train.scaled <- train
train.scaled$Age <- scale(train$Age,center = F)
train.scaled$Pclass <- as.factor(train.scaled$Pclass)
train.scaled$SibSp <- scale(train$SibSp,center = F)
train.scaled$Fare <- scale(train$Fare,center = F)
coef(glm(Survived ~ ., train, family='binomial'))


### Lime auf LDA Model
fit <- lda(Survived ~ ., train)
explanation <- lime(train,fit)

# Test Observations
male <- titanic[7,]
female <- titanic[9,]

# Erklärungen für Beobachtungen
male_explanations <- explain(male,explanation,n_labels = 1, n_features = 3,n_permutations = 1000)
plot_features(male_explanations)
plot_explanations(male_explanations)
# => sex, dann pclass
female_explanations <- explain(female,explanation,n_labels = 1, n_features = 3,n_permutations = 1000)
plot_features(female_explanations)
plot_explanations(female_explanations)
# => sex, dann pclass



### Lime auf Multi-Layer-Perceptron
train_features <- train
train_features$Survived <- NULL
train_features$Sex <- ifelse(pull(train, Sex) == 'male',1,0)
labels <- pull (train, Survived)

## Einfaches Modell spezifizieren
model_keras <- keras_model_sequential()
model_keras %>% 
# Erstes Hidden Layer
layer_dense (units = 16, kernel_initializer = "uniform", activation = "relu",input_shape = ncol(train_features)) %>% 
layer_dropout (rate = 0.1) %>%  
# Zweites Hidden Layer
layer_dense (units = 16, kernel_initializer = "uniform", activation = "relu") %>% 
layer_dropout (rate = 0.1) %>%  
# Output Layer
layer_dense (units = 1,kernel_initializer = "uniform", activation = "sigmoid") %>% 
# Model kompilieren
compile (optimizer = 'adam',loss = 'binary_crossentropy',  metrics   = c('accuracy') ) 

# Model trainieren
system.time ( 
  model.nn <- fit (
    object           = model_keras,             
    x                = as.matrix (train_features), 
    y                = labels,             
    batch_size       = 50,     
    epochs           = 50,     
    validation_split = 0.20) ) 


## Lime für Keras vorbereiten
model_type.keras.models.Sequential <- function(x, ...) {"classification"}
predict_model.keras.models.Sequential <- function (x, newdata, type, ...) {
  pred <- predict_proba (object = x, x = as.matrix(newdata))
  data.frame (Positive = pred, Negative = 1 - pred) 
}

## Entscheidung erkären
explainer <- lime (x = train_features, model = model_keras, bin_continuous = FALSE)
explanations <- lime::explain (train_features[c(7,9), ], explainer = explainer, n_labels = 1, n_features   = 4)
plot_features (explanations)
plot_explanations(explanations)
