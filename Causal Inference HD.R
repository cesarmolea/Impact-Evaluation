## Data Mining SigTierras
## Date: January 17th 2020
## Author: César Eduardo Montiel Olea
## I applied Imbens and Rubin methodology (2012) to select variables on PSM when you face
##   a great number of covariates in your data

################################
# 0. Installing packages    ####
################################

# Install packages
if (!require("devtools")) install.packages("devtools"); library("devtools")
if (!require("hdm")) install.packages("hdm"); library("hdm")
if (!require("caTools")) install.packages("caTools"); library("caTools")
if (!require("foreign")) install.packages("foreign"); library("foreign")
if (!require("nnet")) install.packages("nnet"); library("nnet")
if (!require("reshape2")) install.packages("reshape2"); library("reshape2")
if (!require("glmnet")) install.packages("glmnet"); library("glmnet")
if (!require("nnet")) install.packages("nnet"); library("nnet")
if (!require("randomForest")) install.packages("randomForest") # install if necessary
if (!require("MASS")) install.packages("MASS") # install if necessary
if (!require("caret")) install.packages("caret"); library("caret")
if (!require("ada")) install.packages("ada"); library("ada")
if (!require("descr")) install.packages("descr"); library("descr")
if (!require("gbm")) install.packages("gbm"); library("gbm")
if (!require("e1071")) install.packages("e1071"); library("e1071")
if (!require("twang")) install.packages("twang"); library("twang")

###################################
# 1. Preparing data               #
###################################

# Loading the data
setwd("C:/Users/CESARMO/Inter-American Development Bank Group/Zane, Giulia - Sigtierras/Data/ML")
data <- read.csv("ML_big.csv")
data <- na.omit(data)

data$own_appliances <- as.factor(data$own_appliances)
data$own_tv <- as.factor(data$own_tv)
data$bono <- as.factor(data$bono)
data$hh_head_fem <- as.factor(data$hh_head_fem)
data$ethnic <- as.factor(data$ethnic)
data$head_casado <- as.factor(data$head_casado)
data$hh_primary <- as.factor(data$hh_primary)

#data$treated <- as.factor(data$treated)

data$riego_p <- as.factor(data$riego_p)
data$manuse_p <- as.factor(data$manuse_p)
data$herbuse_p <- as.factor(data$herbuse_p)

######################################
## 2. PSM                       #####
######################################

extra <- c("own_appliances", "propriegotot", "pct_elevate", 
  "pct_private", "tot_area", "hh_age", "hh_head_fem",
  "head_casado", "total_agriculture", "areat", "rentada", "partir", "ptrhh_ex",
   "tpropia", "tprop_ex", "propia_ex", "ptrhh_s2", "rent_s2", "partir_s2", 
   "ptrhh_s3", "propia_r", "prop_rex", "tpropia_r", "ptrhh_rex", "seedq", 
   "seedq_p", "estq_p", "pmtransc", "plant_m", "pest_h", "pest_c", "harv_m", 
  "pmtrans", "aso_trans", "seed", "otherpm", "perntjfem", "perntjchild", "perntjmale", "perntjhired", 
   "manuse_p", "lprepp_hp", "lprepp_np", "lprepp_pp", "per_pern")
   
covs <- data[, !(colnames(data) %in% extra)]
covs$treated <- as.numeric(covs$treated)
lm <- lm(treated ~ indice + parcels_notitle + N_no_document + avgyears + bono + 
           mem_prodage + perareas, data = data)

base <- c("indice", "parcels_notitle", "N_no_document", "avgyears", "bono",
          "mem_prodage", "perareas")

df <- NULL
df <- data.frame(var_name = extra)

var_extra <-  extra
var_base <- base
row.names(df) <- extra

# double loop 
for (i in 1:nrow(df)) {
  for (j in var_extra) {
    formula <- paste("treated ~", paste(c(paste(var_base, collapse = " + "), j), collapse = " + "))
    reg <- lm(formula, data = data)
    r2 <- summary(reg)$adj.r.squared
    df[which(extra %in% j),i+1] <- r2
    print(j)
  }
  max_var <- apply(df[, i+1, drop = F],2, function(x) rownames(df)[which.max(x)])
  var_extra <- var_extra[!var_extra %in% max_var]
  var_base <- c(var_base, as.character(max_var))
  var_base
  print(i)
}

# we have 30 variables that improve the R2 adjusted 

useless <- df[, c(31:46)] # we keep 16 models that we don't want
useless <- subset(useless, V31 != "NA") #only rows with NA
useful <- row.names(useless) # useless variables
var.imp <- extra[!extra %in% useful] #useful variables
new.base <- c(var.imp, base) # this is the new base: 29 variables selected by the model plus the 7 of base

#--------------------------------------
## Including quadratics
#--------------------------------------

#new.quad <- covs[!colnames(covs) %in% extra] #quadratics

# take out factor variables
# x <- new.quad[, -c(2)]

factors <- sapply(covs, is.factor)
factors.df <- covs[, factors]
x <- covs[!colnames(covs) %in% colnames(factors.df)] 
x <- x[, c(-1, -23)]

for (i in 1:length(x)) {
  new.extra[i] <- x[i]^2
}

vnames <- c(colnames(x))
quad <- c("quad")
names(new.extra) <- paste(vnames, quad, sep = "") # paste new names with quadratic
data <- cbind(data, new.extra)

# names(new.extra)[names(new.extra) == 'new.quad[, c(2)]'] <- 'hh_head_fem'

## removing garbage
rm(covs, lm, reg, useless, x, extra, var_base, var_extra)

# we need to define a new base and a new extra variables
new.extra <- colnames(new.extra)
df <- NULL
df <- data.frame(var_name = new.extra)

extra <- new.extra
var_extra <-  new.extra
var_base <- new.base
row.names(df) <- new.extra

# double loop 
for (i in 1:nrow(df)) {
  for (j in var_extra) {
    formula <- paste("treated ~", paste(c(paste(var_base, collapse = " + "), j), collapse = " + "))
    reg <- lm(formula, data = data)
    r2 <- summary(reg)$adj.r.squared
    df[which(extra %in% j),i+1] <- r2
    print(j)
  }
  max_var <- apply(df[, i+1, drop = F],2, function(x) rownames(df)[which.max(x)])
  var_extra <- var_extra[!var_extra %in% max_var]
  var_base <- c(var_base, as.character(max_var))
  var_base
  print(i)
}

# Check R2
check.r2 <- df[, c(37:39)] # in model 37 there is no improvement, we keep 36 polynomials

# we have 38 variables that improve the R2 adjusted 

useless <- df[, c(38:111)] # we keep 36 models
useless <- subset(useless, V38 != "NA") #only rows without NA which are the variables that we don't want
useless <- row.names(useless) 

new.var.imp <- extra[!extra %in% useless] # usefull variables
# this is the new base: 29 variables selected by the model plus the  of base
new.base <- c(var.imp, new.var.imp, base) 

quadratics <- data[colnames(data) %in% new.base] # usefull variables
quadratics <- cbind(data$ID_HOGAR, data$treated, quadratics)
write.csv(quadratics, file = "data_quadratics.csv")


