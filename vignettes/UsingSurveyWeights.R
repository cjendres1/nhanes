## ----loadlibs, eval=T, message = FALSE, warning = FALSE-----------------------
library("nhanesA")

demoj = nhanes("DEMO_J")
dim(demoj)

## merge DEMO_J and BXP_J using SEQN.  
bpxj = nhanes("BPX_J")
data = merge(demoj, bpxj, by="SEQN")
dim(data)

## ----survey, warning=FALSE, message=FALSE-------------------------------------
library("survey")
nhanesDesign <- svydesign(id = ~SDMVPSU,  # Primary Sampling Units (PSU)
                          strata  = ~SDMVSTRA, # Stratification used in the survey
                          weights = ~WTMEC2YR,   # Survey weights
                          nest    = TRUE,      # Whether PSUs are nested within strata
                          data    = data)


## ----surveydesign, message = FALSE, warning = FALSE---------------------------

# subset survey design object
dfsub = subset(nhanesDesign, data$RIDAGEYR>=40)

# subset the original dataset
datasub = data[data$RIDAGEYR>=40,]


## ----ethtables, message = FALSE, warning=FALSE--------------------------------

## mean on total data set

mean(datasub$BPXDI1, na.rm = TRUE)

##split the data by ethnicity and calculate mean of the unweighted data

unweighted_means <- sapply(split(datasub$BPXDI1, datasub$RIDRETH1), mean, na.rm=TRUE)
unweighted_means 


## ----svyby, message = FALSE, warning = FALSE----------------------------------
adjmns = svymean(~BPXDI1, dfsub, na.rm=TRUE)
adjmns

# By ethnicity
adjmnsbyEth = svyby(~BPXDI1, ~RIDRETH1, dfsub, svymean, na.rm=TRUE)

weighted_means <- as.numeric(adjmnsbyEth$BPXDI1)

combined_data <- cbind(unweighted_means, weighted_means)
colnames(combined_data) <- c("Unweighted", "Weighted")
combined_data


## ----message = FALSE, warning = FALSE-----------------------------------------
# By Gender 
mns = sapply(split(datasub$BPXDI1, datasub$RIAGENDR), mean, na.rm=TRUE)

adjmnsbyGen = svyby(~BPXDI1, ~RIAGENDR, dfsub, svymean, na.rm=TRUE)

combined_data <- cbind(mns, adjmnsbyGen$BPXDI1)
colnames(combined_data) <- c("Unweighted", "Weighted")
combined_data


## ----message = FALSE, warning = FALSE-----------------------------------------

# For the unweighted data

quantile(datasub$BPXDI1, c(0.25,0.5,.75), na.rm = TRUE)

# For the survey weighted data
svyquantile(~BPXDI1, dfsub, quantiles = c(0.25,0.5,0.75), na.rm=TRUE)
  

# By Gender 
svyby(~BPXDI1, ~RIAGENDR, dfsub, svyquantile, quantiles = c(0.5), na.rm=TRUE)


## ----message = FALSE, warning = FALSE-----------------------------------------

# For the entire dataset
svyvar(~BPXDI1, dfsub, quantiles = c(0.25,0.5,0.75), na.rm=TRUE)
  
# By ethnicity
svyby(~BPXDI1, ~RIDRETH1, dfsub, svyvar, na.rm=TRUE)
  
# By Gender 
svyby(~BPXDI1, ~RIAGENDR, dfsub, svyvar, na.rm=TRUE)


## ----message = FALSE, warning = FALSE-----------------------------------------
weighted_model <- svyglm(BPXDI1 ~ RIDAGEYR + RIDRETH1, design = dfsub, family = gaussian())
summary(weighted_model)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(ggplot2)

## recalculating means (same as above) 

unweighted_means <- sapply(split(datasub$BPXDI1, datasub$RIDRETH1), mean, na.rm=TRUE)
weighted_means <- as.numeric(adjmnsbyEth$BPXDI1)


plot_data <- data.frame(
  Ethnicity = factor(rep(names(unweighted_means), 2),
                     levels = names(unweighted_means)),
  Means = c(unweighted_means, weighted_means),
  Type = factor(c(rep("Unweighted", length(unweighted_means)),
                  rep("Weighted", length(weighted_means))),
               levels = c("Unweighted", "Weighted"))
)

# Creating the plot
ggplot(plot_data, aes(x = Ethnicity, y = Means, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Comparison of Diastolic Blood Pressure by Ethnicity",
       y = "Mean Diastolic Blood Pressure") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())



## ----message = FALSE, warning = FALSE-----------------------------------------

unweighted_model <- glm(BPXDI1 ~ RIDAGEYR + RIDRETH1, data = datasub, family = gaussian())
weighted_model <- svyglm(BPXDI1 ~ RIDAGEYR + RIDRETH1, design = dfsub, family = gaussian())

# For the unweighted model
unweighted_summary <- summary(unweighted_model)
unweighted_coefs <- unweighted_summary$coefficients[, "Estimate"]
unweighted_se <- unweighted_summary$coefficients[, "Std. Error"]

# For the weighted model
weighted_summary <- summary(weighted_model)
weighted_coefs <- as.numeric(weighted_summary$coefficients[, "Estimate"])
weighted_se <- as.numeric(weighted_summary$coefficients[, "Std. Error"])

comparison <- data.frame(
  Variable = names(unweighted_coefs),
  Unweighted = unweighted_coefs,
  Weighted = as.numeric(weighted_coefs)
)

print(comparison)

unweighted_df <- data.frame(Variable = names(unweighted_coefs), 
                            Estimate = unweighted_coefs, 
                            SE = unweighted_se, 
                            Type = "Unweighted")

weighted_df <- data.frame(Variable = names(unweighted_coefs), 
                          Estimate = weighted_coefs, 
                          SE = weighted_se, 
                          Type = "Weighted")

plot_data <- rbind(unweighted_df, weighted_df)

ggplot(subset(plot_data, Variable!='(Intercept)'), aes(x = Estimate, y = reorder(Variable, Estimate), color = Type)) +
  geom_point(position = position_dodge(0.5), size = 2.5) +
  geom_errorbarh(aes(xmin = Estimate - SE, xmax = Estimate + SE),
                 height = 0.2, position = position_dodge(0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Comparison of Regression Coefficients: Weighted vs Unweighted",
       x = "Coefficient Value", y = "Predictors") +
  theme_minimal() +
  scale_color_manual(values = c("Unweighted" = "blue", "Weighted" = "red")) +
  theme(legend.title = element_blank())


