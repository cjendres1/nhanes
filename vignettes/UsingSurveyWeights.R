## ----loadlibs, eval = FALSE, message = FALSE, warning = FALSE-----------------
# library("nhanesA")
# 
# demoj = nhanes("DEMO_J")
# dim(demoj)

## ----ll_print, echo = FALSE---------------------------------------------------
cat(sprintf("[1] 9254   46"))

## ----merge_example, eval = FALSE----------------------------------------------
# ## merge DEMO_J and BXP_J using SEQN.
# bpxj = nhanes("BPX_J")
# data = merge(demoj, bpxj, by="SEQN")
# dim(data)

## ----merge_print, echo = FALSE------------------------------------------------
cat(sprintf("[1] 8704   66"))

## ----survey, eval = FALSE, warning = FALSE, message = FALSE-------------------
# library("survey")
# nhanesDesign <- svydesign(id = ~SDMVPSU,  # Primary Sampling Units (PSU)
#                           strata  = ~SDMVSTRA, # Stratification used in the survey
#                           weights = ~WTMEC2YR,   # Survey weights
#                           nest    = TRUE,      # Whether PSUs are nested within strata
#                           data    = data)
# 

## ----surveydesign, eval = FALSE, message = FALSE, warning = FALSE-------------
# 
# # subset survey design object
# dfsub = subset(nhanesDesign, data$RIDAGEYR>=40)
# 
# # subset the original dataset
# datasub = data[data$RIDAGEYR>=40,]
# 

## ----ethtables, eval = FALSE, message = FALSE, warning=FALSE------------------
# 
# ## mean on total data set
# 
# mean(datasub$BPXDI1, na.rm = TRUE)

## ----ethtables_print, echo = FALSE--------------------------------------------
cat(sprintf("[1] 73.04455"))

## ----eth_means, eval = FALSE--------------------------------------------------
# ##split the data by ethnicity and calculate mean of the unweighted data
# 
# unweighted_means <- sapply(split(datasub$BPXDI1, datasub$RIDRETH1), mean, na.rm=TRUE)
# unweighted_means
# 

## ----eth_means_print, echo = FALSE--------------------------------------------
df <- data.frame(matrix(1,nrow=1,ncol=5))
names(df) <- c('   Mexican American','   Other Hispanic','   Non-Hispanic White',
               '   Non-Hispanic Black','   Other Race - Including Multi-Racial')
df[1,] <- list('72.41000','72.97611','70.84130','75.71466','74.41311')

print(df,row.names=FALSE)

## ----svyby, eval = FALSE, message = FALSE, warning = FALSE--------------------
# adjmns = svymean(~BPXDI1, dfsub, na.rm=TRUE)
# adjmns

## ----svyby_print, echo = FALSE------------------------------------------------
df <- data.frame(matrix(1,nrow=1,ncol=3))
names(df) <- c('','mean', 'SE')
df[1,] <- list('BPXDI1', '73.237', '0.5597')

print(df,row.names=FALSE)

## ----eth_combined, eval = FALSE-----------------------------------------------
# # By ethnicity
# adjmnsbyEth = svyby(~BPXDI1, ~RIDRETH1, dfsub, svymean, na.rm=TRUE)
# 
# weighted_means <- as.numeric(adjmnsbyEth$BPXDI1)
# 
# combined_data <- cbind(unweighted_means, weighted_means)
# colnames(combined_data) <- c("Unweighted", "Weighted")
# combined_data
# 

## ----eth_combined_print, echo = FALSE-----------------------------------------
df_uw <- data.frame(matrix(1,nrow=5,ncol=3))
names(df_uw) <- list('','Unweighted', 'Weighted')
df_uw[1,] <- list('Mexican American','72.41000','74.03194')
df_uw[2,] <- list('Other Hispanic','72.97611','74.73756')
df_uw[3,] <- list('Non-Hispanic White','70.84130','72.45422')
df_uw[4,] <- list('Non-Hispanic Black','75.71466','75.71874')
df_uw[5,] <- list('Other Race - Including Multi-Racial','74.41311','74.60215')

print(df_uw,row.names=FALSE)

## ----bygender, eval = FALSE, message = FALSE, warning = FALSE-----------------
# # By Gender
# mns = sapply(split(datasub$BPXDI1, datasub$RIAGENDR), mean, na.rm=TRUE)
# 
# adjmnsbyGen = svyby(~BPXDI1, ~RIAGENDR, dfsub, svymean, na.rm=TRUE)
# 
# combined_data <- cbind(mns, adjmnsbyGen$BPXDI1)
# colnames(combined_data) <- c("Unweighted", "Weighted")
# combined_data
# 

## ----bygender_print, echo = FALSE---------------------------------------------
df <- data.frame(matrix(1,nrow=2,ncol=3))
names(df) <- c('','Unweighted', 'Weighted')
df[1,] <- list('Male', '74.67330', '75.31134')
df[2,] <- list('Female', '71.43385', '71.31453')

print(df,row.names=FALSE)

## ----svq1, eval = FALSE, message = FALSE, warning = FALSE---------------------
# 
# # For the unweighted data
# 
# quantile(datasub$BPXDI1, c(0.25,0.5,.75), na.rm = TRUE)

## ----echo = FALSE-------------------------------------------------------------
df <- data.frame(matrix(1,nrow=1,ncol=3))
names(df) <- c('25%', '50%', '75%')
df[1,] <- list('66', '74', '82')

print(df,row.names=FALSE)

## ----svq2, eval = FALSE-------------------------------------------------------
# # For the survey weighted data
# svyquantile(~BPXDI1, dfsub, quantiles = c(0.25,0.5,0.75), na.rm=TRUE)

## ----svq2_print, message = FALSE, echo = FALSE--------------------------------
df <- data.frame(matrix(1,nrow=9,ncol=1))
names(df) <- '$BPXDI1                                '
df[1,] <- '    quantile ci.2.5 ci.97.5        se'
df[2,] <- '0.25       66     66      68 0.4691643'
df[3,] <- '0.5        74     74      76 0.4691643'
df[4,] <- '0.75       80     80      82 0.4691643'
df[5,] <- ''
df[6,] <- 'attr(,"hasci")                       '
df[7,] <- '[1] TRUE                             '
df[8,] <- 'attr(,"class")                       '
df[9,] <- '[1] "newsvyquantile"                 '

print(df, row.names=FALSE)

## ----svq3, eval = FALSE-------------------------------------------------------
# # By Gender
# svyby(~BPXDI1, ~RIAGENDR, dfsub, svyquantile, quantiles = c(0.5), na.rm=TRUE)
# 

## ----svq3_print, echo = FALSE-------------------------------------------------
df <- data.frame(matrix(1,nrow=2,ncol=3))
names(df) <- c('RIAGENDR','BPXDI1','se.BPXDI1')
df[1,] <- list('Male','76','0.9383286')
df[2,] <- list('Female','72','0.4691643')

print(df, row.names=FALSE)

## ----eval = FALSE, message = FALSE, warning = FALSE---------------------------
# 
# # For the entire dataset
# svyvar(~BPXDI1, dfsub, quantiles = c(0.25,0.5,0.75), na.rm=TRUE)

## ----echo=FALSE---------------------------------------------------------------
df <- data.frame(matrix(1,nrow=1,ncol=3))
names(df) <- c('', 'variance', 'SE')
df[1,] <- list('BPXDI1', '173.68', '12.279')

print(df,row.names=FALSE)

## ----eval = FALSE, message = FALSE, warning = FALSE---------------------------
#   # By ethnicity
# svyby(~BPXDI1, ~RIDRETH1, dfsub, svyvar, na.rm=TRUE)

## ----echo=FALSE---------------------------------------------------------------
df <- data.frame(matrix(1,nrow=5,ncol=3))
names(df) <- c('RIDRETH1','BPXDI1','se')
df[1,] <- list('Mexican American','200.8781','45.43074')
df[2,] <- list('Other Hispanic','160.0157','23.79938')
df[3,] <- list('Non-Hispanic White','168.3504','18.30355')
df[4,] <- list('Non-Hispanic Black','202.3565','37.12593')
df[5,] <- list('Other Race - Including Multi-Racial','156.9968','17.26037')

print(df,row.names=FALSE)

## ----eval = FALSE, message = FALSE, warning = FALSE---------------------------
#   # By Gender
# svyby(~BPXDI1, ~RIAGENDR, dfsub, svyvar, na.rm=TRUE)
# 

## ----echo=FALSE---------------------------------------------------------------
df <- data.frame(matrix(1,nrow=2,ncol=3))
names(df) <- c('RIAGENDR', 'BPXDI1', 'se')
df[1,] <- list('Male', '141.2704', '10.52796')
df[2,] <- list('Female', '196.1199', '20.65706')

print(df,row.names=FALSE)

## ----eval = FALSE, message = FALSE, warning = FALSE---------------------------
# weighted_model <- svyglm(BPXDI1 ~ RIDAGEYR + RIDRETH1, design = dfsub, family = gaussian())
# summary(weighted_model)

## ----svyglm, echo = FALSE-----------------------------------------------------
cat(sprintf(paste('Call:',
            '\n',
            '\nsvyglm(formula = BPXDI1 ~ RIDAGEYR + RIDRETH1, design = dfsub,',
            '\n    family = gaussian())',
            '\n',
            '\nSurvey design:',
            '\nsubset(nhanesDesign, data$RIDAGEYR >= 40)',
            '\n',
            '\nCoefficients:',
            '\n                                            Estimate Std. Error t value',
            '\n(Intercept)                                 90.96516    1.24720  72.935',
            '\nRIDAGEYR                                    -0.31522    0.01853 -17.012',
            '\nRIDRETH1Other Hispanic                       1.43894    1.25518   1.146',
            '\nRIDRETH1Non-Hispanic White                   0.49810    0.73132   0.681',
            '\nRIDRETH1Non-Hispanic Black                   2.92332    1.09594   2.667',
            '\nRIDRETH1Other Race - Including Multi-Racial  1.38532    0.63780   2.172',
            '\n                                            Pr(>|t|)',
            '\n(Intercept)                                 5.73e-15 ***',
            '\nRIDAGEYR                                    1.04e-08 ***',
            '\nRIDRETH1Other Hispanic                        0.2783',
            '\nRIDRETH1Non-Hispanic White                    0.5113',
            '\nRIDRETH1Non-Hispanic Black                    0.0236 *',
            '\nRIDRETH1Other Race - Including Multi-Racial   0.0550 .',
            '\n---',
            "\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
            '\n',
            '\n(Dispersion parameter for gaussian family taken to be 162.6653)',
            '\n',
            '\nNumber of Fisher Scoring iterations: 2'
            )))

## ----fig.width=7, eval = FALSE, message = FALSE, warning = FALSE--------------
# library(ggplot2)
# 
# ## recalculating means (same as above)
# 
# unweighted_means <- sapply(split(datasub$BPXDI1, datasub$RIDRETH1), mean, na.rm=TRUE)
# weighted_means <- as.numeric(adjmnsbyEth$BPXDI1)
# 
# 
# plot_data <- data.frame(
#   Ethnicity = factor(rep(names(unweighted_means), 2),
#                      levels = names(unweighted_means)),
#   Means = c(unweighted_means, weighted_means),
#   Type = factor(c(rep("Unweighted", length(unweighted_means)),
#                   rep("Weighted", length(weighted_means))),
#                levels = c("Unweighted", "Weighted"))
# )
# 
# # Creating the plot
# ggplot(plot_data, aes(x = Ethnicity, y = Means, fill = Type)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.6) +
#   labs(title = "Comparison of Diastolic Blood Pressure by Ethnicity",
#        y = "Mean Diastolic Blood Pressure") +
#   scale_x_discrete(labels = c('Mex-Amer','Other Hisp','White', 'Black', 'Other')) +
#   scale_fill_manual(values = c("blue", "red")) +
#   theme_minimal() +
#   theme(legend.title = element_blank())
# 
# 

## ----bp_by_eth_plot, echo = FALSE, fig.width=7, message = FALSE, warning = FALSE----
unweighted_means <- as.numeric(df_uw$Unweighted)
names(unweighted_means) <- df_uw[[1]]
weighted_means <- as.numeric(df_uw$Weighted)
plot_data <- data.frame(
  Ethnicity = factor(rep(names(unweighted_means), 2),
                     levels = names(unweighted_means)),
  Means = c(unweighted_means, weighted_means),
  Type = factor(c(rep("Unweighted", length(unweighted_means)),
                  rep("Weighted", length(weighted_means))),
               levels = c("Unweighted", "Weighted"))
)

# Creating the plot
library(ggplot2)
ggplot(plot_data, aes(x = Ethnicity, y = Means, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Comparison of Diastolic Blood Pressure by Ethnicity",
       y = "Mean Diastolic Blood Pressure") +
  scale_x_discrete(labels = c('Mex-Amer','Other Hisp','White', 'Black', 'Other')) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

## ----fig.width=7, eval = FALSE, message = FALSE, warning = FALSE--------------
# 
# unweighted_model <- glm(BPXDI1 ~ RIDAGEYR + RIDRETH1, data = datasub, family = gaussian())
# weighted_model <- svyglm(BPXDI1 ~ RIDAGEYR + RIDRETH1, design = dfsub, family = gaussian())
# 
# # For the unweighted model
# unweighted_summary <- summary(unweighted_model)
# unweighted_coefs <- unweighted_summary$coefficients[, "Estimate"]
# unweighted_se <- unweighted_summary$coefficients[, "Std. Error"]
# 
# # For the weighted model
# weighted_summary <- summary(weighted_model)
# weighted_coefs <- as.numeric(weighted_summary$coefficients[, "Estimate"])
# weighted_se <- as.numeric(weighted_summary$coefficients[, "Std. Error"])
# 
# comparison <- data.frame(
#   Variable = names(unweighted_coefs),
#   Unweighted = unweighted_coefs,
#   Weighted = as.numeric(weighted_coefs)
# )
# 
# print(comparison)

## ----visregcompare, echo = FALSE----------------------------------------------
unwt_coef_names <- c("(Intercept)", "RIDAGEYR", "RIDRETH1Other Hispanic", "RIDRETH1Non-Hispanic White", "RIDRETH1Non-Hispanic Black", "RIDRETH1Other Race - Including Multi-Racial")
unweighted_coefs <- c(92.2848149, -0.3460552,  1.2325637,  0.7553912,  4.2996735,  2.0667520)
weighted_coefs <- c(90.9651590, -0.3152158,  1.4389441,  0.4981020,  2.9233165,  1.3853204)

comparison <- data.frame(
  Variable = unwt_coef_names,
  Unweighted = unweighted_coefs,
  Weighted = as.numeric(weighted_coefs)
)

print(comparison)

## ----visregress, eval = FALSE-------------------------------------------------
# unweighted_df <- data.frame(Variable = names(unweighted_coefs),
#                             Estimate = unweighted_coefs,
#                             SE = unweighted_se,
#                             Type = "Unweighted")
# 
# weighted_df <- data.frame(Variable = names(unweighted_coefs),
#                           Estimate = weighted_coefs,
#                           SE = weighted_se,
#                           Type = "Weighted")
# 
# plot_data <- rbind(unweighted_df, weighted_df)
# 
# ggplot(subset(plot_data, Variable!='(Intercept)'), aes(x = Estimate, y = reorder(Variable, Estimate), color = Type)) +
#   geom_point(position = position_dodge(0.5), size = 2.5) +
#   geom_errorbarh(aes(xmin = Estimate - SE, xmax = Estimate + SE),
#                  height = 0.2, position = position_dodge(0.5)) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
#   labs(title = "Comparison of Regression Coefficients",
#        x = "Coefficient Value", y = "Predictors") +
#   theme_minimal() +
#   scale_color_manual(values = c("Unweighted" = "blue", "Weighted" = "red")) +
#   theme(legend.title = element_blank())
# 

## ----regression_coeff, fig.width=7, echo = FALSE, message = FALSE, warning = FALSE----


unweighted_coefs <- c(92.2848149, -0.3460552,  1.2325637,  0.7553912,  4.2996735,  2.0667520)
weighted_coefs <- c(90.9651590, -0.3152158,  1.4389441,  0.4981020,  2.9233165,  1.3853204)
names(unweighted_coefs) <- c("(Intercept)", "RIDAGEYR", "RIDRETH1Other Hispanic", "RIDRETH1Non-Hispanic White", "RIDRETH1Non-Hispanic Black", "RIDRETH1Other Race - Including Multi-Racial")

unweighted_se <- c(1.36340677, 0.02061825, 1.04001732, 0.79478016, 0.83753015, 0.86954266)
weighted_se <- c(1.24720135, 0.01852892, 1.25517931, 0.73131800, 1.09593669, 0.63780221)


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
  labs(title = "Comparison of Regression Coefficients",
       x = "Coefficient Value", y = "Predictors") +
  theme_minimal() +
  scale_color_manual(values = c("Unweighted" = "blue", "Weighted" = "red")) +
  theme(legend.title = element_blank())

