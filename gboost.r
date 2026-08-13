library(xgboost)
cleand_data <- pdata[c("log_org_size_c","CDI", "edu", "log_inc", "CF","support", "Tech","inst2")]
cleand_data <- na.omit(cleand_data)
X <- model.matrix(CDI2 ~ log_org_size_c + log_inc  + support + inst2 +
                    edu + Tech + CF -1, data = cleand_data)
X <- as.matrix(X)
storage.mode(X) <- "double"
X <- data.matrix(data.frame(
  log_org_size = cleand_data$log_org_size_c,
  log_inc = cleand_data$log_inc,
  support = cleand_data$support,
  Tech = as.numeric(cleand_data$Tech),
  CF = as.numeric(cleand_data$CF),
  inst2 = as.numeric(cleand_data$inst2),
  edu = as.numeric(cleand_data$edu)
))

y <- cleand_data$CDI2
dtrain <- xgboost::xgb.DMatrix(data = X, label = y)
model <- xgb.train(
  params = list(objective = "reg:squarederror",  max_depth = 4,  learning_rate = 0.05),
  nrounds = 200,
  data = dtrain
)
dim(X)
colnames(X)
length(y)
cv <- xgb.cv(
  data = dtrain,
  nrounds = 200,
  nfold = 5,
  objective = "reg:squarederror"
)
importance <- xgb.importance(model = model)
print(importance)
library(SHAPforxgboost)
shap_values <- predict( model, X, predcontrib = TRUE)

importance2 <- colMeans(abs(shap_df))
importance2 <- sort(importance2, decreasing = TRUE)
shap_values <- shap.values(xgb_model = model, X_train = X)
shap_df <- as.data.frame(shap_values$shap_score)
shap_df <- shap_df[, - ncol(shap_df)]

shap_matrix <- predict(model, X, predcontrib = TRUE)
shap_matrix = shap_matrix[,-ncol(shap_matrix)]
shap_matrix = as.matrix(shap_matrix)
X_df <- as.data.frame(X)

shap.plot.summary(shap_df, X_df)


barplot(importance2)
model$feature_names

colnames(shap_df) <- paste0("shap_", colnames(shap_df))
data_shap <- cbind(X_df, shap_df)

plot(X_df$log_org_size, shap_df$log_org_size,
     xlab = "log(org_size)",
     ylab = "SHAP value",
     main = "Effect of Organization Size")
abline(h = 0, col = "red")

library(ggplot2)

ggplot(data_shap, aes(x = log_org_size, y = shap_log_org_size)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_minimal()
library(tibble)
print(data_shap, n = Inf, width = Inf)
view(data_shap)
library(writexl)
write_xlsx(data_shap,"data_shap.xlsx")


############################################################
# 1. SHAP SUMMARY PLOT
############################################################

library(xgboost)
library(ggplot2)
library(dplyr)
library(tidyr)

# SHAP values
shap_matrix <- predict(
  model,
  X,
  predcontrib = TRUE
)

# remove bias column
shap_matrix <- shap_matrix[, -ncol(shap_matrix)]

# convert to dataframe
shap_df <- as.data.frame(shap_matrix)

# ensure same names
colnames(shap_df) <- colnames(X_df)

# reshape to long format
shap_long <- shap_df %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "SHAP"
  )

# summary distribution plot
ggplot(shap_long,
       aes(x = reorder(Variable, abs(SHAP), median),
           y = SHAP)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "SHAP Summary Plot",
    x = "Variables",
    y = "SHAP value"
  )



############################################################
# 2. SHAP DEPENDENCE PLOT
# Example: Organization Size
############################################################

ggplot(data_shap,
       aes(x = log_org_size,
           y = shap_log_org_size)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    title = "",
    x = "Community size",
    y = "Shap value"
  )


# then use:
# y = shap_log_org_size



############################################################
# 3. INTERACTION PLOT
# participation × policy maturity
############################################################

library(interactions)

data_shap <- data_shap %>%
  mutate(
    inst_group = cut(
      inst2,
      breaks = quantile(inst2,
                        probs=c(0,0.33,0.66,1),
                        na.rm=TRUE),
      labels=c("Low institutional capacity",
               "Medium institutional capacity",
               "High institutional capacity"),
      include.lowest=TRUE
    )
  )

# interaction visualization
ggplot(data_shap,
       aes(x = log_org_size,
           y = shap_log_org_size,
           color = inst_group)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="loess",
              se=FALSE) +
  labs(
    x="Community size (log)",
    y="SHAP contribution of community size",
    color="Institutional capacity",
    title=""
  ) +
  theme_minimal()



############################################################
# 4. INTERACTION HEATMAP
############################################################

library(ggplot2)

# create prediction grid
grid <- expand.grid(
  log_participation =
    seq(min(cleand_data$log_participation),
        max(cleand_data$log_participation),
        length.out = 50),

  policy_maturity =
    seq(min(cleand_data$policy_maturity),
        max(cleand_data$policy_maturity),
        length.out = 50)
)

# keep other vars fixed at means
grid$log_org_size <- mean(cleand_data$log_org_size, na.rm = TRUE)
grid$log_inc <- mean(cleand_data$log_inc, na.rm = TRUE)
grid$prc <- mean(cleand_data$prc, na.rm = TRUE)
grid$support <- mean(cleand_data$support, na.rm = TRUE)

# predicted CSI
grid$predicted_CSI <- predict(
  interaction_model,
  newdata = grid
)

# heatmap
ggplot(grid,
       aes(x = log_participation,
           y = policy_maturity,
           fill = predicted_CSI)) +
  geom_tile() +
  theme_minimal() +
  labs(
    title = "Interaction Heatmap",
    x = "Log Participation",
    y = "Policy Maturity",
    fill = "Predicted CSI"
  )
