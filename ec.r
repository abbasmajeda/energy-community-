library(plm)
library(tidyr)
library(zoo) #interpolation
library(phtt) # AMG
library(stargazer)
library(lmtest)
library(sandwich)
library(spdep)
library(dplyr)
library(betareg)
library(texreg)

pdata <- pdata.frame(pdata, index = c("country", "year"))

pdata <- pdata %>% group_by(country) %>% filter(n() >=5)

pdata <- pdata.frame(pdata, index = c("country", "year"))

pdata <- pdata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    org_count = na.approx(org_count, x = year, na.rm = FALSE)
  ) %>%
  ungroup()

pdata <- pdata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(org_count = na.locf(org_count, na.rm = FALSE)) %>%
  ungroup()

pdata <- pdata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    sharespc = na.approx(sharespc, x = year, na.rm = FALSE)
  ) %>%
  ungroup()

pdata <- pdata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(sharespc = na.locf(sharespc, na.rm = FALSE)) %>%
  ungroup()


pdata <- pdata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    CF = na.approx(CF, x = year, na.rm = FALSE)
  ) %>%
  ungroup()

pdata <- pdata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    Tech = na.approx(Tech, x = year, na.rm = FALSE)
  ) %>%
  ungroup()

pdata <- pdata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(Tech = na.locf(Tech, na.rm = FALSE)) %>%
  ungroup()
pdata <- pdata %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(CF = na.locf(CF, na.rm = FALSE)) %>%
  ungroup()

gov_data <- GOV_WGI_GE[GOV_WGI_GE$LABEL=="Metric: Governance estimate (approx. -2.5 to +2.5)" & GOV_WGI_GE$year >= 2005,c("year","country","gov_eff")]
urban_data <- urbanization %>% pivot_longer(cols=-country, names_to = "year", values_to = "urban")
rq_data <- rq[rq$year >= 2005,c("year","country","rq")]
edu_data <- edu %>% pivot_longer(cols=-country, names_to = "year", values_to = "edu")

pdata$key <- paste(as.character(as.vector(pdata$country)), as.numeric(as.vector(pdata$year)))

gov_data$key<- paste(as.character(as.vector(gov_data$country)), as.numeric(as.vector(gov_data$year)))
rq_data$key<- paste(as.character(as.vector(rq_data$country)), as.numeric(as.vector(rq_data$year)))
urban_data$key<- paste(as.character(as.vector(urban_data$country)), as.numeric(as.vector(urban_data$year)))
edu_data$key<- paste(as.character(as.vector(edu_data$country)), as.numeric(as.vector(edu_data$year)))

pdata$gov_eff <- gov_data$gov_eff[match(pdata$key, gov_data$key)]
pdata$rq <- rq_data$rq[match(pdata$key, rq_data$key)]
pdata$urban <- urban_data$urban[match(pdata$key, urban_data$key)]
pdata$edu <- edu_data$edu[match(pdata$key, edu_data$key)]


pdata$org_size = pdata$capacity / pdata$org_count
pdata$log_org_size = log(pdata$org_size)
pdata <- pdata%>%
  group_by(country) %>%
  mutate(
    mean_inc = mean(log_inc, na.rm = TRUE),
    mean_TGC = mean(TGC, na.rm = TRUE),
    mean_support = mean(support, na.rm = TRUE),
    mean_res  = mean(res,  na.rm = TRUE),
    mean_prc  = mean(prc,  na.rm = TRUE),
    w_res = res - mean_res,
    w_inc = log_inc - mean_inc,
    w_prc = prc - mean_prc,
    w_TGC = TGC - mean_TGC,
    mean_org_size= mean(log_org_size, na.rm = TRUE),
    log_inst = log(inst),
    mean_inst = mean(inst, na.rm = TRUE),
    mean_gov_eff = mean(gov_eff, na.rm = TRUE),
    cappc = capacity / pop,
    mean_rq = mean(rq, na.rm = TRUE),
    mean_urban = mean(urban, na.rm = TRUE),
    mean_CF = mean(CF, na.rm = TRUE),
    mean_Tech = mean(Tech, na.rm = TRUE),
    mean_edu = mean(urban, na.rm = TRUE),
    mean_inst2 = mean(inst2, na.rm = TRUE)
  )
pdata$log_org_size_c <- scale(
  pdata$log_org_size,
  center = TRUE,
  scale = FALSE
)
pdata$w_org_size_c = pdata$log_org_size_c - pdata$mean_org_size
pdata$w_support = pdata$support - pdata$mean_support
pdata$w_CF = pdata$CF - pdata$mean_CF
pdata$w_Tech = pdata$Tech - pdata$mean_Tech
pdata$edu <- as.numeric(pdata$edu)
pdata$w_edu = pdata$edu - pdata$mean_edu
pdata$w_inst = pdata$inst - pdata$mean_inst
pdata$w_inst2 = pdata$inst2 - pdata$mean_inst2



library(zoo)
str(pdata$Tech)

ols <- lm(CDI2 ~  log_org_size_c * inst2  + edu + Tech  + support + CF + log_inc , data = pdata)
summary(ols)
library(car)
vif(ols)
fe_model <- plm(CDI2 ~  log_org_size_c * inst2 + edu  + Tech + CF + support  + log_inc , data=pdata , index = c("country","year"), model="within", effect="individual")
discroll = coeftest(fe_model,  vcov = vcovSCC(fe_model, type = "HC1", maxlag = 2))
discroll
vif(fe_model)
summary(fe_model)
re_model<- plm(CDI2 ~ log_org_size_c * inst2 + Tech + edu +  CF + support + log_inc, data=pdata,index = c("country","year"), model="random", effect="individual")
summary(re_model)

bgtestbewley = bgtest(bewley)      # serial correlation
bptestbewley = bptest(bewley)      # heteroskedasticity

cd_test <- pcdtest(fe_model, test="cd")
bp_test <- bptest(fe_model)
pbg_test <- pbgtest(fe_model)
haus_test <- phtest(fe_model, re_model)
pf_test <- pFtest(fe_model, ols)

fmt <- function(x) round(x, 3)
cd_stat <- paste0(fmt(cd_test$statistic), " (p=", fmt(cd_test$p.value), ")")
bp_stat <- paste0(fmt(bp_test$statistic), " (p=", fmt(bp_test$p.value), ")")
pbg_stat <- paste0(fmt(pbg_test$statistic), " (p=", fmt(pbg_test$p.value), ")")
haus_stat<- paste0(fmt(haus_test$statistic), " (p=", fmt(haus_test$p.value), ")")
pf_stat <- paste0(fmt(pf_test$statistic), " (p=", fmt(pf_test$p.value), ")")


pdata3 = pdata[,c("CF","Tech", "inst2", "log_inc", "support", "edu", "CDI2","log_org_size_c")]

pdata3 <- as.data.frame(pdata3)

pdata3 <- as.numeric(pdata3)
stargazer(pdata3, type = "latex",
          summary = TRUE, title = "Summary Statistics ",
          out = "/home/majeed/Documents/energy community/summarystatistics.tex")


library(ggplot2)
library(tidyr)
library(corrplot)
library(spdep)

cor_matrix <- cor(pdata3, use = "pairwise.complete.obs")


print(round(cor_matrix, 3))

# Rename variables in the correlation matrix
new_names <- c(
  "Energy community diffusion",
  "Community size (log)",
  "Institutional capacity",
  "Education",
  "Technology diversity",
  "Capacity factor",
  "Income (log)",
  "Policy support"
)

colnames(cor_matrix) <- new_names
rownames(cor_matrix) <- new_names


# Convert to long format
melted_cor <- as.data.frame(cor_matrix) %>%
  tibble::rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value")


# Plot
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1,
                                   hjust = 1)) +
  labs(title = "", x = "", y = "")
melted_cor <- as.data.frame(cor_matrix)%>% tibble::rownames_to_column(var="Var1")%>% pivot_longer(-Var1, names_to = "Var2", values_to = "value")


ggplot(melted_cor, aes(x=Var1, y=Var2, fill=value))+ geom_tile(color="white")+geom_text(aes(label = round(value,1)), color ="black", size=3)+ scale_fill_gradient2(low="red", mid="white", high="blue", midpoint=0) + theme_minimal()+theme(axis.text.x=element_text(angle=30,vjust=1, hjust=1))+ labs(title="", x="",y="")


library(SparseM)
install.packages("clubSandwich")
library(clubSandwich)


mundlak_model <- plm(
  CDI2 ~ w_org_size_c +  w_inst2 + w_Tech  +  w_inc + w_edu  + w_CF + w_support + mean_org_size  + mean_Tech + mean_CF + mean_support+ mean_inst +mean_inc + mean_edu,
  data = pdata,
  index= c("country", "year"),
  model = "random",
  effect = "individual",
  random.method = "walhus"
)
mundlak = coeftest(mundlak_model, vcov. = vcovSCC(mundlak_model, type = "HC1", maxlag = 2))
mundlak
vif(mundlak_model)
summary(mundlak_model)
purtest(pdata$CSI, test = "ips", exo = "trend", lags = "AIC")
library(pdR)
se_fe <- discroll[, 2]
p_fe  <- discroll[, 4]
se_mundlak <- mundlak[, 2]
p_mundlak  <- mundlak[, 4]

vif(mundlak_model)
vif(fe_model)

stargazer(
  fe_model, mundlak_model,
  type = "latex",
  title = "Panel Data Models: Fixed Effects, Mundlak",
  #label = "tab:panel_models",
  column.labels = c("Fixed Effects",  "Mundlak"),
  # Driscoll–Kraay standard errors
  se = list(se_fe, se_mundlak),
  p = list(p_fe, p_mundlak),
  #star.cutoffs = c(0.1, 0.05, 0.01),
  keep.stat = c("n", "rsq"),
  digits = 3,
  add.lines = list(
    c("Pesaran CD statistic:", cd_stat),
    c("Breusch–Pagan test", bp_stat),
    c("Baltagi–Wu (PBG) test", pbg_stat),
    c("Hausman test (FE vs RE)", haus_stat),
    c("F-test (FE vs pooled)", pf_stat)
  ),
  notes = c(
    "Diagnostic tests are reported for the baseline FE model.",
    "All linear panel models control for unobserved provincial heterogeneity.",
    "Driscoll–Kraay standard errors are reported in parentheses.",
    "Pesaran CD – cross-sectional dependence.",
    "Breusch–Pagan – heteroskedasticity.",
    "PBG – serial correlation.",
    "Hausman – RE consistency (p≈0 ⇒ FE preferred).",
    "F-test – panel effects vs pooled OLS."
  ),
  out = "/home/majeed/Documents/solar generation equity/statisticalModels1.tex"
)

library(sandwich)
library(lmtest)
library(margins)


write.csv(pdata, "pdata.csv")

print(csidata, n=Inf)
lqdata <- pdata[pdata$year==2023,c("country","lq")]
csidata <- pdata[pdata$year %in% c(208,2013,2018,2023),c("country","year","CDI3")]

c(country_mean,lqdata)
library(ggplot2)
pdata$CDI3 = (pdata$CDI2-min(pdata$CDI2))/(max(pdata$CDI2)-min(pdata$CDI2))
country_mean <- aggregate(CDI3 ~ country, pdata, mean)
lq_mean <- aggregate(lq ~ country, pdata, mean)
cor(as.numeric(country_mean$CDI3),as.numeric(lq_mean$lq))
ggplot(country_mean, aes(x=reorder(country, CDI3), y=CDI3)) +
  geom_bar(stat="identity", fill = "#0072B2", alpha = 0.70, width = 0.75) +
  coord_flip() +
  labs(x="Country", y="Average normalized CDI",
       title="") +
  theme_minimal()

year_mean <- aggregate(CSI ~ year, pdata, mean)




ggplot(mydata_sorted,
       aes(x = reorder(province, -solar_perfect + SG))) +

  # CES benchmark
  geom_col(aes(y = solar_perfect),
           fill = "#D55E5E", alpha = 0.70, width = 0.75) +

  # Actual generation
  geom_col(aes(y = SG),
           fill = "#0072B2", alpha = 0.70, width = 0.75) +

  coord_flip() +

  labs(
    title = "",
    x = "Province",
    y = "Solar Capacity (MW)"
  ) +

  theme_minimal(base_size = 20) +

  theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold", color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(size = 9, color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    plot.margin = margin(10, 15, 10, 15)
  )
# ---------------------------


library(xgboost)
cleand_data <- pdata[c("log_org_size_c","CDI2", "log_inc","edu", "CF","support", "Tech","inst2")]
cleand_data <- na.omit(cleand_data)
X <- model.matrix(CDI2 ~ log_org_size_c + log_inc + edu + support + inst2 + Tech + CF -1, data = cleand_data)
X <- as.matrix(X)
storage.mode(X) <- "double"
X <- data.matrix(data.frame(
  log_org_size = cleand_data$log_org_size_c,
  log_inc = cleand_data$log_inc,
  edu = cleand_data$edu,
  support = cleand_data$support,
  Tech = cleand_data$Tech,
  CF = cleand_data$CF,
  inst2 = cleand_data$inst2
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
str(X_train)
shap_values <- predict( model, X, predcontrib = TRUE)
shap_df <- as.data.frame(shap_values)
shap_df <- shap_df[, - ncol(shap_df)]
importance2 <- colMeans(abs(shap_df))
sort(importance2, decreasing = TRUE)
shap_values <- shap.values(xgb_model = model, X_train = X)
shap.plot.summary(shap_values$shap_score, X_df)
barplot(importance2)
model$feature_names

X_df <- as.data.frame(X)
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

