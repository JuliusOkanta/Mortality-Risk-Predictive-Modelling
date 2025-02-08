library(tidyverse)
library(ggplot2)
library(tidyverse)
library(kableExtra)
library(readxl)
library(ggplot2)
library(dplyr)
library(viridis)
library(gganimate)
library(hrbrthemes)
library(gifski)
library(av)
library(ggrepel)
library(scales)
library(RColorBrewer)
install.packages("ggcorrplot")
install.packages("hrbrthemes")
install.packages('gganimate')
install.packages("viridis")

install.packages("gifski")
install.packages("av")
install.packages("scales")

install.packages("ggrepel")
install.packages("RColorBrewer")

install.packages("caret")
library(caret)
install.packages("glmnet")
library(glmnet)
install.packages("fastDummies")
library(fastDummies)

install.packages("pscl")
library(pscl)

install.packages("car")
library(car)

# Step 1: Load the dataset without column names
file_path <- "C:/Users/evapa/Documents/MASTER/EHR/A3/alldata.csv"
raw_data <- read_csv(file_path, col_names = FALSE)

# Step 2: Replace placeholders with NA
cleaned_data[cleaned_data == ":"] <- NA

# Step 3: Set the first row as column headers
colnames(raw_data) <- raw_data[1, ]
cleaned_data <- raw_data[-1, ]  # Remove the first row after setting it as headers
cleaned_data %>% head(10)

# Step 4: Exploratory data analysis
### Check structure and summary
glimpse(cleaned_data)
column_names <- colnames(cleaned_data)
print(column_names)
### Remove columns with more than 10% missing values
cleaned_data <- cleaned_data %>%
  select(where(~ mean(is.na(.)) <= 0.1))
# Convert outcome variable to factor
cleaned_data <- cleaned_data %>%
  mutate(DEATH_HOSP = as.factor(DEATH_HOSP), GENDER=as.factor(GENDER), 
         ETHNICITY=as.factor(ETHNICITY),DM2=as.factor(DM2),CAD=as.factor(CAD),
         CKD=as.factor(CKD),HYPERTENSION=as.factor(HYPERTENSION),
         COPD=as.factor(COPD),AMI=as.factor(AMI),HepF=as.factor(HepF))

# Convert numeric predictors to numeric
numeric_columns <- c("AGE_FIRST", "AGE_LAST", "AVG_LOS", "AVG_ICULOS_days", 
                     "HEARTRATE_avg", "SBP_avg", "DBP_avg", "aniongap_min", "aniongap_max","creatinine_min", "creatinine_max",
                     "glucose_min", "glucose_max","bicarbonate_min","bicarbonate_max",
                     "creatinine_min","creatinine_max",
                     "chloride_min", "chloride_max", "glucose_min", "glucose_max",
                     "hematocrit_min","hematocrit_max","hemoglobin_min","hemoglobin_max",    
                     "platelet_min","platelet_max","potassium_min","potassium_max","ptt_min","ptt_max","inr_min"        
                     ,"inr_max", "pt_min","pt_max","sodium_min","sodium_max","bun_min","bun_max"        
                     ,"wbc_min","wbc_max")
cleaned_data <- cleaned_data %>%
  mutate(across(all_of(numeric_columns), as.numeric))
summary(cleaned_data)
### Check for missing values
sapply(cleaned_data, function(x) sum(is.na(x)))
### Check the distribution of outcome variable
table(cleaned_data$DEATH_HOSP)

### Univariate Analysis of Predictors --- Analyze the relationship between each predictor and the outcome variable.
# Log transform the numeric variables
log_cleaned_data <- cleaned_data %>%
  mutate(across(where(is.numeric), ~ log(. + 1), .names = "log_{.col}"))
glimpse(log_cleaned_data)

# Chi-square tests
chisq.test(table(cleaned_data$GENDER, cleaned_data$DEATH_HOSP))
chisq.test(table(cleaned_data$ETHNICITY, cleaned_data$DEATH_HOSP))
chisq.test(table(cleaned_data$DM2, cleaned_data$DEATH_HOSP))
chisq.test(table(cleaned_data$CAD, cleaned_data$DEATH_HOSP))
chisq.test(table(cleaned_data$CKD, cleaned_data$DEATH_HOSP))
chisq.test(table(cleaned_data$HYPERTENSION, cleaned_data$DEATH_HOSP))
chisq.test(table(cleaned_data$COPD, cleaned_data$DEATH_HOSP))
chisq.test(table(cleaned_data$AMI, cleaned_data$DEATH_HOSP))
chisq.test(table(cleaned_data$HepF, cleaned_data$DEATH_HOSP))

# Convert outcome variable to numeric (binary: 0/1)
#cleaned_data$DEATH_HOSP <- as.numeric(as.character(cleaned_data$DEATH_HOSP))
num_cleaned_data <- log_cleaned_data %>%
  dummy_cols(select_columns = c("GENDER", "ETHNICITY"), remove_selected_columns = TRUE) %>% select(-ICU_STAYS)
glimpse(num_cleaned_data)

# Convert all factors to numeric
num_cleaned_data <- num_cleaned_data %>%
  mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
glimpse(num_cleaned_data)

single_level_columns <- sapply(num_cleaned_data, function(x) length(unique(x)) <= 1)
print(names(num_cleaned_data)[single_level_columns])

model_cleaned_data <- num_cleaned_data %>%
  select(SUBJECT_ID, DEATH_HOSP, DM2, CAD, CKD, HYPERTENSION, COPD, AMI, HepF,
         ETHNICITY_WHITE,GENDER_M,
         log_AGE_FIRST, log_AGE_LAST, log_AVG_LOS, log_AVG_ICULOS_days,
         log_HEARTRATE_avg, log_SBP_avg, log_DBP_avg,  log_aniongap_min, log_aniongap_max,
         log_creatinine_min, log_creatinine_max, log_glucose_min, log_glucose_max,
         log_bicarbonate_min,log_bicarbonate_max, log_creatinine_min,log_creatinine_max,
         log_chloride_min, log_chloride_max, log_glucose_min, log_glucose_max,
         log_hematocrit_min,log_hematocrit_max,log_hemoglobin_min,log_hemoglobin_max,
         log_platelet_min,log_platelet_max, log_potassium_min, log_potassium_max,
         log_ptt_min, log_ptt_max, log_inr_min, log_inr_max, log_pt_min, log_pt_max,
         log_sodium_min, log_sodium_max, log_bun_min,log_bun_max,log_wbc_min,log_wbc_max )
model_cleaned_data %>% head(10)

### ------------- Split data into training and test sets

# Set seed for reproducibility
set.seed(123)

# Split into training (70%) and test (30%)
train_index <- createDataPartition(model_cleaned_data$DEATH_HOSP, p = 0.7, list = FALSE)
train_data <- model_cleaned_data[train_index, ]
test_data <- model_cleaned_data[-train_index, ]

glimpse(train_data)

### ------------- Select variables based on correlation matrix (selected predictors r > 0.15)

TEST <- train_data %>% select(-SUBJECT_ID)

# Calculate correlation
cor_matrix <- cor(TEST, use = "complete.obs")

corr_train_data <-train_data %>%
  select(SUBJECT_ID,DEATH_HOSP, log_bun_min,log_bun_max,log_aniongap_min, log_aniongap_max, 
         log_creatinine_min, log_creatinine_max, log_AVG_ICULOS_days,
         log_HEARTRATE_avg,log_inr_max,log_SBP_avg, log_DBP_avg)
corr_train_data <- corr_train_data %>% select (-SUBJECT_ID) %>% head(10)

# Calculate correlation matrix
cor_matrix <- cor(corr_train_data, use = "complete.obs")

# Adjust plot size using ggsave or setting dimensions
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE) + 
  theme(plot.margin = margin(10, 10, 10, 10),  # Add extra margins if needed
        text = element_text(size = 10))       # Increase text size

corr_train_data <-train_data %>%
  select(SUBJECT_ID,DEATH_HOSP,log_bun_min,log_aniongap_max, 
         log_AVG_ICULOS_days, log_HEARTRATE_avg,log_inr_max,log_SBP_avg, log_DBP_avg)
corr_selected_data<-corr_train_data
corr_train_data <- corr_train_data %>% select (-SUBJECT_ID, -DEATH_HOSP) 
corr_train_data %>% head(10)
cor_matrix <- cor(corr_train_data, use = "complete.obs")
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE) + 
  theme(plot.margin = margin(10, 10, 10, 10),  # Add extra margins if needed
        text = element_text(size = 10))       # Increase text size
ggsave("c_plot_corr_data.png", width = 12, height = 12, dpi = 600)  # Save as larger file

### ----------- Select variables based on LASSO regression
# Check for missing values in the dataset used to create x and y
missing_rows <- rowSums(is.na(train_data))
table(missing_rows > 0)  # Count rows with missing values

# Remove rows with missing values
train_data <- train_data %>%
  filter(complete.cases(.))

# Recreate x and y after filtering
x <- model.matrix(DEATH_HOSP ~ . - SUBJECT_ID, data = train_data)[, -1]
y <- train_data$DEATH_HOSP

# Fit LASSO with cross-validation
set.seed(123)  # For reproducibility
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")  # LASSO (alpha = 1)

# Plot the cross-validation curve
plot(lasso_model)

# Optimal lambda
optimal_lambda <- lasso_model$lambda.min

# Coefficients at optimal lambda
lasso_coefficients <- coef(lasso_model, s = optimal_lambda)

# Extract selected predictors
selected_predictors <- rownames(lasso_coefficients)[lasso_coefficients[, 1] != 0]
selected_predictors <- selected_predictors[selected_predictors != "(Intercept)"]  # Remove intercept
print(selected_predictors)

### -------------------------------------------
# Subset cleaned_data using selected_predictors
lasso_selected_data <- train_data %>%
  select(SUBJECT_ID, DEATH_HOSP, all_of(selected_predictors)) 
# Ensure data contains only numeric variables
numeric_data <- lasso_selected_data %>% select(where(is.numeric))

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Adjust plot size using ggsave or setting dimensions
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower", 
           lab = TRUE) + 
  theme(plot.margin = margin(10, 10, 10, 10),  # Add extra margins if needed
        text = element_text(size = 10))       # Increase text size

# Save as larger file
ggsave("correlation_plot_.png", width = 12, height = 12, dpi = 600)

glimpse(selected_data)

### Run logistic regression model (corr predictors) - TRAIN DATA

df <-corr_selected_data

# Fit logistic regression model
corr_logistic_model_train <- glm(DEATH_HOSP ~ log_bun_min + log_aniongap_max + log_AVG_ICULOS_days + 
                      log_HEARTRATE_avg + log_inr_max + log_SBP_avg + log_DBP_avg, 
                      data = df, 
                      family = "binomial")

# Summary of the model
summary(corr_logistic_model_train)

# Calculate Nagelkerke's R^2
nagelkerke_r2 <- pR2(corr_logistic_model_train)
print(nagelkerke_r2)

# Calculate VIF
vif_values <- vif(corr_logistic_model_train)

# Print VIF values
print(vif_values)
 

### Run logistic regression model (lasso predictors) - TRAIN DATA

df <-lasso_selected_data
# Fit logistic regression model
lasso_logistic_model_train <- glm(DEATH_HOSP ~ AMI+ HepF+ ETHNICITY_WHITE+GENDER_M+log_AGE_LAST+
                        log_AVG_LOS+ log_AVG_ICULOS_days+
                        log_HEARTRATE_avg+ log_SBP_avg+
                        log_DBP_avg+log_aniongap_max+
                        log_creatinine_min+ log_glucose_max+
                        log_creatinine_min+ log_glucose_max+
                        log_platelet_min+log_potassium_min+ 
                        log_ptt_max+ log_inr_max+ 
                        log_sodium_min+ log_bun_min+log_wbc_max, 
                      data = df, 
                      family = "binomial")

# Summary of the model
summary(lasso_logistic_model_train)

# Calculate Nagelkerke's R^2
nagelkerke_r2 <- pR2(lasso_logistic_model_train)
print(nagelkerke_r2)

# Calculate VIF
vif_values <- vif(lasso_logistic_model_train)

# Print VIF values
print(vif_values)
# 
# ### Run logistic regression model (corr predictors) - TEST DATA
# 
# df <-corr_selected_data
# 
# # Fit logistic regression model
# corr_logistic_model_train <- glm(DEATH_HOSP ~ log_bun_min + log_aniongap_max + log_AVG_ICULOS_days + 
#                                    log_HEARTRATE_avg + log_inr_max + log_SBP_avg + log_DBP_avg, 
#                                  data = df, 
#                                  family = "binomial")
# 
# # Summary of the model
# summary(corr_logistic_model_train)
# 
# # Calculate Nagelkerke's R^2
# nagelkerke_r2 <- pR2(corr_logistic_model_train)
# print(nagelkerke_r2)
# 
# # Calculate VIF
# vif_values <- vif(corr_logistic_model_train)
# 
# # Print VIF values
# print(vif_values)


### Run logistic regression model (lasso predictors) - TEST DATA

# Subset cleaned_data using selected_predictors
lasso_selected_data_test <- test_data %>%
  select(SUBJECT_ID, DEATH_HOSP, all_of(selected_predictors)) 

df <-lasso_selected_data_test
# Fit logistic regression model
lasso_logistic_model_test <- glm(DEATH_HOSP ~ AMI+ HepF+ ETHNICITY_WHITE+GENDER_M+log_AGE_LAST+
                                    log_AVG_LOS+ log_AVG_ICULOS_days+
                                    log_HEARTRATE_avg+ log_SBP_avg+
                                    log_DBP_avg+log_aniongap_max+
                                    log_creatinine_min+ log_glucose_max+
                                    log_creatinine_min+ log_glucose_max+
                                    log_platelet_min+log_potassium_min+ 
                                    log_ptt_max+ log_inr_max+ 
                                    log_sodium_min+ log_bun_min+log_wbc_max, 
                                  data = df, 
                                  family = "binomial")

# Summary of the model
summary(lasso_logistic_model_test)

# Calculate Nagelkerke's R^2
nagelkerke_r2 <- pR2(lasso_logistic_model_test)
print(nagelkerke_r2)

# Calculate VIF
vif_values <- vif(lasso_logistic_model_test)

# Print VIF values
print(vif_values)

