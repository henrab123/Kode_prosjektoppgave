# Load libraries
library(ggplot2)
library(dplyr)

# Adding the estimates of gamma to the data.frames:
KLAGE_V22$gamma <- estimated_parameters_V22_m1$gamma[,"Estimate"]
KLAGE_V23$gamma <- estimated_parameters_V23_m1$gamma[,"Estimate"]

KLAGE_V22$s <- estimated_parameters_V22_m1$s[,"Estimate"][as.numeric(factor(KLAGE_V22$kommisjon))]
KLAGE_V23$s <- estimated_parameters_V23_m1$s[,"Estimate"][as.numeric(factor(KLAGE_V23$kommisjon))]

#########################################
### LOGISTISK REG ~ gamma, FinalGrade ###
#########################################

model_V22 <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V22, family = binomial)
summary(model_V22)

model_V22_utenA <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA)

model_V22_utenA_medstuff <- glm(klagde ~ gamma + finalGradeLabel + gamma*finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA_medstuff)


#####################################
### LOGISTISK REG ~ s, FinalGrade ###
#####################################

model_V22_utenA_Sbare <- glm(klagde ~ s, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA_Sbare)

model_V22_utenA_S <- glm(klagde ~ s + finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA_S)

model_V22_utenA_medstuff_S <- glm(klagde ~ s + finalGradeLabel + s*finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA_medstuff_S)

#####################################
### LOGISTISK REG ~ s, FinalGrade ###
#####################################

# Full model
full_model <- glm(klagde ~ (s + gamma + finalGradeLabel + totalScore)^2, data = KLAGE_V22_UtenA, family = binomial)

# Stepwise selection
best_model <- step(full_model)

Nesten_optimal_modell <- glm(klagde ~ s + gamma + finalGradeLabel + totalScore, data = KLAGE_V22_UtenA, family = binomial)
summary(Nesten_optimal_modell)

Optimal_modell <- glm(klagde ~ gamma + finalGradeLabel + totalScore, data = KLAGE_V22_UtenA, family = binomial)
summary(Optimal_modell)

Optimal_modellA <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(Optimal_modellA)

Optimal_modellB <- glm(klagde ~ finalGradeLabel + totalScore, data = KLAGE_V22_UtenA, family = binomial)
summary(Optimal_modellB)

KLAGE_V22$predicted <- predict(model, type = "response")




KLAGE_V22_UtenA <- KLAGE_V22 %>% filter(finalGradeLabel != "A")
model_V22_utenA <- glm(klagde ~ gamma + finalGradeLabel, data = KLAGE_V22_UtenA, family = binomial)
summary(model_V22_utenA)

# Plot logistic regression curve
ggplot(KLAGE_V22_UtenA, aes(x = gamma, y = klagde)) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.5) +  # Scatter points
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +  # Logistic curve
  labs(title = "Probability of Klage as a function of Gamma",
       x = "Gamma", y = "Probability of Klage") +
  theme_minimal()


KLAGE_V22_A <- KLAGE_V22 %>% filter(finalGradeLabel == "A")
KLAGE_V22_B <- KLAGE_V22 %>% filter(finalGradeLabel == "B")
KLAGE_V22_C <- KLAGE_V22 %>% filter(finalGradeLabel == "C")
KLAGE_V22_D <- KLAGE_V22 %>% filter(finalGradeLabel == "D")
KLAGE_V22_E <- KLAGE_V22 %>% filter(finalGradeLabel == "E")
KLAGE_V22_F <- KLAGE_V22 %>% filter(finalGradeLabel == "F")

model_V22_A <- glm(klagde ~ gamma, data = KLAGE_V22_A, family = binomial)
model_V22_B <- glm(klagde ~ gamma, data = KLAGE_V22_B, family = binomial)
model_V22_C <- glm(klagde ~ gamma, data = KLAGE_V22_C, family = binomial)
model_V22_D <- glm(klagde ~ gamma, data = KLAGE_V22_D, family = binomial)
model_V22_E <- glm(klagde ~ gamma, data = KLAGE_V22_E, family = binomial)
model_V22_F <- glm(klagde ~ gamma, data = KLAGE_V22_F, family = binomial)

summary(model_V22_A)
summary(model_V22_B)
summary(model_V22_C)
summary(model_V22_D)
summary(model_V22_E)
summary(model_V22_F)


KLAGE_V23_A <- KLAGE_V23 %>% filter(finalGradeLabel == "A")
KLAGE_V23_B <- KLAGE_V23 %>% filter(finalGradeLabel == "B")
KLAGE_V23_C <- KLAGE_V23 %>% filter(finalGradeLabel == "C")
KLAGE_V23_D <- KLAGE_V23 %>% filter(finalGradeLabel == "D")
KLAGE_V23_E <- KLAGE_V23 %>% filter(finalGradeLabel == "E")
KLAGE_V23_F <- KLAGE_V23 %>% filter(finalGradeLabel == "F")

model_V23_A <- glm(klagde ~ gamma, data = KLAGE_V23_A, family = binomial)
model_V23_B <- glm(klagde ~ gamma, data = KLAGE_V23_B, family = binomial)
model_V23_C <- glm(klagde ~ gamma, data = KLAGE_V23_C, family = binomial)
model_V23_D <- glm(klagde ~ gamma, data = KLAGE_V23_D, family = binomial)
model_V23_E <- glm(klagde ~ gamma, data = KLAGE_V23_E, family = binomial)
model_V23_F <- glm(klagde ~ gamma, data = KLAGE_V23_F, family = binomial)

summary(model_V23_A)
summary(model_V23_B)
summary(model_V23_C)
summary(model_V23_D)
summary(model_V23_E)
summary(model_V23_F)




# Fit logistic regression model for each grade
models_by_grade <- KLAGE_V22 %>%
  group_by(finalGradeLabel) %>%
  do(model = glm(klagde ~ gamma, data = ., family = binomial)) 

# View the models (coefficients, etc.)
models_by_grade$model

# Plot logistic regression curve for each grade
ggplot(KLAGE_V22, aes(x = gamma, y = klagde, color = finalGradeLabel)) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.5) +  # Scatter points
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +  # Logistic curve
  facet_wrap(~ finalGradeLabel) +  # Facet by grade
  labs(title = "Probability of Klage as a function of gamma for Each Grade",
       x = "gamma", y = "Probability of Klage") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend as it's redundant here




#############
### GAMMA ###
#############

ggplot(KLAGE_V22, aes(x = gamma, y = as.factor(klagde), color = finalGradeLabel)) + 
  geom_histogram(aes(x = gamma, y = ..density..), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of Gamma", 
       x = "Gamma", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

ggplot(KLAGE_V22, aes(x = gamma, y = as.factor(klagde), color = as.factor(kommisjon))) + 
  geom_histogram(aes(x = gamma, y = ..density..), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of Gamma", 
       x = "Gamma", y = "Klagde (0 = No, 1 = Yes)", color = "Kommisjon") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

#############
###   S   ###
#############

ggplot(KLAGE_V22, aes(x = s, y = as.factor(klagde), color = finalGradeLabel)) + 
  # Histogram of s (scaled density if needed)
  geom_histogram(aes(x = s, y = after_stat(density) * 0.01), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of s", 
       x = "s", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

################
### TOTSCORE ###
################

ggplot(KLAGE_V22, aes(x = totalScore, y = as.factor(klagde), color = finalGradeLabel)) + 
  # Transparent histogram of gamma
  geom_histogram(aes(x = totalScore, y = ..density..*15), bins = 30, fill = "gray", alpha = 0.3, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of TotalScore", 
       x = "TotalScore", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )


KLAGE_V22_utenF <- KLAGE_V22 %>% filter(finalGradeLabel != "F")
KLAGE_V23_utenF <- KLAGE_V23 %>% filter(finalGradeLabel != "F")

#############
### GAMMA ###
#############

ggplot(KLAGE_V22, aes(x = gamma, y = as.factor(klagde), color = finalGradeLabel)) + 
  geom_histogram(aes(x = gamma, y = ..density..), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of Gamma NOF", 
       x = "Gamma", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

ggplot(KLAGE_V22_utenF, aes(x = gamma, y = as.factor(klagde), color = as.factor(kommisjon))) + 
  geom_histogram(aes(x = gamma, y = ..density..), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of Gamma NOF", 
       x = "Gamma", y = "Klagde (0 = No, 1 = Yes)", color = "Kommisjon") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

#############
###   S   ###
#############

ggplot(KLAGE_V22_utenF, aes(x = s, y = as.factor(klagde), color = finalGradeLabel)) + 
  # Histogram of s (scaled density if needed)
  geom_histogram(aes(x = s, y = after_stat(density) * 0.01), bins = 30, fill = "gray", alpha = 0.2, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of s NOF", 
       x = "s", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  # High contrast without gridlines
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

################
### TOTSCORE ###
################

ggplot(KLAGE_V22_utenF, aes(x = totalScore, y = as.factor(klagde), color = finalGradeLabel)) + 
  # Transparent histogram of gamma
  geom_histogram(aes(x = totalScore, y = ..density..*15), bins = 30, fill = "gray", alpha = 0.3, inherit.aes = FALSE) + 
  # Jitter plot of klagde
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.8) +
  scale_y_discrete(labels = c("0" = "No Klage", "1" = "Klagde")) + 
  labs(title = "Klagde as a function of TotalScore NOF", 
       x = "TotalScore", y = "Klagde (0 = No, 1 = Yes)", color = "Grade") +
  theme_classic() +  
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )