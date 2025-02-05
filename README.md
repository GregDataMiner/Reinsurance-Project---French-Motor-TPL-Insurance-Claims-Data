# 📊 **Insurance Data Analysis Project**

This project performs data analysis on insurance claims using **R**. The main objectives are to load and prepare the data, calculate key metrics (e.g., frequency of claims), and visualize the results through histograms.

[The final version of the report is available here](reinsurance_project.pdf)

---
![cover](Image/cover.png)

---
### **1. Load and prepare data**

```r
# Import required libraries
library(readxl)    # Read Excel files
library(dplyr)     # Data manipulation
library(gridExtra) # Arrange multiple plots
library(ggplot2)   # Visualization

# Load Excel files
freq <- read_excel("data/freMTPLfreq.xlsx")
sev <- read_excel("data/freMTPLsev.xlsx")
valorisation <- read_excel("data/freMTPLsev.xlsx", sheet = 2)

# Merge datasets by 'PolicyID'
merged <- merge(freq, sev, by = "PolicyID")

# Convert 'PolicyID' to numeric and rename columns
freq$PolicyID <- as.numeric(freq$PolicyID)
colnames(valorisation)[1:3] <- c("year", "p", "index")
```

---

### **2. Data exploration**

```r
# Summarize and preview the data
summary(freq)
summary(sev)
summary(merged)
head(merged)
```

---

### **3. Data transformation**

```r
# Calculate claim frequency and adjust premiums based on exposure
freq$frequency <- freq$ClaimNb / freq$Exposure
sev$ClaimAmount_Exposition <- merged$ClaimAmount / merged$Exposure
freq$Prime_Ajustee <- freq$prime / freq$Exposure

# Weighted average frequency and zero-claim exposure fraction
average_frequency <- weighted.mean(freq$frequency, freq$Exposure)
cat("Average Frequency =", average_frequency, "\n")

fraction_zero_claims <- sum(freq$Exposure[freq$ClaimNb == 0]) / sum(freq$Exposure)
cat("Fraction of exposure with zero claims =", sprintf("%.1f%%", fraction_zero_claims * 100), "\n")
```
Fraction of exposure with zero claims = 95.3% 
---

### **4. Data visualization**

```r
# Plot 1: Histogram of number of claims
p1 <- ggplot(freq, aes(x = ClaimNb)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  scale_y_log10() +
  ggtitle("Number of claims") +
  theme_minimal()

# Plot 2: Histogram of exposure
p2 <- ggplot(freq, aes(x = Exposure)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  scale_y_log10() +
  ggtitle("Exposure in years") +
  theme_minimal()

# Plot 3: Histogram of claim frequency
p3 <- ggplot(freq, aes(x = frequency)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  scale_y_log10() +
  ggtitle("Frequency (number of claims per year)") +
  theme_minimal()

# Display all three plots side by side
grid.arrange(p1, p2, p3, ncol = 3)
```
![Stats](Image/Stats.png)

---
### **5. Pure premium calculation and reinsurance analysis**

This part of the project calculates the **pure premium** and **S/P ratio** while simulating reinsurance statistics using the **Burning Cost method** for the year 2025.
---
```r
# Calculate the pure premium adjusted for exposure
sum_claimamount_aj <- sum(sev$ClaimAmount_Exposition)
sum_claimamount <- sum(sev$ClaimAmount)
Policy_number <- length(freq$PolicyID)
Prime_pure <- sum_claimamount_aj / Policy_number

# Example of pure premium without adjustment
print(paste("Equivalent days/year:", 365 * mean(freq$Exposure)))
```
"équivalent jour/ an : 204.797119867607"
```
Prime_pure_nonaj <- sum_claimamount / Policy_number
print(paste("Non-adjusted pure premium:", Prime_pure_nonaj, "Adjusted pure premium:", Prime_pure))
```
[1] "prime pure non ajustée : 83.4164155587665 Prime pure ajustée: 371.140298204758"
```
# Calculate the S/P ratio without adjusting for exposure duration (claims and premiums are time-matched)
sum_prime <- sum(freq$prime)
Ratio_SP <- sum_claimamount / sum_prime
print(paste("S/P Ratio:", Ratio_SP))
```
[1] "Ratio S/P : 0.540281050694435"

---

### **6 Claims evolution visualization**

```r
# Line plot of claim evolution over the years
ggplot(sev, aes(x = année, y = ClaimAmount_Exposition)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Evolution of Claims Over Time", x = "Year", y = "Cost") +
  theme_minimal()
```

# Average premium and exposure-adjusted premium
```
prime_moyenne <- mean(freq$prime)
prime_moyenne
```
[1] 154.3945
```
primeaj_moyenne <- mean(freq$Prime_Ajustee)
primeaj_moyenne
```
[1] 275.1154

---

### **7 Claim indexing and adjustment**

```r
# Index claims for future projection (2025)
sev$indice <- NA

# Loop through each row to assign the corresponding index
for (i in 1:nrow(sev)) {
  n <- sev$année[i]  # Extract the year
  indice_correspondant <- valorisation$indice[valorisation$année == n]  # Find matching index
  
  # If an index is found, assign it
  if (length(indice_correspondant) > 0) {
    sev$indice[i] <- indice_correspondant
  }
}

# Adjust claims using the indexed values
sev$asif <- sev$indice * sev$ClaimAmount_Exposition
summary(sev)
```

---

### **8 Burning Cost reinsurance simulation**

```r
# Aggregate claims by year
aggregation <- aggregate(asif ~ année, data = sev, sum)

# Define reinsurance treaty parameters (4.3 XS 1.3 M€ per year)
coverage <- 4300000
priority <- 1300000

# Calculate the reinsurer's share of aggregated claims
aggregation$part_reassureur <- pmin(pmax(aggregation$asif - priority, 0), coverage)
```
# Estimate the reinsurance premium rate using the Burning Cost method
```
aggregation$burning_cost <- aggregation$part_reassureur / aggregation$asif
aggregation$Prime_pure_reassurance <- Prime_pure * aggregation$burning_cost
```



**Plot : Evolution of aggregated claims and reinsurer's share**

```r
# Import required library
library(scales)

# Plot aggregated claims ("asif") and reinsurer's share over time
ggplot(aggregation, aes(x = année)) +
  geom_line(aes(y = asif, color = "Asif"), size = 1) +
  geom_line(aes(y = part_reassureur, color = "Reinsurer's Share"), size = 1.2) +
  labs(title = "Evolution of Asif and Reinsurer's Share Over Time", x = "Year", y = "Amount (€)") +
  scale_color_manual(values = c("Asif" = "blue", "Reinsurer's Share" = "red")) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M", accuracy = 1)) +
  expand_limits(y = 0) +
  theme_minimal()
```
![evolution_de_asif](Image/evolution_de_asif.png)

**Plot 2: Evolution of Burning Cost**
```
# Plot Burning Cost trend over the years
ggplot(aggregation, aes(x = année, y = burning_cost)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Evolution of Burning Cost Over Time", x = "Year", y = "Burning Cost") +
  theme_minimal()
```
![burning_cost](Image/burning_cost.png)

**Plot 3: Evolution of pure reinsurance premium**
```
# Plot pure reinsurance premium trend
ggplot(aggregation, aes(x = année, y = Prime_pure_reassurance)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "green", size = 2) +
  labs(title = "Evolution of Pure Reinsurance Premium Over Time", x = "Year", y = "Pure Premium (€)") +
  theme_minimal()
```
![prime_reassurance](Image/prime_reassurance.png)


```markdown
### **7. GLM Models for Claim Frequency and Severity**

This part of the project implements **Generalized Linear Models (GLMs)** to estimate **claim frequency** and **claim severity** based on features like driver age, region, and car brand. We also create a custom profile to predict the insurance premium.

---
```
### **9 GLM for claim frequency**

```r
# GLM for claim frequency (Poisson distribution)
glm_frequence <- glm(
  ClaimNb ~ DriverAge + Region + Brand,
  family = poisson(link = "log"),
  data = freq
)

# Model summary
summary(glm_frequence)
```

---

### **10 GLM for claim severity**

```r
# Remove rows where claim amount is zero
freq <- freq[freq$ClaimNb > 0, ]

# GLM for claim severity (Gamma distribution)
glm_severite <- glm(
  ClaimNb ~ DriverAge + Region + Brand,
  family = Gamma(link = "log"),
  data = freq
)

# Model summary
summary(glm_severite)
```

---

### **11 Profile simulation for insurance pricing**

```r
# Convert relevant variables to factors
freq$Region <- as.factor(freq$Region)
freq$Brand <- as.factor(freq$Brand)

# Define a profile to predict the insurance premium
profil <- data.frame(
  DriverAge = 45,
  Region = factor("Ile-de-France", levels = levels(freq$Region)),
  Brand = factor("Renault, Nissan or Citroen", levels = levels(freq$Brand))
)

# Predict claim frequency and severity for the profile
frequence_predite <- predict(glm_frequence, newdata = profil, type = "response")
severite_predite <- predict(glm_severite, newdata = profil, type = "response")

# Calculate the estimated insurance premium
tarif_assurance <- frequence_predite * severite_predite
print(paste("Estimated premium for the profile:", round(tarif_assurance, 2), "€"))

# Print predicted values
print(frequence_predite)
print(severite_predite)
```
[1] "Tarif estimé pour le profil : 0.05 €"
> print(frequence_predite)
         1 
0.04548413 
> print(severite_predite)
       1 
1.073384 
---

### **12 Model diagnostics**

```r
# Diagnostic plots for the frequency model
plot(glm_frequence)
```
![glm_frequence1](Image/glm_frequence1.png)
![glm_frequence](Image/glm_frequence.png)
```
# Diagnostic plots for the severity model
plot(glm_severite)
```
![glm_severite](Image/glm_severite.png)


### **13 Visualizing Claim Severity Distribution**

This section generates an **histogram** and a **density plot** to analyze the distribution of claim severity. A logarithmic scale is applied to better visualize higher claim amounts.

---

### **14 Verify and clean data**

```r
# Ensure ggplot2 is loaded
library(ggplot2)

# Verify the distribution of claim amounts
summary(sev$ClaimAmount)

# Remove rows with zero or negative claim amounts
sev <- sev[sev$ClaimAmount > 0, ]
```

---

### **15 Histogram of claim severity distribution**

```r
# Plot a histogram of claim severity
ggplot(sev, aes(x = ClaimAmount)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  scale_x_log10() +  # Log transformation for better visualization of large values
  ggtitle("Distribution of Claim Severity") +
  xlab("Claim Amount (log10)") +
  ylab("Frequency") +
  theme_minimal()
```
![hist_sev](Image/hist_sev.png)

---

### **16 Density plot of claim severity**

```r
# Plot a density curve of claim severity
ggplot(sev, aes(x = ClaimAmount)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  scale_x_log10() +
  ggtitle("Density of Claim Severity") +
  xlab("Claim Amount (log10)") +
  ylab("Density") +
  theme_minimal()
```

![dens_claim](Image/dens_claim.png)

# **Conclusion**

This project successfully demonstrates the use of **data science** and **actuarial models** to analyze and predict key metrics in motor insurance claims. Through data preparation, visualization, and predictive modeling, we were able to:
- Calculate **pure premiums** and **reinsurance rates** using the **Burning Cost method**.
- Build **GLM models** to predict **claim frequency** and **claim severity** based on policyholder characteristics.
- Simulate insurance pricing for custom profiles.
- Visualize important trends in **claims**, **severity**, and **reinsurance performance** over time.

These analyses provide valuable insights for **risk assessment**, **portfolio management**, and **pricing optimization** in the insurance and reinsurance sectors.

---

#### **How to Go Further**

There are several ways to build upon this project:

1. **Model Improvement**  
   - Incorporate additional predictors such as **vehicle type**, **policy duration**, and **driver history** to improve model accuracy.
   - Experiment with advanced machine learning models (e.g., **XGBoost**, **Random Forest**) to enhance predictions.

2. **Time-Series Analysis**  
   - Apply **time-series models** (e.g., ARIMA, Prophet) to forecast future claims and premium trends.

3. **Reinsurance Optimization**  
   - Simulate alternative reinsurance structures (e.g., quota share, stop-loss) to optimize the balance between retained risk and ceded risk.

4. **Scenario Analysis**  
   - Test how different economic or regulatory scenarios might impact claims, premiums, and reinsurance strategies.

---

#### **Complete Report**

For a detailed explanation of the methodology, data, and results, refer to the **full project report**, available as a PDF file in this GitHub repository.

---

#### **Contact**

If you have any questions or suggestions, feel free to open an issue on this repository or contact the project maintainer.







