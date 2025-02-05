

library(readxl)                # Lecture de fichiers Excel
library(dplyr)
library(gridExtra)
library(ggplot2)

#Chargement des données et préparation
# Lecture des fichiers Excel
#replace "..." by your filepath
freq <- read_excel(".../freMTPLfreq.xlsx")
sev <- read_excel(".../freMTPLsev.xlsx")
valorisation <- read_excel(".../freMTPLsev.xlsx", sheet = 2)
merged<-merge(freq,sev,by="PolicyID")
summary(merged)
head(merged)
#Traitement de la base de données : transformer la variable Policy en nombre
freq$PolicyID<- as.numeric(freq$PolicyID)
colnames(valorisation)[1:3] <- c("année", "p", "indice")

#visualisation des données 
summary(freq)
head(freq)
summary(sev)

#ajustement des primes à l'exposition, et calcul de la fréquence de sinistre par contrat 
freq$frequency<-freq$ClaimNb/freq$Exposure
sev$ClaimAmount_Exposition<-merged$ClaimAmount/merged$Exposure
freq$Prime_Ajustee <- freq$prime / freq$Exposure

# Calculer la fréquence
freq$Frequency <- freq$ClaimNb / freq$Exposure
# Calcul de la fréquence moyenne pondérée par l'exposition
average_frequency <- weighted.mean(freq$Frequency, freq$Exposure)
cat("Average Frequency =", average_frequency, "\n")
# Calcul de la fraction de l'exposition avec zéro sinistre
fraction_zero_claims <- sum(freq$Exposure[freq$ClaimNb == 0]) / sum(freq$Exposure)
cat("Fraction of exposure with zero claims =", sprintf("%.1f%%", fraction_zero_claims * 100), "\n")


# Histogramme du nombre de sinistres
p1 <- ggplot(freq, aes(x = ClaimNb)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  scale_y_log10() +
  ggtitle("Number of claims") +
  theme_minimal()
# Histogramme de l'exposition
p2 <- ggplot(freq, aes(x = Exposure)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  scale_y_log10() +
  ggtitle("Exposure in years") +
  theme_minimal()
# Histogramme de la fréquence (sinistres par année)
p3 <- ggplot(freq, aes(x = frequency)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  scale_y_log10() +
  ggtitle("Frequency (number of claims per year)") +
  theme_minimal()
# Afficher les trois graphiques côte à côte
grid.arrange(p1, p2, p3, ncol = 3)
#on visualise les caractéristiques des sinistres rencontrés, l'exposition
#réelle au risque afin d'établir un profil de ce àquoi s'attendre 


#calcul de la prime pure avec les valeurs ajustées à l'exposition

sum_claimamount_aj<-sum(sev$ClaimAmount_Exposition)
sum_claimamount<-sum(sev$ClaimAmount)
Policy_number<-length(freq$PolicyID)
Prime_pure <- sum_claimamount_aj/Policy_number
#par exemple si l'on calculait sans cet ajustement, on aurait une prime pure 
#équivalente à une couverure de : 
print(paste("équivalent jour/ an :", 365*mean(freq$Exposure)))

Prime_pure_nonaj<-sum_claimamount/Policy_number
print(paste("prime pure non ajustée :",Prime_pure_nonaj,"Prime pure ajustée:", Prime_pure))

#calcul Ratio S/P, pas besoin d'ajuster à 'la fréquence'exposition, car les 
#sinistres et les primes doivent être comparés sur la même durée 
sum_prime<-sum(freq$prime)
Ratio_SP<-sum_claimamount/sum_prime
print(paste("Ratio S/P :", Ratio_SP))

# Graphique : Évolution des sinistres 
ggplot(sev, aes(x = année, y = ClaimAmount_Exposition)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Évolution des sinistres au fil des ans", x = "Année", y = "Coût") +
  theme_minimal()


prime_moyenne<-mean(freq$prime)
prime_moyenne
primeaj_moyenne<-mean(freq$Prime_Ajustee)
primeaj_moyenne

#Construction des statistiques de sinistres et de primes (assiettes) ‘as if ’ 2025.

sev$indice <- NA
# Boucle pour parcourir chaque ligne de sev
for (i in 1:nrow(sev)) {
  # Extraire l'année de la ligne courante
  n <- sev$année[i]
  
  # Chercher l'indice correspondant dans le dataframe valorisation
  indice_correspondant <- valorisation$indice[valorisation$année == n]
  # Si un indice est trouvé, l'ajouter à la colonne indice
  if (length(indice_correspondant) > 0) {
    sev$indice[i] <- indice_correspondant
  }
}
#on revalorise les sinistres
sev$asif<-sev$indice*sev$ClaimAmount_Exposition
summary(sev)

#Déterminez le taux de réassurance (taux de la prime pure) par la méthode 
#‘Burning Cost’ 2025, si la cédante est réassurée par un traité 4,3 XS 1,3 
#(excédant de sinistres en M€) par an

# Agréger les sinistres par année dans sev
aggregation <- data.frame(
  année = integer(),
  asif = numeric(),
  part_reassureur = numeric(),
  burning_cost = numeric()
)
aggregation <- aggregate(asif ~ année, data = sev, sum)

#calcul de la charge agrégée de la tranche "as if" avec portéeXpriorité de 4,3XS1,3 
portée<-4300000
priorité<-1300000
aggregation$part_reassureur <- pmin(pmax(aggregation$asif - priorité, 0), portée)

#ajout du de l’estimateur du taux de prime de réassurance par la méthode du 
#Burning cost 
aggregation$burning_cost<-aggregation$part_reassureur/aggregation$asif
aggregation$Prime_pure_reassurance=Prime_pure*aggregation$burning_cost

library(scales)

# Graphique 1 : Évolution de asif et part_reassureur
ggplot(aggregation, aes(x = année)) +
  geom_line(aes(y = asif, color = "Asif"), size = 1) +
  geom_line(aes(y = part_reassureur, color = "Part Réassureur"), size = 1.2) +
  labs(title = "Évolution de Asif et Part Réassureur au fil des ans", x = "Année", y = "Montant (€)") +
  scale_color_manual(values = c("Asif" = "blue", "Part Réassureur" = "red")) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M", accuracy = 1)) +
  expand_limits(y = 0) +
  theme_minimal()

# Graphique 2 : Évolution du Burning Cost
ggplot(aggregation, aes(x = année, y = burning_cost)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  labs(title = "Évolution du Burning Cost au fil des ans", x = "Année", y = "Burning Cost") +
  theme_minimal()

# Graphique 3 : Évolution de la Prime Pure de Réassurance
ggplot(aggregation, aes(x = année, y = Prime_pure_reassurance)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "green", size = 2) +
  labs(title = "Évolution de la Prime Pure de Réassurance au fil des ans", x = "Année", y = "Prime Pure (€)") +
  theme_minimal()



#Partie 2 
# Modèle GLM pour la fréquence des sinistres
glm_frequence <- glm(
  ClaimNb ~ DriverAge + Region + Brand ,
  family = poisson(link = "log"),
  data = freq
)
# Résumé du modèle
summary(glm_frequence)

# Supprimer les lignes avec montant_sinistre = 0 pour le modèle de sévérité
summary(freq$ClaimNb)
freq <- freq[freq$ClaimNb > 0, ]

# Modèle GLM pour la sévérité des sinistres
glm_severite <- glm(
  ClaimNb ~ DriverAge + Region + Brand ,
  family = Gamma(link = "log"),
  data = freq
)

# Résumé du modèle
summary(glm_severite)


######## Mise à jour ##########
#     Créarion du profil      #
###############################

#====== Conversion en Facteurs =====#
#####################################

freq$Region <- as.factor(freq$Region)
freq$Brand <- as.factor(freq$Brand)

levels(freq$Region)
levels(freq$Brand)

##### Profil ####
#################

profil <- data.frame(
  DriverAge = 45,
  Region = factor("Ile-de-France", levels = levels(freq$Region)),
  Brand = factor("Renault, Nissan or Citroen", levels = levels(freq$Brand))
)

frequence_predite <- predict(glm_frequence, newdata = profil, type = "response")
severite_predite <- predict(glm_severite, newdata = profil, type = "response")
tarif_assurance <- frequence_predite * severite_predite
print(paste("Tarif estimé pour le profil :", round(tarif_assurance, 2), "€"))

print(frequence_predite)
print(severite_predite)

# Diagnostic du modèle de fréquence
plot(glm_frequence)
# Diagnostic du modèle de sévérité
plot(glm_severite)


####== Densité sévérité ===###
##############################
##############################

# Charger ggplot2
library(ggplot2)

# Supposons que sev est votre DataFrame contenant les sinistres
# Vérifier les montants de sinistres
summary(sev$ClaimAmount)

# Supprimer les valeurs nulles ou négatives si elles existent
sev <- sev[sev$ClaimAmount > 0, ]

# Histogramme de la distribution de la sévérité des sinistres
ggplot(sev, aes(x = ClaimAmount)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  scale_x_log10() +  # Transformation logarithmique pour mieux visualiser les valeurs élevées
  ggtitle("Distribution de la sévérité des sinistres") +
  xlab("Montant des sinistres (log10)") +
  ylab("Fréquence") +
  theme_minimal()

# Ajouter un graphique de densité
ggplot(sev, aes(x = ClaimAmount)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  scale_x_log10() +
  ggtitle("Densité de la sévérité des sinistres") +
  xlab("Montant des sinistres (log10)") +
  ylab("Densité") +
  theme_minimal()

