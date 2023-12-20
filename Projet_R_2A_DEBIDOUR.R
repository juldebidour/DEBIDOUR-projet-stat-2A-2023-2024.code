# Projet de modélisation statistique - Novembre-Décembre 2023 - Debidour Julien

# 1. Chargement des données et exploration initiale.

# Packages utilisés.

install.packages("lmtest")
install.packages("car")
install.packages("ggplot2")

# Chargement des librairies.

library(PCAmixdata)
library(lmtest)
library(carData)
library(ggplot2)
library(gridExtra)

# Chargement des données initiales de activation.Rdata.

donnees <- readRDS("/Users/debidour/Desktop/ENSC/PERSONNEL/2A/Mathématiques/Modélisations Statistiques/Projet_R/activation.Rdata")
donnees_femme <- subset(donnees, Sexe == "F")
donnees_homme <- subset(donnees, Sexe == "H")

# Affichage des premières lignes du jeu de données.

head(donnees)
head(donnees_femme)
head(donnees_homme)

# Dimensions du jeu de données.

dim(donnees)
dim(donnees_femme)
dim(donnees_homme)
stack(donnees)
stack(donnees_femme)
stack(donnees_homme)
tail(donnees)
tail(donnees_femme)
tail(donnees_homme)

# Sélection des variables numériques (à l'exception de Sujet et Sexe).

donnees_numeriques <- donnees[, sapply(donnees, is.numeric)]
donnees_numeriques_femme <- donnees_femme[, sapply(donnees_femme, is.numeric)]
donnees_numeriques_homme <- donnees_homme[, sapply(donnees_homme, is.numeric)]

# Détermination du nombre total de colonnes dans les données numeriques.

num_cols <- ncol(donnees_numeriques)
num_cols_femme <- ncol(donnees_numeriques_femme)
num_cols_homme <- ncol(donnees_numeriques_homme)

palette_couleurs <- rainbow(ncol(donnees_numeriques) / 3)  # Utilisation de la palette de couleurs rainbow pour les boxplots.

for (i in seq(4, num_cols, 3)) {
  indices <- i:min(i + 2, num_cols)
  boxplot(donnees_numeriques[, indices], col = palette_couleurs[seq_along(indices)], 
          main = paste("(en cm³) -", names(donnees_numeriques)[indices]))
}

par(mfrow = c(2, 2))  # Réglage de la disposition des graphiques en grille (2 lignes, 2 colonnes) pour les femmes.
for (i in seq(4, num_cols_femme, 3)) {
  indices <- i:min(i + 2, num_cols_femme)
  boxplot(donnees_numeriques_femme[, indices], main = paste("Femme -", names(donnees_numeriques_femme)[indices]))
}

par(mfrow = c(2, 2))  # Réglage de la disposition des graphiques en grille (2 lignes, 2 colonnes) pour les hommes.
for (i in seq(4, num_cols_homme, 1)) {
  indices <- i:min(i + 2, num_cols_homme)
  boxplot(donnees_numeriques_homme[, indices], main = paste("Homme -", names(donnees_numeriques_homme)[indices]), "- en cm³:")
}

# Résumé des statistiques descriptives pour les variables numériques.

summary(donnees)
summary(donnees_femme)
summary(donnees_homme)

par(mfrow = c(2, 2))  # Pour afficher les histogrammes dans une grille 2x2

# Histogrammes pour l'âge

hist(donnees_homme$Age, col = rgb(1, 0, 0, 0.5), main = "Âge - Hommes vs Femmes", xlab = "Âge (années)", ylab = "Fréquence (en %)", ylim = c(0, 40))
hist(donnees_femme$Age, col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topright", legend = c("Hommes", "Femmes"), fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

# Histogrammes pour le volume cérébral

hist(donnees_homme$Volume_Cerebral, col = rgb(1, 0, 0, 0.5), main = "Volume Cérébral - Hommes vs Femmes", xlab = "Volume Cérébral (cm³)", ylab = "Fréquence (en %)", ylim = c(0, 20))
hist(donnees_femme$Volume_Cerebral, col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topright", legend = c("Hommes", "Femmes"), fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

# Histogrammes pour l'index de Latéralisation

hist(donnees_homme$Index_Lateralisation_Hemispherique, col = rgb(1, 0, 0, 0.5), main = "Index de Latéralisation - Hommes vs Femmes", xlab = "Index de Latéralisation (U.I)", ylab = "Fréquence (en %)", ylim = c(0, 30))
hist(donnees_femme$Index_Lateralisation_Hemispherique, col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topright", legend = c("Hommes", "Femmes"), fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

# Histogrammes pour Prod_G_Frontal_Inf_Tri_1

hist(donnees_numeriques_homme[, 4], col = rgb(1, 0, 0, 0.5), main = "Taux d'activation moyen de l'aire de Broca - Hommes vs Femmes", xlab = "Taux d'activation moyen de l'aire de Broca (U.I)", ylab = "Fréquence (en %)", ylim = c(0, 25))
hist(donnees_numeriques_femme[, 4], col = rgb(0, 0, 1, 0.5), add = TRUE)
legend("topright", legend = c("Hommes", "Femmes"), fill = c(rgb(1, 0, 0, 0.5), rgb(0, 0, 1, 0.5)))

par(mfrow = c(2, 2))  # Pour afficher les boxplots dans une grille 2x2

# Boxplots des 4 variables principales : 

boxplot_age <- ggplot(donnees, aes(x = Sexe, y = Age, fill = Sexe)) +
  geom_boxplot() +
  labs(title = "Boxplot de l'âge selon le sexe", x = "Sexe", y = "Âge (années)") +
  scale_fill_manual(values = c("pink", "blue")) +
  theme_minimal()

boxplot_volume <- ggplot(donnees, aes(x = Sexe, y = Volume_Cerebral, fill = Sexe)) +
  geom_boxplot() +
  labs(title = "Boxplot du volume cérébral selon le sexe", x = "Sexe", y = "Volume Cérébral (cm³)") +
  scale_fill_manual(values = c("pink", "blue")) +
  theme_minimal()

boxplot_index <- ggplot(donnees, aes(x = Sexe, y = Index_Lateralisation_Hemispherique, fill = Sexe)) +
  geom_boxplot() +
  labs(title = "Boxplot de l'index de latéralisation selon le sexe", x = "Sexe", y = "Index de latéralisation (U.I)") +
  scale_fill_manual(values = c("pink", "blue")) +
  theme_minimal()

boxplot_activation <- ggplot(donnees, aes(x = Sexe, y = PROD_G_Frontal_Inf_Tri_1_L, fill = Sexe)) +
  geom_boxplot() +
  labs(title = "Boxplot de l'activation de l’aire de Broca gauche selon le sexe", x = "Sexe", y = "Activation de l’aire de Broca gauche (U.I)") +
  scale_fill_manual(values = c("pink", "blue")) +
  theme_minimal()

# Affichage des boxplots dans une grille 2x2
grid.arrange(boxplot_age, boxplot_volume, boxplot_index, boxplot_activation, ncol = 2)

# 2. Analyse en Composantes Principales (ACP).

# Calcul de l'ACP

resACP <- PCAmix(donnees_numeriques[,0:15]) #À lancer pour observer l'ACP.

# Essai de colorisation non abouti.

# summary(PCAmix(donnees_numeriques[,0:15]))
# split<-splitmix(donnees)
# summary(split)
# X1<-split$X.quanti
# X2<-split$X.quali
# summary(X1)
# summary(X2)
# PCAmix(X.quanti=X1,X.quali=X2)
# obj <- PCAmix(X.quanti=X1,X.quali=X2)
# plot(obj,choice="ind")# Affichage des valeurs propres.
# round(obj$eig, digits = 3)
# plot(obj,choice="cor")
# plot(obj,choice="ind",coloring.ind=X2$Sexe==F)
# plot(obj,choice="ind",coloring.ind=X2$Sexe==F,label=FALSE,
#      posleg="bottomright", main="Observations")
# plot(obj,choice="sqload",coloring.ind =X2$Sexe==F, leg=TRUE,
#      posleg="topright", main="All variables")


round(resACP$eig, digits = 3)
round(resACP$quanti$cos2, digits = 3)

# Rappel : Critère de Kayser : Eigenvalue (valeurs propres) supérieures à 1


# Graphiques de l'ACP.

par(mfrow=c(1,1))

plot(resACP,choice = "cor", main="Cercle des corrélations (dimensions 1-2) pour tous les individus")  # À lancer pour observer seulement l'ACP du cercle des corrélations.
plot(resACP,choice = "cor", main="Cercle des corrélations (dimensions 3-4) pour tous les individus", axes = c(3, 4))  # À lancer pour observer seulement l'ACP du cercle des corrélations.
plot(resACP_femme, choice = "ind", main="Graphique de la représentation des individus (dimensions 1-2) tous les individus", axes = c(1, 2)) # Graphique des individus pour tous les individus.
plot(resACP_femme, choice = "ind", main="Graphique de la représentation des individus (dimensions 3-4) tous les individus", axes = c(3, 4)) # Graphique des individus pour tous les individus.
plot(resACP_femme, choice = "sqload", main="Graphique des charges (dimensions 1-2) tous les individus", axes = c(1, 2)) # Graphique des charges pour tous les individus.
plot(resACP_femme, choice = "sqload", main="Graphique des charges (dimensions 3-4) tous les individus", axes = c(3, 4)) # Graphique des charges pour tous les individus.

# Analyse en Composantes Principales pour les femmes.

resACP_femme <- PCAmix(donnees_numeriques_femme[, 0:15]) # À lancer pour observer l'ACP pour les femmes.
round(resACP_femme$eig, digits = 3) # Affichage des valeurs propres pour les femmes.
round(resACP_femme$quanti$cos2, digits = 2) # Valeurs de cos2 pour les femmes.
plot(resACP_femme, choice = "cor", main="Cercle des corrélations (dimensions 1-2) pour les femmes") # Graphique du cercle des corrélations pour les femmes.
plot(resACP_femme, choice = "cor", axes = c(3, 4), main = "Cercle des corrélations (dimensions 3-4) pour les femmes")
plot(resACP_femme, choice = "ind", main="Graphique de la représentation des individus (dimensions 1-2) pour les femmes", axes = c(1, 2)) # Graphique des individus pour les femmes.
plot(resACP_femme, choice = "ind", main="Graphique de la représentation des individus (dimensions 3-4) pour les femmes", axes = c(3, 4)) # Graphique des individus pour les femmes.
plot(resACP_femme, choice = "sqload", main="Graphique des charges (dimensions 1-2) pour les femmes", axes = c(1, 2)) # Graphique des charges pour les femmes.
plot(resACP_femme, choice = "sqload", main="Graphique des charges (dimensions 3-4) pour les femmes", axes = c(3, 4)) # Graphique des charges pour les femmes.

# Analyse en Composantes Principales pour les hommes.

resACP_homme <- PCAmix(donnees_numeriques_homme[,0:15]) # À lancer pour observer l'ACP pour les hommes.
round(resACP_homme$quanti$cos2, digits = 2) # Valeurs de cos2 pour les hommes.
plot(resACP_homme, choice = "cor", main="Cercle des corrélations (dimensions 1-2) pour les hommes") # Graphique du cercle des corrélations pour les hommes.
plot(resACP_homme, choice = "cor", main="Cercle des corrélations (dimensions 3-4) pour les hommes", axes = c(3, 4)) # Graphique du cercle des corrélations pour les hommes.
plot(resACP_femme, choice = "ind", main="Graphique de la représentation des individus (dimensions 1-2) pour les hommes", axes = c(1, 2)) # Graphique des individus pour les hommes.
plot(resACP_femme, choice = "ind", main="Graphique de la représentation des individus (dimensions 3-4) pour les hommes", axes = c(3, 4)) # Graphique des individus pour les hommes.
plot(resACP_femme, choice = "sqload", main="Graphique des charges (dimensions 1-2) pour les hommes", axes = c(1, 2)) # Graphique des charges pour les hommes.
plot(resACP_femme, choice = "sqload", main="Graphique des charges (dimensions 3-4) pour les hommes", axes = c(3, 4)) # Graphique des charges pour les hommes.

# 3. Régression Linéaire.

# Estimation du modèle de régression linéaire multiple.
# Utilisation de la fonction lm pour estimer les paramètres du modèle (et obtenir le tableau de l’analyse de variance).

res_donnees_numeriques <- lm(donnees$PROD_G_Frontal_Inf_Tri_1_L ~ donnees$Sexe + donnees$Age + donnees$Volume_Cerebral + donnees$Index_Lateralisation_Hemispherique + donnees$PROD_G_Angular_2_L + donnees$PROD_G_Occipital_Lat_1_L + donnees$PROD_G_Rolandic_Oper_1_L + donnees$PROD_S_Sup_Temporal_4_L + donnees$PROD_G_Hippocampus_1_L + donnees$PROD_G_Frontal_Inf_Tri_1_R + donnees$PROD_G_Angular_2_R + donnees$PROD_G_Occipital_Lat_1_R + donnees$PROD_G_Rolandic_Oper_1_R + donnees$PROD_S_Sup_Temporal_4_R + donnees$PROD_G_Hippocampus_1_R)
summary(res_donnees_numeriques)

# Régression Linéaire pour les femmes.

res_donnees_numeriques_femme <- lm(donnees_femme$PROD_G_Frontal_Inf_Tri_1_L ~ donnees_femme$Age + donnees_femme$Volume_Cerebral + donnees_femme$Index_Lateralisation_Hemispherique + donnees_femme$PROD_G_Angular_2_L + donnees_femme$PROD_G_Occipital_Lat_1_L + donnees_femme$PROD_G_Rolandic_Oper_1_L + donnees_femme$PROD_S_Sup_Temporal_4_L + donnees_femme$PROD_G_Hippocampus_1_L + donnees_femme$PROD_G_Frontal_Inf_Tri_1_R + donnees_femme$PROD_G_Angular_2_R + donnees_femme$PROD_G_Occipital_Lat_1_R + donnees_femme$PROD_G_Rolandic_Oper_1_R + donnees_femme$PROD_S_Sup_Temporal_4_R + donnees_femme$PROD_G_Hippocampus_1_R)
summary(res_donnees_numeriques_femme)

# Régression Linéaire pour les hommes.

res_donnees_numeriques_homme <- lm(donnees_homme$PROD_G_Frontal_Inf_Tri_1_L ~ donnees_homme$Age + donnees_homme$Volume_Cerebral + donnees_homme$Index_Lateralisation_Hemispherique + donnees_homme$PROD_G_Angular_2_L + donnees_homme$PROD_G_Occipital_Lat_1_L + donnees_homme$PROD_G_Rolandic_Oper_1_L + donnees_homme$PROD_S_Sup_Temporal_4_L + donnees_homme$PROD_G_Hippocampus_1_L + donnees_homme$PROD_G_Frontal_Inf_Tri_1_R + donnees_homme$PROD_G_Angular_2_R + donnees_homme$PROD_G_Occipital_Lat_1_R + donnees_homme$PROD_G_Rolandic_Oper_1_R + donnees_homme$PROD_S_Sup_Temporal_4_R + donnees_homme$PROD_G_Hippocampus_1_R)
summary(res_donnees_numeriques_homme)

# Utilisation de step pour la sélection automatique des variables.

res_step <- step(res_donnees_numeriques)
summary(res_step)

# 4. Diagnostic du modèle de Régression.

# Visualisation des résidus.

plot(res_step$fitted,res_step$residuals) # graphique des valeurs prédites versus les résidus.
abline(h=0)

# Tests de normalité des résidus.

shapiro.test(res_donnees_numeriques$residuals) # test de normalité des résidus pour res_donnees_numeriques.
shapiro.test(res_step$residuals) # test de normalité des résidus pour res_step.

# Test d'homoscédasticité.

bartlett.test(donnees_numeriques) # test de Bartlett pour vérifier l'homoscédasticité des résidus.
bptest(res_donnees_numeriques)  # Test de Breusch-Pagan pour vérifier l'homoscédasticité des résidus.

# Test de significativité globale du modèle : test pour savoir si le modèle de régression linéaire dans son ensemble est significatif.

anova(res_donnees_numeriques)

# Visualisation des résidus pour les femmes et les hommes.

plot(res_donnees_numeriques_femme$fitted, res_donnees_numeriques_femme$residuals) # Graphique pour les femmes.
plot(res_donnees_numeriques_homme$fitted, res_donnees_numeriques_homme$residuals) # Graphique pour les hommes.

# Tests de normalité des résidus pour les femmes et les hommes.

shapiro.test(res_donnees_numeriques_femme$residuals) # test de normalité des résidus pour les femmes.
shapiro.test(res_donnees_numeriques_homme$residuals) # test de normalité des résidus pour les hommes.

# Test d'homoscédasticité pour les femmes et les hommes.

bartlett.test(donnees_numeriques_femme) # test de Bartlett pour les résidus des femmes.
bartlett.test(donnees_numeriques_homme) # test de Bartlett pour les résidus des hommes.
bptest(res_donnees_numeriques_homme)  # Test de Breusch-Pagan pour les résidus des hommes.
bptest(res_donnees_numeriques_femme)  # Test de Breusch-Pagan pour les résidus des femmes.

# Test de significativité globale du modèle pour les femmes et les hommes : test pour savoir si le modèle de régression linéaire dans son ensemble est significatif.

anova(res_donnees_numeriques_femme)
anova(res_donnees_numeriques_homme)

# 5. Mesures d'influence.

# Calcul des mesures d'influence.

influence_values <- influence.measures(res_donnees_numeriques)

# Affichage les mesures d'influence.

print(influence_values)

# Graphique des résidus studentisés vs les valeurs ajustées :
plot(res_donnees_numeriques, which = 1, main = "Graphique des résidus studentisés vs. les valeurs ajustées")


# Mesures d'influence pour les femmes et les hommes.

# Calcul des mesures d'influence pour les femmes et les hommes.

influence_values_femme <- influence.measures(res_donnees_numeriques_femme)
influence_values_homme <- influence.measures(res_donnees_numeriques_homme)

# Affichage les mesures d'influence pour les femmes et les hommes.

print(influence_values_femme)
print(influence_values_homme)

# Graphique des résidus studentisés vs. les valeurs ajustées pour les femmes et les hommes.

plot(res_donnees_numeriques_femme, which = 1) # Graphique pour les femmes.
plot(res_donnees_numeriques_homme, which = 1) # Graphique pour les hommes.



