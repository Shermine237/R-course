

N <- 100

### Determinons k (Nombre de classe)
# Par la methode de Sturge
(k.sturge <- round(1 + 3.33 * log10(N)))

# Par la methode de Yule
(k.yule <- round(2.5 * N^(1/4)))

#round: pour arrondir
#k.sturge equivalent a k_sturge (juste des noms de variable)


tube <- c(1.94, 2.20, 2.33, 2.39, 2.45, 2.50, 2.54, 2.61, 2.66, 2.85)
tube <- c(tube, 1.96, 2.21, 2.33, 2.40, 2.46, 2.51, 2.54, 2.62, 2.68, 2.87)
tube <- c(tube, 2.07, 2.26, 2.34, 2.40, 2.47, 2.52, 2.55, 2.62, 2.68, 2.90)
tube <- c(tube, 2.09, 2.26, 2.34, 2.40, 2.47, 2.52, 2.55, 2.62, 2.68, 2.91)
tube <- c(tube, 2.09, 2.28, 2.35, 2.40, 2.48, 2.52, 2.56, 2.62, 2.71, 2.94)
tube <- c(tube, 2.12, 2.29, 2.36, 2.41, 2.49, 2.52, 2.56, 2.63, 2.73, 2.95)
tube <- c(tube, 2.13, 2.30, 2.37, 2.42, 2.49, 2.53, 2.57, 2.63, 2.75, 2.99)
tube <- c(tube, 2.14, 2.31, 2.38, 2.42, 2.49, 2.53, 2.57, 2.65, 2.76, 2.99)
tube <- c(tube, 2.19, 2.31, 2.38, 2.42, 2.49, 2.53, 2.59, 2.66, 2.77, 3.09)
tube <- c(tube, 2.19, 2.31, 2.38, 2.42, 2.50, 2.54, 2.59, 2.66, 2.78, 3.12)


# Longueur du vecteur "tube"
length(tube)

# Mininum du vecteur "tube"
min(tube)

# Maximum du vecteur "tube"
max(tube)

# Quelques caracteristiques du vecteur "tube"
summary(tube)

# Moyenne des observations
mean(tube)

# Mediane des observations
median(tube)

# Calcul des quartiles (4)9
quantile(tube, probs = (1:3)/4) #1:3= 1, 2, 3 et (1:3)/4= 0.25, 0.5, 0.75

# Calcul des deciles (10)
quantile(tube, probs = (1:9)/10)

# A 20% des observations par exemple
quantile(tube, probs = 0.2)

# Calcul du pas en prelude a la transformation de la variable
pas <- (max(tube) - min(tube))/k.sturge

### Transformation de la variable quantitative en variable qualitative
epsilon <- 0.0001
# break
breaks <- min(tube) + (0:k.sturge) * pas
# on ajoute epsilon au dernier element de breaks pour regler le probleme du max qui hors intervale
breaks[length(breaks)] <- breaks[length(breaks)] + epsilon
# on definis les intervales avec cut
tube.qual <- cut(tube, breaks = breaks, right = F)

# Tableau des frequences absolues (effectifs)
tube.eff <- table(tube.qual)

# Tableau des frequences relatives (pourcentages)
round(table(tube.qual)/length(tube) * 100, 2) #round(table(tube.qual)/N * 100, 2). 2: nombre de chiffre apres la virgule pour arrondir











