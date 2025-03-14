library(plyr)
library(readr)
library(dplyr)
#library(sound)
library(seewave)
library(tuneR)
library(lubridate)

#Ce code nécessite la fonction suivante :
#https://github.com/tdelattre/spectroCustom
source("//nas-avi.paca.inrae.fr/paca-psh-users$/psh/rminguet/Documents/Thèse/Analyse/Script_generaux/Fonction/spectrogramCustomFunction.R")
#Modifier le chemin d'accès précédent vers l'endroit où a été stocké la fonction

#définition du lecteur audio 
tuneR::setWavPlayer('vlc.exe --play-and-exit')
getWavPlayer()

#définition de la localisation pour les conversions de dates
Sys.setlocale("LC_TIME", "C")
#---------------------------------------------------------------------------------------------------------------------------

#Toute la démarche est basée sur notre description de chemin d'accès où est spécifié notre session et notre parcelle


                        ################################################################################
                        #                                     1er étape :                                #
                        #       charger le fichier des données birdnet compilées (detection total)       #
                        ################################################################################

######################################################################
# Si tableau de test d'écoute déjà disponible : passer à l'étape 5
######################################################################

#Ici fichier de compilation des détections de 2023

FullDS2024<-read.csv("Chemin_d_acces.csv", header=TRUE, sep=";")

#mise en forme correcte du fichier
FullDS2024$Confidence <- gsub(",", ".", FullDS2024$Confidence)
FullDS2024$Confidence<-as.numeric(FullDS2024$Confidence)

#créer la colonne date, heure et capteur
FullDS2024$date<-substring(FullDS2024$fileName,10,17)
FullDS2024$date<-as_date(FullDS2024$date)
FullDS2024$recorder=substring(FullDS2024$fileName,1,8)
FullDS2024$time=substring(FullDS2024$fileName,19,24)

#créer une colonne intervalle_confiance pour tirer aléatoirement dedans
FullDS2024 <- FullDS2024 %>%
  mutate(intervalle_confiance = ifelse(Confidence <= 0.2, 0.2, ceiling(Confidence * 10) / 10))
FullDS2024$intervalle_confiance<-as.factor(FullDS2024$intervalle_confiance)



                            ###########################################################
                            #                         2ème étape :                    #
                            #         définir chemin d'accès pour fichier audio       #
                            ###########################################################

# (doit correspondre à l'endroit où sont les sons à écouter, correspondant au fichier Birdnet ci-dessus)
rootdir="N:/tdelattre/data/TERRAIN_2024_NAS/PRINTEMPS/"

# ajout du nom du dossier où est stocké l'enregistrement (ajout de la session et de la parcelle dans notre cas)
FullDS2024$mydir=paste(rootdir,FullDS2024$session,"/",FullDS2024$parcelle,"/Data2/",sep="")
head(FullDS2024$mydir)

# ajout du nom de fichier audio dans le jeu de données en mémoire
FullDS2024$Begin.File=paste(substr(FullDS2024$fileName,start = 0,stop=25),"wav",sep="")
head(FullDS2024$Begin.File)



                            #############################################################################
                            #                           3ème étape :                                    #
                            # Mise en place du jeu de données souhaité en fonction des filtres choisit  #
                            #         ici : n tirages aléatoires par tranche de confiance             #
                            #############################################################################

# filtrer sur le nom d'espèce, en tirant aléatoirement un nombre n d'échantillons dans chaque intervalle de confiance

Listen <- FullDS2024 %>%
  filter(Common.Name == "Mésange charbonnière") %>%       #définir ici le nom d'espèce
  group_by(intervalle_confiance) %>%
  slice_sample(n = 10, replace = FALSE) %>%       #définir ici le nombre de tirage aléatoire
  ungroup()

#Vérification des indices de confiance
hist(Listen$Confidence)

#Si besoin, la structure que le tableau doit avoir pour que le script d'écoute fonctionne est sur le github : 



                        ###################################
                        #           4ème étape :          #
                        # Boucle d'écoute et annotations  #
                        ###################################


fenetre<-1   #permet d'avoir les abords du chant et pas que les 3 secondes


# Création d'un dataframe pour stocker les réponses
tableau_recapitulatif <- data.frame(Enregistrement = numeric(length(Listen$Selection)),
                             Chemin_acces = numeric(length(Listen$Selection)),
                             Begin.Time..s. = numeric(length(Listen$Selection)),
                             End.Time..s. = numeric(length(Listen$Selection)),
                             Ind_confiance = numeric(length(Listen$Selection)),
                             Vrai_Positif = numeric(length(Listen$Selection)))


for (i in 1:length(Listen$Selection)) {
  # Afficher le numéro de l'enregistrement
  print(i)
  # Afficher le chemin d'accès
  print(paste(Listen$mydir[i], Listen$Begin.File[i], sep=""))
  # Afficher l'indice de confiance de chaque enregistrement
  print(paste("Indice de confiance = ", Listen$Confidence[i], sep=""))
  
  # Créer la sélection avec le chemin d'accès pour chaque enregistrement
  selection <- readWave(paste(Listen$mydir[i], Listen$Begin.File[i], sep=""),
                        from=Listen$Begin.Time..s.[i]-fenetre,
                        to=Listen$End.Time..s.[i]+fenetre,
                        units="seconds")
  
  # Création du spectrogramme
  spectroCustom(selection)  #fonction du spectro dispo sur le github de Thomas Delattre : https://github.com/tdelattre/spectroCustom
  
  # Ecoute de l'enregistrement
  play(selection, getWavPlayer())
  
  # Demander à l'utilisateur si la détection est vraie
  reponse <- as.numeric(readline("La détection est-elle vraie ? (1 pour oui, 0 pour non, 2 si doute, 3 pour réécouter) : "))
  
  # Stocker les informations dans le dataframe des réponses
  tableau_recapitulatif[i, "Enregistrement"] <- Listen$Begin.File[i]
  tableau_recapitulatif[i, "Chemin_acces"] <- paste(Listen$mydir[i], Listen$Begin.File[i], sep="")
  tableau_recapitulatif[i, "Begin.Time..s."] <- Listen$Begin.Time..s.[i]
  tableau_recapitulatif[i, "End.Time..s."] <- Listen$End.Time..s.[i]
  tableau_recapitulatif[i, "Ind_confiance"] <- Listen$Confidence[i]
  tableau_recapitulatif[i, "Vrai_Positif"] <- ifelse(reponse == 1, "oui", ifelse(reponse == 0, "non", ifelse(reponse == 2, "doute", reponse)))
  
  
  # Réécouter l'enregistrement si la réponse est 2
  while(reponse == 3) {
    play(selection, getWavPlayer())
    reponse <- as.numeric(readline("La détection est-elle vraie ? (1 pour oui, 0 pour non, 2 si doute, 3 pour réécouter) : "))
    tableau_recapitulatif[i, "Vrai_Positif"] <- reponse
  }
}



                                        ###########################################
                                        #                  Etape 5                #
                                        #  Calcul de l'indice de confiance seuil  #
                                        ###########################################

################################################################################################
# Si tableau de test d'écoute déjà disponible :
# tableau recapitulatif<-read.csv("chemin_dacces_fichier_ecoute.csv", header=TRUE, sep=";)
# Un exemple de la structure du tableau attendu dans cette partie est présente sur le github :
################################################################################################


#Remettre les réponses en binaire pour mettre en place le modèle
tableau_recapitulatif <- tableau_recapitulatif %>%
  mutate(vrai_positif_binaire = case_when(
    Vrai_Positif == "oui" ~ 1,
    Vrai_Positif == "non" ~ 0,
    Vrai_Positif == "doute" ~ 2,
    TRUE ~ NA_real_  # Pour gérer les valeurs manquantes ou autres valeurs non prévues
  ))
#On enlève maintenant les doutes (Decider ici si vous voulez les fusionner (avec oui ou non) ou les ignorer)
tableau_recapitulatif <- tableau_recapitulatif %>%
  filter(Vrai_Positif != "doute")

#On définit la colonne Vrai_positif_binaire comme étant binaire
tableau_recapitulatif$vrai_positif_binaire <- factor(tableau_recapitulatif$vrai_positif_binaire,levels = c(0, 1))

#Info sur les test effectuer
tableau_recapitulatif %>%
  summarise(
    faux_positifs = sum(Vrai_Positif == "non"),
    vrais_positifs = sum(Vrai_Positif == "oui"),
    doutes = sum(Vrai_Positif == "doute")
  )



#Mise en place du modèle
Modele_indice_seuil <- glm(vrai_positif_binaire ~ Ind_confiance, 
                           data = tableau_recapitulatif, 
                           family = "binomial")



#représentation graphique des résultats d'écoute pour l'espèce en question
tableau_recapitulatif$predicted_prob <- predict(Modele_indice_seuil, type = "response")
ggplot(tableau_recapitulatif, aes(x = Ind_confiance, y = as.numeric(vrai_positif_binaire) - 1)) +  # Décalage pour la position des points
  geom_point(aes(color = as.factor(vrai_positif_binaire)), alpha = 0.6, shape = 1, size = 2, fill = NA) +  # Points vides
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "blue") +  # Courbe de régression logistique
  xlab("Indice de confiance") +  # Label de l'axe X
  ylab("Probabilité de vrai positif") +  # Label de l'axe Y
  ggtitle("Probabilité de vrai positif en fonction de l'indice de confiance") +  # Titre du graphique
  theme_classic() +  # Thème classique
  scale_y_continuous(breaks = seq(0, 1, by = 0.2),  # Échelle de l'axe y de 0 à 1 par intervalles de 0.2
                     labels = seq(0, 1, by = 0.2)) +  
  scale_color_manual(values = c("red", "blue"), labels = c("Mauvaises réponses", "Bonnes réponses")) +  # Couleurs pour les points
  theme(legend.title = element_blank())



# Calcul de l'indice seuil  
Proba_bonne_reponse_choisit <- 0.90       #Choisit la probabilité de bonne réponse (NPT dans l'article Darras et al, 2019) souhaité

indice_seuil_espece_cible <- (log(Proba_bonne_reponse_choisit / (1 - Proba_bonne_reponse_choisit)) - Modele_indice_seuil$coefficients[1]) / 
  Modele_indice_seuil$coefficients[2]
indice_seuil_espece_cible




