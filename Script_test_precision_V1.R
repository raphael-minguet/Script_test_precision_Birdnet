library(plyr)
library(readr)
library(dplyr)
library(seewave)
library(tuneR)
library(lubridate)

#Ce code nécessite la fonction suivante :
#https://github.com/tdelattre/spectroCustom
source("//nas-avi.paca.inrae.fr/paca-psh-users$/psh/rminguet/Documents/Thèse/Analyse/Script_generaux/Fonction/spectrogramCustomFunction.R")
#Modifier le chemin d'accès précédent vers l'endroit où a été stocké la fonction

#définition du lecteur audio sur windows
tuneR::setWavPlayer('vlc.exe --play-and-exit')
getWavPlayer()

#définition du lecteur audio sur linux
#AtuneR::setWavPlayer('/usr/bin/aplay')
#getWavPlayer()

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

#Ici fichier de compilation des détections Birdnet
FullDS2024<-read.csv("N:/tdelattre/data/TERRAIN_2024_NAS/PRINTEMPS/compilation_birdnet_s1S10.csv", header=TRUE, sep=";")


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
head(FullDS2024$mydir)    # ici : "N:/tdelattre/data/TERRAIN_2024_NAS/PRINTEMPS/S1/177/Data2/"

# ajout du nom de fichier audio dans le jeu de données en mémoire
FullDS2024$Begin.File=paste(substr(FullDS2024$fileName,start = 0,stop=25),"wav",sep="")
head(FullDS2024$Begin.File)   # ici : "SMU05114_20240313_112531.wav"



                            #############################################################################
                            #                           3ème étape :                                    #
                            # Mise en place du jeu de données souhaité en fonction des filtres choisit  #
                            #         ici : n tirages aléatoires par tranche de confiance             #
                            #############################################################################

# filtrer sur le nom d'espèce, en tirant aléatoirement un nombre n d'échantillons dans chaque intervalle de confiance

Listen <- FullDS2024 %>%
  filter(Common.Name == "Mésange bleue") %>%       #définir ici le nom d'espèce
  group_by(intervalle_confiance) %>%
  slice_sample(n = 100, replace = FALSE) %>%       #définir ici le nombre de tirage aléatoire
  ungroup()

#Vérification des indices de confiance
hist(Listen$Confidence)

#Si besoin, la structure que le tableau doit avoir pour que le script d'écoute fonctionne est sur le github : https://github.com/raphael-minguet/Script_test_precision_Birdnet/blob/main/Exemple_structure_tableau_sortie_Birdnet.csv


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
  
  # Pause pour laisser le au spectrogramme le temps d'être affiché (à activer si l'ordi à tendance à lancer le son avant le spectro)
  #Sys.sleep(5)
  
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


#write.csv2(tableau_recapitulatif, "//nas-avi.paca.inrae.fr/paca-psh-users$/psh/rminguet/Documents/Thèse/Analyse/Communauté_printemps/Indice de confiance par espèces/Tableau validation/analyse_OK/Mesange bleue/Mesangebleue_OK.csv")

                                        ###########################################
                                        #                  Etape 5                #
                                        #  Calcul de l'indice de confiance seuil  #
                                        ###########################################

################################################################################################
# Si tableau de test d'écoute déjà disponible :
# tableau recapitulatif<-read.csv("chemin_dacces_fichier_ecoute.csv", header=TRUE, sep=";)
# Un exemple de la structure du tableau attendu dans cette partie est présente sur le github : https://github.com/raphael-minguet/Script_test_precision_Birdnet/blob/main/Exemple_structure_tableau_de_donnees_test_ecoute.csv
################################################################################################


#Remettre les réponses en binaire pour mettre en place le modèle
tableau_recapitulatif <- tableau_recapitulatif %>%
  mutate(vrai_positif_binaire = case_when(
    Vrai_Positif == "oui" ~ 1,
    Vrai_Positif == "non" ~ 0,
    Vrai_Positif == "doute" ~ 2,
    TRUE ~ NA_real_  # Pour gérer les valeurs manquantes ou autres valeurs non prévues
  ))

#Info sur les test effectuées
print(
  tableau_recapitulatif %>%
    summarise(
      faux_positifs = sum(Vrai_Positif == "non"),
      vrais_positifs = sum(Vrai_Positif == "oui"),
      doutes = sum(Vrai_Positif == "doute")
    ),
  row.names = FALSE
)

#On enlève maintenant les doutes (Decider ici si vous voulez les fusionner (avec oui ou non) ou les ignorer)
tableau_recapitulatif <- tableau_recapitulatif %>%
  filter(Vrai_Positif != "doute")

#On définit la colonne Vrai_positif_binaire comme étant binaire
tableau_recapitulatif$vrai_positif_binaire <- factor(tableau_recapitulatif$vrai_positif_binaire,levels = c(0, 1))



#Mise en place du modèle
Modele_indice_seuil <- glm(vrai_positif_binaire ~ Ind_confiance, 
                           data = tableau_recapitulatif, 
                           family = "binomial")



#représentation graphique des résultats d'écoute pour l'espèce en question
tableau_recapitulatif$predicted_prob <- predict(Modele_indice_seuil, type = "response")
ggplot(tableau_recapitulatif, aes(x = Ind_confiance, y = as.numeric(vrai_positif_binaire) - 1)) +  
  geom_point(aes(color = as.factor(vrai_positif_binaire)), alpha = 0.6, shape = 1, size = 2, fill = NA) +  
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "blue") +  # Suppression de l'ombre grise
  xlab("Indice de confiance") +  
  ylab("Probabilité de vrai positif") +  
  ggtitle("Probabilité de vrai positif en fonction de l'indice de confiance") +  
  theme_classic() +  
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = seq(0, 1, by = 0.2)) +  
  scale_color_manual(values = c("red", "blue"), labels = c("Mauvaises réponses", "Bonnes réponses")) +  
  theme(legend.title = element_blank())



# Calcul de l'indice seuil  
Proba_bonne_reponse_choisit <- 0.90       #Choisit la probabilité de bonne réponse (NPT dans l'article Darras et al, 2019) souhaité

indice_seuil_espece_cible <- (log(Proba_bonne_reponse_choisit / (1 - Proba_bonne_reponse_choisit)) - Modele_indice_seuil$coefficients[1]) / 
  Modele_indice_seuil$coefficients[2]
indice_seuil_espece_cible



