#Commencez par charger le jeu de données relatif au tournoi de Roland Garros avec read_csv dans un tibble rg à partir du fichier rolandgarros2013.csv.

rg <- read_csv("rolandgarros2013.csv")
getwd() # ou est ce qu'il va chercher le fichier 

# Affichez le nom des demi-finalistes.
rg %>% 
  filter(Round == 6) %>% 
  select(starts_with(("Player")))

# Calculez le nombre moyen d’aces par match dans le tournoi.

rg %>%
  summarize(
    moyenne_aces = mean(ACE.1 + ACE.2, na.rm = TRUE)
  )

# Combien y a-t-il eu d’aces par match en moyenne à chaque niveau du tournoi ?
rg %>%
  mutate(total_aces = ACE.1 + ACE.2) %>%
  group_by(Round) %>%
  summarise(
    moyenne_aces = mean(total_aces, na.rm = TRUE)
  )

#Récupérez la liste des joueurs à partir des colonnes Player1 et Player2 et stockez le résultat dans un tibble rg_joueurs ne contenant qu’une seule variable Joueur. Une façon de faire consiste à utiliser
#bind_rows pour concaténer les colonnes Player1 et Player2 dans une colonne Joueur, puis d’appeler la fonction distinct.
rg_joueurs <- bind_rows(
  rg %>% select(Joueur = Player1),
  rg %>% select(Joueur = Player2)
) %>%
  distinct()

rg_joueurs


#Ajoutez au tibble rg_joueurs la variable Victoires qui contient le nombre de victoires de chaque
#joueur dans le tournoi de Roland Garros. Une approche est de commencer par créer une fonction qui
#compte le nombre de victoires pour un joueur donné dans rg, puis de l’appliquer à rg_joueurs avec rowwise et mutate.

compte_victoires <- function(joueur) {
  sum(rg$Result == 1 & rg$Player1 == joueur) +
    sum(rg$Result == 2 & rg$Player2 == joueur)
}

rg_joueurs <- rg_joueurs %>%
  rowwise() %>%
  mutate(Victoires = compte_victoires(Joueur)) %>%
  ungroup()

#autre façon: 
rg %>% 
  select(Player1, Player2,Result)

n_rg_victoires <- function(joueur) {
  return(
    rg %>% 
      filter(
        Player1 == joueur & Result ==1
      ) | (
        Player2 == joueur & Result ==0
      )
  ) %>% 
    summarize( n=n ()) %>% 
    as.numeric()
}

n_rg_victoires("Roger Federer")

rg_joueurs %>% 
  rowwise () %>% 
  mutate(n_victoires =n_rg_victoires(Joueur)) %>% 
  ungroup()

#Importez le jeu de données relatif au tournoi de l’Open d’Australie dans un tibble oa à partir du fichier openaustralie2013.csv
oa <- read_csv("openaustralie2013.csv")

#Concaténez les tibbles rg et oa dans un tibble tennis avec bind_rows en ajoutant une variable Tournoi
#contenant RG ou OA selon le tournoi grâce à l’option .id. Comptez le nombre de matchs par tournoi à partir de ce nouveau tibble.
tennis <- bind_rows(
  RG = rg,
  OA = oa,
  .id = "Tournoi"
)

#Utilisez tennis pour comparer le nombre moyen d’aces par match à chaque niveau du tournoi à Roland
#Garros et à l’Open d’Australie. Affichez le résultat au format long et au format large.
test <- tennis %>%
  mutate(total_aces = ACE.1 + ACE.2) %>%
  group_by(Tournoi, Round) %>%
  summarize(
    moyenne_aces = mean(total_aces, na.rm = TRUE),
    .groups = "drop"
  )

head(test)
#format large 
test_large <- tennis %>%
  mutate(total_aces = ACE.1 + ACE.2) %>%
  group_by(Tournoi, Round) %>%
  summarise(
    moyenne_aces = mean(total_aces, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Tournoi,
    values_from = moyenne_aces
  )



#Créez un tibble oa_joueurs avec la variable Victoires qui contient le nombre de victoires de chaque
#joueur dans le tournoi de l’Open d’Australie comme dans les questions 5 et 6.
oa_joueurs <- bind_rows(
  oa %>% select(Joueur = Player1),
  oa %>% select(Joueur = Player2)
) %>%
  distinct()

compte_victoires_oa <- function(joueur) {
  sum(oa$Result == 1 & oa$Player1 == joueur) +
    sum(oa$Result == 2 & oa$Player2 == joueur)
}

oa_joueurs <- oa_joueurs %>%
  rowwise() %>%
  mutate(Victoires = compte_victoires_oa(Joueur)) %>%
  ungroup()

oa_joueurs

oa_joueurs %>%
  arrange(desc(Victoires))

#Faites une jointure entre rg_joueurs et oa_joueurs sur la variable Joueur pour comparer le nombre
#de victoires par tournoi pour chaque joueur. Expliquez la différence de résultat selon que la jointure est
#à gauche, à droite, intérieure ou extérieure.

rg_joueurs %>%
  left_join(oa_joueurs, by = "Joueur")

rg_joueurs %>%
  right_join(oa_joueurs, by = "Joueur")

rg_joueurs %>%
  inner_join(oa_joueurs, by = "Joueur")

rg_joueurs %>%
  full_join(oa_joueurs, by = "Joueur")

comparaison <- rg_joueurs %>%
  inner_join(oa_joueurs, by = "Joueur")

comparaison


