

data(iris)
view(iris)

# Sélectionnez les variables Petal.Width et Species.
iris %>%
  select(Petal.Width, Species)

# Construisez un tibble qui contient uniquement les iris d’espèce versicolor ou virginica.
iris_bis <- iris %>%
  filter(Species %in% c("versicolor", "virginica"))

iris_bis

# Comptez le nombre d’iris setosa en utilisant summarise.
iris_agregats <- iris %>%   
  summarize(nombre_setosa = sum(Species == "setosa"))

# Calculez la moyenne de la variable Petal.Width pour les iris de l’espèce versicolor.
iris %>%
  filter(Species == "versicolor") %>%
  summarize(moyenne = mean(Petal.Width))

iris %>% 
  filter(Species == "setosa") %>% 
  summarize(moyenne2 = mean(Petal.Width))

# Ajoutez une variable Sum.Width qui correspond à la somme de Petal.Width et Sepal.Width.
iris_test <- iris %>%
  mutate(Sum.Width = Petal.Width + Sepal.Width)

# Calculez la moyenne et la variance de la variable Sepal.Length pour chaque espèce.
iris %>%
  group_by(Species) %>%
  summarise(
    moyenne = mean(Sepal.Length),
    variance = var(Sepal.Length)
  )

#Houston Flights 
data("hflights")

#Sélectionnez les variables DepTime, ArrTime, ActualElapsedTime et AirTime grâce à ends_with.
test1 <- hflights %>% select("DepTime","ArrTime","ActualElapsedTime","AirTime")
test2 <- hflights %>% select(ends_with("Time"))

#Ajoutez une variable ActualGroundTime qui correspond à ActualElapsedTime moins AirTime.
test3 <- hflights %>%
  mutate(ActualGroundTime = ActualElapsedTime - AirTime)

#Ajoutez une variable AverageSpeed qui donne la vitesse moyenne du vol et ordonnez le résultat selon les valeurs décroissantes de cette variable.
#ActualElapsedTime = durée réelle du vol, en minutes
#AirTime temps réellement passé en vol (minutes)
#Comme AirTime est en minutes, il faut convertir en heures 
test4 <- hflights %>%
  mutate(AverageSpeed = Distance / (AirTime / 60)) %>%
  arrange(desc(AverageSpeed)) %>% select(Year, AirTime, Distance, AverageSpeed)


#Sélectionnez les vols à destination de JFK.
test5 <- hflights %>% 
  filter(Dest == "JFK")

#Comptez le nombre de vols à destination de JFK
 hflights %>% 
  filter(Dest == "JFK") %>% count()

 #Créez un résumé de hflights qui contient : n : le nombre total de vols, n_dest: le nombre total de destinations distinctes, n_carrier : le nombre total de compagnies distinctes.
 test6 <- hflights %>%
   summarize(
     n = n(),
     n_dest = n_distinct(Dest),
     n_carrier = n_distinct(UniqueCarrier)
   )
 
 #Pour les vols de la compagnie AA, créez un résumé qui contient: • le nombre total de vols, le nombre total de vols annulés,la valeur moyenne de ArrDelay (attention à la gestion des NA).
 test7 <- hflights %>% 
   filter(UniqueCarrier == "AA") %>% 
   summarize(
     n = n(),
     n_canceled = sum(Cancelled),
     mean = mean(ArrDelay, na.rm = TRUE)
   )
 
 #Calculez pour chaque compagnie :le nombre total de vols, la valeur moyenne de AirTime
 
 
 
 
 
 
 