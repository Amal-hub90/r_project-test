graphique <- function() {
  # Affichage du graphique
  plot(1:10, 1:10, col = "orange")
  # Sauvegarde dans un fichier PNG
  dev.copy(png, "graphique.png", width = 800, height = 600)
  dev.off()
}
graphique()