library(umap)

iris_data <- iris[, -5]
iris_label <- iris[, 5]

# Método naive (escrito en R)
iris_umap <- umap(d = iris_data, method = "naive")
head(iris_umap$layout)

# Método umap-learn (python umap-learn)
# Hay que instalar previamente umap-learn en python
iris_umap2 <- umap(d = iris_data, method = "umap-learn")
head(iris_umap2$layout)

# Uniendo datos
nueva_umap <- cbind(iris_umap$layout, iris_label) %>% 
  as.tibble()
names(nueva_umap) <- c("x", "y", "specie")

nueva_umap2 <- cbind(iris_umap2$layout, iris_label) %>% 
  as.tibble()
names(nueva_umap2) <- c("x", "y", "specie")

# Predicción de nuevos datos
prueba <- data.frame(5.90, 4, 2.5, 1.82)
names(prueba) <- names(iris[, -5])
names(prueba)

predict_umap1 <- as.data.frame(predict(object = iris_umap, data = prueba))
predict_umap2 <- as.data.frame(predict(object = iris_umap2, data = prueba))

# Comparando dos métodos
library(ggpubr)
ggarrange(
  nueva_umap %>% 
    ggplot(aes(x = x, y = y, color = factor(specie,
                                            labels = levels(as.factor(iris$Species))))) +
    geom_point() +
    scale_color_brewer(palette = "Set1") +
    labs(color = "Especie", title = "method = 'naive'") +
    geom_point(data = predict_umap1, aes(x = V1, y = V2), color = "black", size = 3),
  
  nueva_umap2 %>% 
    ggplot(aes(x = x, y = y, color = factor(specie,
                                            labels = levels(as.factor(iris$Species))))) +
    geom_point() +
    scale_color_brewer(palette = "Set1") +
    labs(color = "Especie", title = "method = 'umap-learn'",
         subtitle = "Python desde R (reticulate)") +
    geom_point(data = predict_umap2, aes(x = V1, y = V2), color = "black", size = 3)
)

