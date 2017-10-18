# aa-tp
Trabajo práctico de la materia Aprendizaje Automático - Machine Learning (Maestría en DM - FCEN - UBA). Consiste en utilizar un dataset de KDNuggets donde se registraron un conjunto de tweets y predecir cuáles fueron escritos por humanos y cuáles no.

## Descripción
El repositorio cuenta con tres archivos:
* feat-engineering.R: carga el dataset y crea las variables que permitan mejorar el desempeño predictivo del modelo. El proceso de feature engineering requirió analizar tanto la metadata de los tweets como el texto de los mismos.
* modelos.R: crea un conjunto de modelos predictivos estudiados en la materia, desde los más sencillos a los más complejos.
* ensamble.R: construye manualmente distintos ensambles ponderados según probabilidades.
