# Predicting Poverty

Este repositorio corresponde al proyecto de predicción de la pobreza monetaria para el curso Big Data and Machine Learning for Applied Economics 2022.
Autores:
[Camilo Bonilla](https://github.com/cabonillah),  [Nicolás Velásquez](https://github.com/Nicolas-Velasquez-Oficial) y  [Rafael Santofimio](https://github.com/rasantofimior/)

# Resumen

Combatir la pobreza en todas sus dimensiones es uno de los principales retos que afrontan los gobiernos, es por esto que se diseñan múltiples técnicas, herramientas y metodologías para medir y predecir este fenómeno a nivel regional y local. Así, con base estas priorizar programas dirigidos a mejorar las condiciones educativas, de trabajo, salud, acceso a servicios públicos, niñez y juventud, y a la focalización de recursos y subsidios.   Teniendo en cuenta lo anterior, y que resulta demandante en términos de tiempo y costos la de medición de este fenómeno a través de encuestas, este repositorio alberga la propuesta de dos modelos que predicen la pobreza monetaria de los hogares colombianos a partir de la información reportada en la GEIH 2018, los cuales están diseñados a partir de herramientas de Big Data, y que permiten establecer la importancia relativa de las variables con su respectivo poder explicativo.

Este repositorio contiene las siguientes carpetas:

## Carpeta Document

-Pobreza_prediccion_COL.pdf:
Este documento muestra las aproximaciones teóricas de la medición y la predicción de la pobreza monetaria en Colombia, el tipo de tratamiento que se le dio a los datos que fueron insumo base para los distintos modelos, las especificaciones e hiperparámetros fijada a cada modelo, así como un análisis de los principales resultados, conclusiones y recomendaciones. 

## Carpeta Stores
Esta carpeta alberga bases de datos relacionadas con el entrenamiento y testeo a nivel de hogar y persona. Así como los resultados de cada modelos, y el reporte de los dos mejores modelos de clasificación y regresión. 
- test_hogares.Rds
- train_hogares.Rds
- test_personas.Rds
- train_personas.Rds
- data.rds
Base de datos consolidada a nivel de hogar (input)
- ddi-documentation-spanish-608.pdf
Documento con la descripción de las variables
- elastic_clas.rds
- elastic_reg.rds
- lasso_clas.rds
- lasso_reg.rds
- lm_clas.rds
- rf_clas.rds
- rf_reg.rds
- ridge_clas.rds
- ridge_reg.rds
- xgb_clas.rds
- xgb_reg.rds
- predictions_Bonilla_Santofimio_Velasquez_c4_r5.csv
Este archivo contiene las predicciones del mejor modelo de clasificación y regresión respectivamente. 


## Carpeta File:

-   El análisis de datos se realiza utilizando el software R version 4.0.2 (2022-09-05)

-   main.sh: Contains the sequence of execution of dofiles and Rscripts to produce the figures and tables in the paper and appendix. Dofiles and Los Escripts en R se nombran segun la función que tienen:

    -   La carpeta scripts contine los codigos nombrados a continución:

        -   data_merging
        -   train_test_impute
        -   LM_RES
        -   LOGIT_CLASS
        -   Tablets_reporters
        -   elastic_clas
        -   elastic_reg
        -   lasso_clas
        -   lasso_reg
        -   lm_clas
        -   lm_reg
        -   random_forest
        -   ridge_clas
        -   ridge_reg
        -   sgboost_rf
        -   rf_clas
        -   rf_reg
        -   tuning
        -   workflows
        -   grids
        -   specs
        -   xgb_clas
        -   xgb_reg
        

## Carpeta Views:

Las Figuras y tablas estan alojadas en la carpeta "views" nombradas de la siguiente manera: 

    - categoricas_descrip.tex
    - continuas_descrip.tex
    - Log_Ingpcup.png
    - RF CLASS.png
    - RF REG.png
    - Seleccion_aletoria_predictores_RF.png
Notas:

-   Los Scripts descriptives_punto2.R, Punto_3.R y performance_tests.R, llaman al modulo dentro del script data_cleaning.R el cual estandariza la data utilizada.
-   Si se ejecutan los scripts desde programas como R Studio, se debe asegurar antes que el directorio base se configure en "Problem-Set1\scripts".
-   Se recomienda enfacticamnete seguir las instrucciones y comentarios del código (en orden y forma). Así mismo, es importante que antes de verificar la              replicabilidad del código, se asegure de tener **todos** los requerimientos informáticos previamente mencionados (i.e. se prefieren versiones de **R** menores a la 4.2.1. para evitar que paquetes, funciones y métodos que han sido actualizados no funcionen). Además, la velocidad de ejecución dependerá de las características propias de su máquina, por lo que deberá (o no) tener paciencia mientras se procesa.*
