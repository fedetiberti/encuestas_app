# encuestas_app

Esta ShinyApp programada en R recopila datos de encuestas electorales desde Wikipedia y genera un gráfico, trazando tendencias para cada coalición electoral en base a un método de suavizado (LOESS).

La fuente de los datos es este artículo (https://es.wikipedia.org/wiki/Anexo:Encuestas_de_intenci%C3%B3n_de_voto_para_las_elecciones_presidenciales_de_Argentina_de_2023). 
El código del scrapeo del artículo y la app está disponible en Github. Código mantenido por Rodrigo Quiroga, forkeado del repositorio original de Federico Tiberti (https://github.com/fedetiberti/encuestas_app)
al cuál agregué las opciones de proyectar votos indecisos, comparar con los resultados electorales 2021 y 2023, separar los resultados pre y post PASO 2023, y un slider para ajustar el suavizado. La inclusión de las encuestas en este agregador no implica un respaldo a sus metodologías ni a la verosimilitud de sus resultados. Nota: Por default sólo se incluye en el análisis a encuestadoras con 5 o más encuestas, excluyendo a Management & Fit, Giaccobe & Asociados y Proyección Consultores, con lo cual se obtiene un mejor ajuste a los datos electorales 2021. 

Versión interactiva disponible aquí:
https://rquiroga7.shinyapps.io/Agregador_Encuestas/

El gráfico generado se asimila al siguiente:
![plot_encuestas_2023-09-25](https://github.com/rquiroga7/encuestas_app/assets/8103453/f1ed3ceb-5b5d-4356-89fa-d9c593a14e6c)
