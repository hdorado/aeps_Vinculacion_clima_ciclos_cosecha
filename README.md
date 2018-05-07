# Vinculación de variables climáticas a ciclos de cultivo

El presente proyecto desarrollado en `Rproject` tiene el propósito de describir el proceso realizado para el procesamiento de variables climáticas en ciclos de cultivo, donde la información fuente son estaciones meteorológicas. Este es un paso importante en el análisis de agricultura específica por sitio, debido a que el clima desempeña un rol importante en la explicación de la variación en rendimiento. Los pasos descritos serán de utilidad cuando se tiene sitios de interés georreferenciados, cuya información está dada en un periodo de tiempo y además se quiere evaluar la relación con estaciones meteorológicas dentro de la misma localidad que están georreferenciadas.

Dentro de la carpeta scripts encontrará los dos siguientes archivos:

* *Stations_Catalog.R*: En este proceso se organiza los datos climáticos en un catálogo que será utilizado en el siguiente script Distances_Process. También se calcula la altura de los lotes y estaciones, con base en un RASTER de altura de la localidad de interés.
* *Distances_Process*:  En este paso se realiza el proceso de vinculación de estaciones a lotes y se cálculas los indicadores climáticos de interés.

El archivo *Merge_Stations_Funs.R*, contiene funciones programadas que son utilizadas en *Distances_Process*
Para utilizar los scripts debe primero tener instalada una versión de R-Studio, preferiblemente la 1.1.423 y una versión de R mayor a 3.4.3, posteriormente abrir el archivo `Vinculacion_clima_lotes_comerciales.Rproj` y desde esta consola abrir los scripts. Para ejecutar el ejemplo reproducible se debe descomprimir el archivo `chiapas_srtm.zip`, que está en la carpeta BASIC_FILES.
Para más información detallada del funcionamiento de cada paquete, puede consultar la carpeta CATALOGS, en la cual se extiende la explicación de cada función y además sigue un ejemplo reproducible en Chiapas México.
