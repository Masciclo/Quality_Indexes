![Ambito](Imagenes/portada_catastro.png)

## Descripción

En el marco de los catastros realizados en colaboración con CEDEUS de las ciclovías del gran Santiago entre los años 2019-2022, y del proyecto de investigación trabajado para esta publicación [texto del enlace](URL), este proyecto de data science evalúa tres indicadores de calidad de ciclovías derivados de normativas ([Ministerio de Transportes y Telecomunicaciones, 2021](https://drive.google.com/file/d/1R5758sRyN_SN1e4AlhZG54nbbOQGZ3m_/view?usp=sharing)), manuales de diseño ([Guía de composición y diseño operacional de ciclovías, 2019](https://drive.google.com/file/d/12npPNzV-C_9jRNPPgI2-s5krZYxMswyf/view?usp=sharing)) y academia ([Merkuria, 2012](https://drive.google.com/file/d/1BRqqaboLDGDyY7uY7nbGsKPCBZhmBXlE/view?usp=sharing)), y propone la misma evaluación para un indicador propio.  
![Variables](Imagenes/variables.png)

## Resultados

A partir desde una archivo shapefile con las variables mostrada en la fig.1 se producen la siguiente lista de tablas, gráficos y mapas:

### Tablas

- 1.a Km totales ciclovias y cruces
- 1.b Km totales ciclovias y cruces por comuna
- 2.a Km totales por tipologia
- 2.b Km totales por tipologia por comuna
- 3.a Km totales por tipo de servicio de calle
- 3.b Km totales por tipo de servicio de calle por comuna
- 4 Tipo inoperatividad
- 5.a Distribucion km operativos inoperativos
- 5.b Distribucion km operativos inoperativos por comuna
- 6.a Distribucion km ciclovias operativos inoperativos
- 6.b Distribucion km ciclovias operativos inoperativos
- 7.a Distribucion km tipo inoperatividad
- 7.b Distribucion km tipo inoperatividad comuna
- 8.a Distribucion km tipo inoperatividad cruces
- 8.b Distribucion km tipo inoperatividad cruces comuna

### Mapas

- Mapa calidad decreto
- Mapa calidad merkuria
- Mapa calidad normativa
- Mapa calidad objetivo

## Descripción de Funciones Principales

1. **Preparación de datos**
   - `df <- GetDataReady(df)`: Prepara los datos leyendo un archivo shapefile y generando un archivo CSV con inconsistencias de datos que deben ser revisadas y limpiadas manualmente.

2. **Estadísticas descriptivas**
   - `GetBasicAnalysis(df)`: Genera tablas y gráficos estadísticos básicos a partir de los datos del shapefile limpio.

3. **Análisis de Calidad**
   - `GetQualityAnalysisPaper(df)`: Realiza un análisis de calidad según la norma y genera un mapa correspondiente.
   - `GetQualityAnalysisPObj(df)`: Realiza un análisis de calidad sin adherirse a la norma y genera un mapa objetivo.
   - `GetQualityAnalysisMerkuria(df)`: Realiza un análisis de calidad basado en el estándar Merkuria y genera un mapa correspondiente.
   - `GetQualityDecreto(df)`: Realiza un análisis de calidad basado en el estándar Decreto y genera un mapa correspondiente.

4. **Mapas**
   - `MapaCalidadNormativa(df)`: Genera un mapa basado en la calidad normativa.
   - `MapaCalidadObjetivo(df)`: Genera un mapa de calidad objetivo.
   - `MapaCalidadMerkuria(df)`: Genera un mapa de calidad Merkuria.