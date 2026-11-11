
# Tarea 3 - Herramientas para el Manejo de Datos I

**Autor:** Mariano Morales  
**Carné:** C5H565

## Descripción
Para la asistente: los parametros que me genero mi carne son los mismos a los genericos (7, 2), digo esto para evitar confusiones a la hora de revisar mi trabajo.
Proyecto correspondiente a la Tarea 3 del curso Herramientas de Ciencia de Datos I.

El proyecto desarrolla tres ejercicios mediante R y Excel:
1. Alertas móviles.
2. Análisis de la Ley de Benford.
3. Cálculo del valor acumulado de depósitos.

## Estructura del proyecto

- `base/`: archivos de datos originales.
- `cod/`: archivos de código R.
- `data/`: archivos de datos utilizados en los análisis.
- `config/`: parámetros de configuración.
- `excel/`: archivos de Excel.
- `info/figuras/`: gráficos generados.
- `README.md`: descripción e instrucciones del proyecto.


## Instrucciones para reproducir el análisis

1. Descargar o clonar este repositorio de GitHub.

2. Abrir la carpeta del proyecto en RStudio.

3. Desde la carpeta principal del proyecto, clonar la base del profesor:

   ```bash
   git clone https://github.com/PotoyJuarez/CA0204-Tarea3-base.git base
   ```

4. Ejecutar el archivo `cod/01_datos.R` para generar los datos
   correspondientes al carné C5H565.

5. Verificar los parámetros en `config/parametros_tarea3.csv`.

6. Abrir el archivo `.qmd` del proyecto y utilizar Render para
   generar el informe HTML.

7. Abrir el archivo Excel de la carpeta `excel/` y actualizar
   las consultas de Power Query cuando corresponda.

Los resultados de los tres ejercicios se presentan en el informe
HTML y en la hoja `Respuestas` del archivo de Excel.
