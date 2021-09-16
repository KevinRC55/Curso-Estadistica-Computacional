# Tareas a realizar

### Manejo de datos

* Edita el nombre de todas las columnas y cambia el orden de las mismas (como lo visto
en clase). Guárdalo con otro nombre, por ejemplo, datosordenados.txt. El formato del
archivo puede ser .csv,.txt,.xls,.dat
* Separa las variables cualitativas de las cuantitativas y guarda cada grupo con un nombre
distinto.

### Datos faltantes e imputación

* Revisa el contenido de la base de datos e indica si tienes presencia de datos faltantes en alguna(s) variable(s). Puedes utilizar las funciones vistas en R para realizarlo.
* Identifica el tipo de patrón de datos faltantes con alguna de las técnicas discutidas en clase. Explica tus hallazgos lo más detallado posible.
* Utiliza el método de imputaci´on simple -con el criterio de la media- para completar la información. Guarda con otro nombre esta base de datos, ya que se utilizará más adelante.
* Si la base de datos está originalmente completa, entonces elige 2 ó 3 variables (cuantitativas) y realiza lo siguiente:
- Simula un patrón MCAR del 15% de valores faltantes en cada una.
- Verifica mediante un método gráfico o inferencial (de los estudiados en clase) que el patrón de faltantes es MCAR.
- Imputa -con el criterio de la media- los valores faltantes y guarda esta nueva base datos con otro nombre.
* A partir de la base de datos “incompleta” compara los siguientes métodos de imputación del paquete mice: sample, norm.nob, cart, rf. La comparación puedes hacerla con marginplot, o bien, utilizando la función summary para que obtengas el resumen estadístico de cada variable.
