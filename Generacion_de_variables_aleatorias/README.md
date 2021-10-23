# Métodos para generación de variables aleatorias (Distribución triangular)

## Tareas a realizar

* *1.* Graficar la Función $F(x)$.
$$F(x)=[\frac{1}{2}(x-2)] \mathbb{I}_{[2,3]}(x)+[\frac{1}{2}(2-\frac{x}{3})] \mathbb{I}_{(3,6]}(x) $$

* *2.* Escribir el código para producir números aleatorios con la distribución $F(x)$.
$$x = 2+2\sqrt{u} \quad \textrm{ para } 0\leq{u}\leq{0.25}$$
$$x = 6-\sqrt{12-12u} \quad \textrm{ para } 0.25<u\leq{1}$$

* *3.* Dibujar el histograma de los valores simulados y sobre él graficar $F(x)$.

* *4.* Calcular el $E[X]$, $Var(x)$ teóricos (a mano o en R), compararlos con los correspondientes valores simulados.

* *5.* Comparar sus valores simulados con los obtenidos directamente en R.
