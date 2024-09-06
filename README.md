# Parcial 
  ## Punto 1
  - Resuelto de forma iterativa

  ```C

    #include <stdio.h>
    
    unsigned long long factorial_iterativo(int n) {
    unsigned long long resultado = 1;
    for (int i = 2; i <= n; i++) {
        resultado *= i;
        }
    return resultado;
    }

    int main() {
    int numero;
    printf("Ingrese un número entero: ");
    scanf("%d", &numero);
    
    if (numero < 0) {
        printf("El factorial no está definido para números negativos.\n");
        } else {
        printf("El factorial iterativo de %d es: %llu\n", numero, factorial_iterativo(numero));
        }
    return 0;
    }
  ```
- Resuelto de forma recursiva

  ```C
    #include <stdio.h>

  unsigned long long factorial_recursivo(int n) {
    if (n == 0 || n == 1) {
        return 1;
    } else {
        return n * factorial_recursivo(n - 1);
    }
  }

  int main() {
    int numero;
    printf("Ingrese un número entero: ");
    scanf("%d", &numero);
    
    if (numero < 0) {
        printf("El factorial no está definido para números negativos.\n");
    } else {
        printf("El factorial recursivo de %d es: %llu\n", numero, factorial_recursivo(numero));
    }
    return 0;
  }
  ```
- Comparacion: Ambas formas de calcular el factorial —iterativa y recursiva— son eficientes y rápidas, pero la versión iterativa es notablemente más eficiente en términos de uso de memoria. Esto se debe a que no necesita realizar múltiples llamadas a funciones para completar el cálculo. En cambio, la versión recursiva realiza una llamada a la función por cada nivel de cálculo, lo que puede llevar a un mayor uso de memoria y a un mayor riesgo de fallo o desbordamiento de la pila, especialmente para valores grandes de entrada.
- Mejora del programa.
  ```C
    #include <stdio.h>

    // Implementación del factorial usando programación dinámica
    unsigned long long factorial_dinamico(int n) {
    unsigned long long tabla[n + 1];
    
    // Caso base
    tabla[0] = 1;
    
    // Rellenamos la tabla con los valores de factoriales 
    for (int i = 1; i <= n; i++) {
        tabla[i] = i * tabla[i - 1];
    }
    
    return tabla[n];
    }

    int main() {
    int numero;
    printf("Ingrese un número entero: ");
    scanf("%d", &numero);
    
    if (numero < 0) {
        printf("El factorial no está definido para números negativos.\n");
    } else {
        printf("El factorial dinámico de %d es: %llu\n", numero, factorial_dinamico(numero));
    }
    return 0;
    }
  ```
 En este enfoque de programación dinámica, almacenamos los resultados de los cálculos intermedios del factorial en un arreglo, evitando así realizar el mismo cálculo más de una vez. Esto no solo reduce significativamente el tiempo de ejecución, sino que también disminuye la complejidad del cálculo del factorial.
