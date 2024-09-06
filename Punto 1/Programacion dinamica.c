#include <stdio.h>

// Implementación del factorial usando programación dinámica
unsigned long long factorial_dinamico(int n) {
    unsigned long long tabla[n + 1];

    // Caso base
    tabla[0] = 1;

    // Rellenamos la tabla con los valores de factoriales desde 1 hasta n
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


