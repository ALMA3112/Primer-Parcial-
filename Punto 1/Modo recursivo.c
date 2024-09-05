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

