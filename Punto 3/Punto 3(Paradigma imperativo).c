#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Definir estructura de estudiante
typedef struct {
    char* nombre;
    char* apellido;
    unsigned short edad;      // Usamos un short para ahorrar espacio
    char* id;                 // ID almacenado como string
    int* calificaciones;      // Puntero dinámico para calificaciones
    size_t num_calificaciones; // Número de calificaciones
} Estudiante;

// Lista dinámica de estudiantes
typedef struct {
    Estudiante** lista;       // Array de punteros a Estudiante
    size_t num_estudiantes;   // Número de estudiantes en la lista
} ListaEstudiantes;

// Función para crear un estudiante dinámicamente
Estudiante* crearEstudiante(const char* nombre, const char* apellido, unsigned short edad, const char* id, int* calificaciones, size_t num_calificaciones) {
    Estudiante* nuevoEstudiante = (Estudiante*) malloc(sizeof(Estudiante));
    
    // Asignar memoria para nombre y apellido
    nuevoEstudiante->nombre = (char*) malloc(strlen(nombre) + 1);
    nuevoEstudiante->apellido = (char*) malloc(strlen(apellido) + 1);
    strcpy(nuevoEstudiante->nombre, nombre);
    strcpy(nuevoEstudiante->apellido, apellido);

    // Asignar memoria para el ID
    nuevoEstudiante->id = (char*) malloc(strlen(id) + 1);
    strcpy(nuevoEstudiante->id, id);
    
    // Guardar edad
    nuevoEstudiante->edad = edad;

    // Asignar memoria para calificaciones
    nuevoEstudiante->calificaciones = (int*) malloc(num_calificaciones * sizeof(int));
    for (size_t i = 0; i < num_calificaciones; i++) {
        nuevoEstudiante->calificaciones[i] = calificaciones[i];
    }

    nuevoEstudiante->num_calificaciones = num_calificaciones;

    return nuevoEstudiante;
}

// Función para eliminar estudiante y liberar memoria
void eliminarEstudiante(Estudiante* estudiante) {
    free(estudiante->nombre);
    free(estudiante->apellido);
    free(estudiante->id);
    free(estudiante->calificaciones);
    free(estudiante);
}

// Función para agregar un estudiante a la lista
void agregarEstudiante(ListaEstudiantes* lista, Estudiante* estudiante) {
    lista->num_estudiantes++;
    lista->lista = (Estudiante**) realloc(lista->lista, lista->num_estudiantes * sizeof(Estudiante*));
    lista->lista[lista->num_estudiantes - 1] = estudiante;
}

// Función para imprimir un estudiante
void imprimirEstudiante(Estudiante* estudiante) {
    printf("Nombre: %s %s, Edad: %d, ID: %s, Calificaciones: [", estudiante->nombre, estudiante->apellido, estudiante->edad, estudiante->id);
    for (size_t i = 0; i < estudiante->num_calificaciones; i++) {
        printf("%d", estudiante->calificaciones[i]);
        if (i < estudiante->num_calificaciones - 1) {
            printf(", ");
        }
    }
    printf("]\n");
}

// Función para imprimir todos los estudiantes
void imprimirListaEstudiantes(ListaEstudiantes* lista) {
    printf("\nLista de Estudiantes:\n");
    for (size_t i = 0; i < lista->num_estudiantes; i++) {
        imprimirEstudiante(lista->lista[i]);
    }
}

// Función para calcular el uso de memoria de un estudiante
size_t calcularUsoMemoriaEstudiante(Estudiante* estudiante) {
    return sizeof(Estudiante)
        + strlen(estudiante->nombre) + 1
        + strlen(estudiante->apellido) + 1
        + strlen(estudiante->id) + 1
        + estudiante->num_calificaciones * sizeof(int);
}

// Función para imprimir el uso de memoria de cada estudiante
void imprimirUsoMemoriaEstudiantes(ListaEstudiantes* lista) {
    printf("\nUso de Memoria de Cada Estudiante:\n");
    for (size_t i = 0; i < lista->num_estudiantes; i++) {
        size_t memoria_usada = calcularUsoMemoriaEstudiante(lista->lista[i]);
        printf("Estudiante %s %s: %zu bytes\n", lista->lista[i]->nombre, lista->lista[i]->apellido, memoria_usada);
    }
}

// Función para liberar toda la memoria usada por la lista de estudiantes
void liberarListaEstudiantes(ListaEstudiantes* lista) {
    for (size_t i = 0; i < lista->num_estudiantes; i++) {
        eliminarEstudiante(lista->lista[i]);
    }
    free(lista->lista);
}

int main() {
    // Inicializar lista de estudiantes
    ListaEstudiantes lista = {NULL, 0};

    // Ejemplo de calificaciones
    int calificaciones1[] = {85, 90, 78};
    int calificaciones2[] = {20, 35, 10};
    int calificaciones3[] = {95, 100, 100};
    
    // Crear estudiantes
    Estudiante* estudiante1 = crearEstudiante("Carlos", "Cardona", 19, "12345678", calificaciones1, 3);
    Estudiante* estudiante2 = crearEstudiante("Julian", "Briñez", 19, "87654321", calificaciones2, 3);
     Estudiante* estudiante3 = crearEstudiante("Santiago", "Ruiz", 19, "10203040", calificaciones3, 3);

    // Agregar estudiantes a la lista
    agregarEstudiante(&lista, estudiante1);
    agregarEstudiante(&lista, estudiante2);
     agregarEstudiante(&lista, estudiante3);

    // Imprimir lista de estudiantes
    imprimirListaEstudiantes(&lista);

    // Imprimir uso de memoria de cada estudiante
    imprimirUsoMemoriaEstudiantes(&lista);

    // Liberar memoria usada por la lista
    liberarListaEstudiantes(&lista);

    return 0;
}
