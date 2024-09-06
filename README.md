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

 ## Punto 2

 ## Punto 3
 
 - Paradigma declarativo:
  ```haskell
  -- Definición del tipo Estudiante
    data Estudiante = Estudiante {
    nombre :: String,
    apellido :: String,
    edad :: Int,
    identificacion :: String,
    calificaciones :: [Int]
    } deriving (Show)

  -- Función para crear un estudiante
    crearEstudiante :: String -> String -> Int -> String -> [Int] -> Estudiante
    crearEstudiante nom ape ed ident califs = Estudiante nom ape ed ident califs

  -- Función para agregar un estudiante a la lista de estudiantes
    agregarEstudiante :: [Estudiante] -> Estudiante -> [Estudiante]
    agregarEstudiante lista estudiante = lista ++ [estudiante]

  -- Función para eliminar un estudiante por ID
    eliminarEstudiante :: [Estudiante] -> String -> [Estudiante]
    eliminarEstudiante lista idEstudiante = filter (\est -> identificacion est /= idEstudiante) lista

  -- Función para imprimir un estudiante  
    imprimirEstudiante :: Estudiante -> String
    imprimirEstudiante est = "Nombre: " ++ nombre est ++ " " ++ apellido est ++ ", Edad: " ++ show (edad est)
    ++ ", ID: " ++ identificacion est ++ ", Calificaciones: " ++ show (calificaciones est)

  -- Función para imprimir la lista de estudiantes
    imprimirListaEstudiantes :: [Estudiante] -> IO ()
    imprimirListaEstudiantes lista = mapM_ (putStrLn . imprimirEstudiante) lista

  -- Función para calcular el uso de memoria de un estudiante (estimado)
    usoMemoriaEstudiante :: Estudiante -> Int
    usoMemoriaEstudiante est = length (nombre est) + length (apellido est) + length (identificacion est) + length (calificaciones est) * 4

  -- Función para imprimir el uso de memoria de cada estudiante
    imprimirUsoMemoriaEstudiantes :: [Estudiante] -> IO ()
    imprimirUsoMemoriaEstudiantes lista = mapM_ (\est -> putStrLn ("Estudiante " ++ nombre est ++ " " ++ apellido est
    ++ ": " ++ show (usoMemoriaEstudiante est) ++ " bytes")) lista

  main :: IO ()
  main = do
    -- Crear estudiantes
    let estudiante1 = crearEstudiante "Carlos" "Cardona" 19 "12345678" [85, 90, 78]
    let estudiante2 = crearEstudiante "Julian" "Briñez" 19 "87654321" [20, 35, 10]
    let estudiante3 = crearEstudiante "Santiago" "Ruiz" 19 "10203040" [95, 100, 100]
    
    -- Lista de estudiantes inicial
    let listaEstudiantes = agregarEstudiante [] estudiante1
    let listaEstudiantes2 = agregarEstudiante listaEstudiantes estudiante2
    let listaEstudiantes3 = agregarEstudiante listaEstudiantes2 estudiante3

    -- Imprimir lista de estudiantes
    putStrLn "Lista de Estudiantes:"
    imprimirListaEstudiantes listaEstudiantes3

    -- Imprimir uso de memoria de cada estudiante
    putStrLn "\nUso de Memoria de Cada Estudiante:"
    imprimirUsoMemoriaEstudiantes listaEstudiantes3
  ```
- Paragigma imperativo:
  ```C
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
  ```
- Conclusiones: 
 * Paralelismo y Concurrencia: En términos de paralelismo y concurrencia, el lenguaje C destaca por su eficiencia en velocidad y control. Sin embargo, esta ventaja viene con una mayor complejidad en su manejo, lo que puede hacer que sea más difícil de trabajar. Por otro lado, Haskell ofrece un enfoque más amigable para la concurrencia y la seguridad debido a su inmutabilidad y funciones puras. Esto facilita la creación de programas concurrentes, pero puede resultar en un rendimiento más lento debido a la gestión automática de la memoria y la sobrecarga del recolector de basura.


  * Gestión de Memoria: C proporciona un control detallado sobre la gestión de la memoria, permitiendo optimizaciones de bajo nivel que pueden mejorar el rendimiento. No obstante, este control manual también conlleva riesgos como las fugas de memoria y errores similares. En contraste, Haskell maneja la memoria automáticamente mediante un recolector de basura, lo que simplifica el desarrollo del código y reduce los errores relacionados con la memoria. Sin embargo, esto puede introducir una sobrecarga adicional que afecta el rendimiento.

  * Optimización: Cuando se trata de optimización, C ofrece la capacidad de realizar ajustes específicos y detallados, lo que puede llevar a un código altamente optimizado pero también más complejo. Haskell, en cambio, se apoya en el compilador para realizar optimizaciones, lo que simplifica el desarrollo y reduce la necesidad de ajustes manuales. Aunque esto hace que el código sea más fácil de escribir y mantener, limita la flexibilidad en la optimización específica para casos concretos.

  * Complejidad del Código: El código en C tiende a ser más extenso y complicado debido a la necesidad de gestionar manualmente la memoria y controlar el flujo de ejecución. Esto puede aumentar la probabilidad de errores. Haskell, con su enfoque en funciones puras e inmutabilidad, produce código más compacto y manejable. Este estilo declarativo facilita el mantenimiento y la comprensión del código, a pesar de que la gestión de memoria automática y otros aspectos del lenguaje pueden introducir alguna complejidad adicional.

  * Rendimiento General: En general, C suele ofrecer un rendimiento superior para aplicaciones que requieren un control preciso y un uso eficiente de los recursos. Es ideal para situaciones donde la velocidad es crucial. Haskell, aunque más seguro y fácil de manejar para programas concurrentes, puede tener un rendimiento inferior debido a la sobrecarga de su sistema de gestión automática de memoria. Sin embargo, ofrece ventajas significativas en términos de seguridad y facilidad de desarrollo.



 
