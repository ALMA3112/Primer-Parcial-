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
  

 
