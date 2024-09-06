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
