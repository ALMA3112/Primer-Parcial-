import Data.List (sortBy)
import Data.Ord (comparing)

ordenarEstudiantes :: [(String, Int)] -> [(String, Int)]
ordenarEstudiantes = sortBy (\(name1, grade1) (name2, grade2) ->
    compare grade2 grade1 <> compare name1 name2)

students = [("Ana", 85), ("Luis", 90), ("Carlos", 85), ("Beto", 90), ("David", 95)]
main = print (ordenarEstudiantes students)
