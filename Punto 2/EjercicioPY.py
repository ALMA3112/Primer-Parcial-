def bubble_sort(students):
    n = len(students)
    for i in range(n):
        for j in range(0, n - i - 1):
            # Comparar por calificación (descendente) y luego por nombre (ascendente)
            if (students[j][1] < students[j + 1][1]) or \
               (students[j][1] == students[j + 1][1] and students[j][0] > students[j + 1][0]):
                students[j], students[j + 1] = students[j + 1], students[j]
    return students

# Lista de ejemplo: (nombre, calificación)
students = [
    ("Ana", 85),
    ("Carlos", 92),
    ("Beatriz", 78),
    ("David", 92),
    ("Elena", 89)
]

sorted_students = bubble_sort(students)
print("Estudiantes ordenados:")
for student in sorted_students:
    print(f"{student[0]}: {student[1]}")
