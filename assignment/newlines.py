import re

file_path = "C:\\Users\\north\\Desktop\\Uni\\Spring\\Logic_Programming\\Ergasia1\\assignment\\assignment.out"

with open(file_path, 'r') as file:
    input_str = file.read()

output_str = re.sub(r",(?![^[]*\])", "\n", input_str)

print(output_str)
