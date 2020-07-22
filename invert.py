import numpy as np
import ast 

try:
    f = open("file.txt", "r")
    mat = f.read()
    f.close()
    m = np.matrix(ast.literal_eval(mat))
    inverted = np.linalg.inv(m)
    f = open("file.txt", "w")
    f.write(str(inverted))
    f.close()
except:
    print("error")