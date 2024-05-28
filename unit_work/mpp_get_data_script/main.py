import re
import pandas as pd

df = pd.read_csv("files/DowJonesIndustrialAverageHistoricalData.csv")

with open("files/f_data.txt", "w") as file:
    for value in df["Low"]:
        file.write(str(value).replace(',', '') + "\n")


def open_and_prepare_data(filepath):
    with open(filepath, "r") as file:
        lines = file.read()
    lines = re.sub(r'[^\d\s]', '', lines)
    lines = re.sub(r'\s+', ' ', lines)
    list_matrix = list(lines.strip())
    list_matrix.append(' ')
    row = []
    _str = ""
    for i in list_matrix:
        if i != ' ':
            flag = True
        else:
            row.append(int(_str))
            flag = False
            _str = ""
        if flag:
            _str += i
    return row


def open_and_prepare_data_for_prolog(filepath):
    with open(filepath, "r") as file:
        lines = file.read()
    lines = re.sub(r'[^\d]', ' ', lines)
    lines = re.sub(r'\s+', ' ', lines)
    list_matrix = list(lines.strip())
    list_matrix.append(' ')
    row = []
    _str = ""
    for i in list_matrix:
        if i != ' ':
            flag = True
        else:
            row.append(int(_str))
            flag = False
            _str = ""
        if flag:
            _str += i
    return row


def to_matrix(_list):
    matrix = []
    row = []
    count = 0
    for i in range(0, 26):
        for j in range(0, 26):
            row.append(_list[j + count])
        matrix.append(row)
        row = []
        count += 26
    return matrix


def print_mx(_matrix):
    for row in _matrix:
        print(row)


def first_formula(_matrix1, _matrix2):
    summ = 0
    _len = len(_matrix1)
    if _len != len(_matrix2):
        return False
    for i in range(0, _len):
        for j in range(0, _len):
            summ += _matrix1[i][j] - _matrix2[i][j]
    return summ


c_matrix = to_matrix(open_and_prepare_data("files/result_files/c_result.txt"))
lisp_matrix = to_matrix(open_and_prepare_data("files/result_files/lisp_result.txt"))
r_matrix = to_matrix(open_and_prepare_data("files/result_files/r_result.txt"))
prolog_matrix = to_matrix(open_and_prepare_data_for_prolog("files/result_files/prolog_result.txt"))
clips_matrix = to_matrix(open_and_prepare_data("files/result_files/clips_result.txt"))

# print_mx(c_matrix)

print(first_formula(c_matrix, lisp_matrix))
