import pandas as pd

df = pd.read_csv("files/DowJonesIndustrialAverageHistoricalData.csv")

with open("files/f_data.txt", "w") as file:
    for value in df["Low"]:
        file.write(str(value).replace(',', '') + "\n")
