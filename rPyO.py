import OriginExt as origin
import os, os.path
import csv
import pandas as pd
import sys

app = origin.Application()
app.Visible = 1

root = sys.argv[1]
print(sys.argv[1])
cells = [d for d in os.listdir(root) if os.path.isdir(os.path.join(root, d))]

for cell in cells:
    if cell[:7] != "Channel":
        cells.remove(cell)

files = [d for d in os.listdir(root) if os.path.isfile(os.path.join(root, d))]

for file in files:
    if file[-3:] != "csv":
        files.remove(file)

proj = app.CreatePage(app.OPT_WORKSHEET, "SummaryData")
wbook = app.Pages(proj)

for i in files:
    tmpData = pd.read_csv(root + "/" + i, header=0, encoding = "ISO-8859-1")

    wks = wbook.Layers.Add(i)
    wks.SetCols(tmpData.shape[1])
    for n, name in enumerate(tmpData.columns):
        col = wks.Columns(n)
        col.LongName = tmpData.columns[n]
        col.SetData(tmpData[name].values)

proj = app.CreatePage(app.OPT_WORKSHEET, "Cells-FullData")
wbook = app.Pages(proj)

for i in cells:
    tmpData = pd.read_csv(root + "/" + i + "/" + i + ".csv", header=0, encoding = "ISO-8859-1")

    wks = wbook.Layers.Add(i)
    wks.SetCols(tmpData.shape[1])
    for n, name in enumerate(tmpData.columns):
        col = wks.Columns(n)
        col.LongName = tmpData.columns[n]
        col.SetData(tmpData[name].values)