## Python script from Kristen (Axom)

from erddapy import ERDDAP
import pandas as pd

# erddapy: https://github.com/ioos/erddapy
# https://ioos.github.io/erddapy/00-quick_intro-output.html
server = "https://erddap.secoora.org/erddap"
e = ERDDAP(
  server=server,
  protocol="tabledap",
  response="csvp",
)

# get all dataset_ids
# have to do it this way despite it sounding like you
# can use "allDatasets"
e.dataset_id = "allDatasets"
e.variables = [
  "datasetID",
]

datasets = e.to_pandas().dropna()

# get locations for all datasets
e.variables = [
  "latitude",
  "longitude",
]

rows = []

for dataset_id in datasets["datasetID"]:
  print(dataset_id)
if dataset_id == "allDatasets":
  continue
e.dataset_id = dataset_id
try:
  df = e.to_pandas().dropna()
except:
  df = pd.DataFrame(columns=["latitude (degrees_north)", "longitude (degrees_east)"],
                  data=[[None, None]])

rows.append([dataset_id, *df.values[0]])

df = pd.DataFrame(rows, columns=["dataset_id", "latitude (degrees_north)", "longitude (degrees_east)"])  


# df.to_csv("locations.csv", index=False)
