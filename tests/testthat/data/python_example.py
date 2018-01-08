import pandas
import feather

# Read flights data and select flights to O'Hare
flights = pandas.read_csv("tests/testthat/data/flights.csv")
flights = flights[flights['dest'] == "ORD"]

# Select carrier and delay columns and drop rows with missing values
flights = flights[['carrier', 'dep_delay', 'arr_delay']]
flights = flights.dropna()
flights.head(10)

# Write to feather file for reading from R
feather.write_dataframe(flights, "tests/testthat/data/flights.feather")
