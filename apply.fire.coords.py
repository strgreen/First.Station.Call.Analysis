import os
import sys
import csv

with open('fire.station.coords.csv', 'r') as csv_file:
	dataTable = csv.reader(csv_file)
	fireDict = {}
	for row in dataTable:
		fireDict[row[0]] = row[1:]
		

with open('sing.fire.data.v3.csv', 'r') as csv_file:
	dataTable = []
	row = 0
	next(csv_file)
	for line in csv_file:
		temp = line.strip().split(',')
		dataTable.append(temp[0:])
		coords  = fireDict[dataTable[row][12]]
		dataTable[row].append(coords[0])
		dataTable[row].append(coords[1])
		print("##################")
		print(dataTable[row])
		print("##################")
		
		row = row + 1
		
		#for index in
		
		
		#dataTable.append(line.strip().split(','))
		#print(len(dataTable))