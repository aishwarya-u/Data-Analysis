import folium
import csv
import matplotlib.pylab as plt
import collections

csvFiledata = []
with open('newhousedata-2.csv',newline='') as csvFile:
    reader = csv.reader(csvFile,delimiter=',')
    for row in reader:
        csvFiledata.append(row)

headers = csvFiledata[1]
maps_osm = folium.Map(location=[40.670938,-74.110006], zoom_start=13)

sampledData = []
csvFiledata = csvFiledata[1:]
for index, data in enumerate(csvFiledata):
    print(index, data[5],data[6])    
    folium.CircleMarker(location=[data[5],data[6]],
                        radius=5,
                        popup="{0},{1},{2},{3}".format(data[0],data[2],data[3],data[1]),
                        color='#ffff00').add_to(maps_osm)
    sampledData.append((data[4], float(data[16]),float(data[20])))

maps_osm.save('maptest.html')

sampledData.sort()

zipvalue = sampledData[0][0]
count = 0
totalCost = 0
totalSqFoot = 0
averageCost = {}
averageSqfoot = {}
for index, data in enumerate(sampledData):    
    count += 1
    totalCost += int(data[1])
    totalSqFoot += int(data[2])
    
    if ((index + 1) == len(sampledData) or ((index + 1) < len(sampledData) and sampledData[index + 1][0] != zipvalue)):
        averageCost[zipvalue] = totalCost / count
        averageSqfoot[zipvalue] = totalSqFoot / count
        zipvalue = None if (index + 1) == len(sampledData) else sampledData[index + 1][0]
        count = 0
        totalCost = 0
        totalSqFoot = 0

plt.bar(range(len(averageCost)),averageCost.values(),align='center')
plt.xticks(range(len(averageCost)),averageCost.keys())
plt.xlabel("County",fontsize=14, color='red')
plt.ylabel("Average Cost",fontsize=14, color='red')
plt.title("Average cost vs County")
plt.show()

plt.bar(range(len(averageSqfoot)),averageSqfoot.values(),align='center')
plt.xticks(range(len(averageSqfoot)),averageSqfoot.keys())
plt.xlabel("County",fontsize=14, color='red')
plt.ylabel("Average cost per square feet",fontsize=14, color='red')
plt.title("Average cost per square feet vs County")
plt.show()

