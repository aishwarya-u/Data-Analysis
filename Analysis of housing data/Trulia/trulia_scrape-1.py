from bs4 import BeautifulSoup
import requests
import pandas as pd
import csv
import os


# Fetch html from url
def fetchhtml(url):
    pretend_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:11.0) Gecko/20100101 Firefox/11.0'
    content = requests.get(url, headers={'User-Agent': pretend_agent}).content
    soup = BeautifulSoup(content, "lxml")
    return soup


# get next page
def geturl(url,page):
    url += str(page) + "_p"
    return url


def get_data(county):
    # start of page
    count = 10

    # Number of pages
    limit_page = 11

    # List of addresses
    home_addresses = []

    # default pattern of url
    url = "https://www.trulia.com/sold/" + county + "/"

    while(count < limit_page):
        # print(geturl(url,count))

        # For every page get next page
        soup = fetchhtml(geturl(url,count))

        homelink = ["https://www.trulia.com" + x.get("href") for x in soup.find_all('a', {'class': "tileLink"})]

        # Remove duplicate homes from list
        homelink = list(set(homelink))
        for link in homelink:
            print(link.strip())
            user = fetchhtml(link.strip())

            try:
                address = user.find_all('div', {'class': "h3 typeReversed pvs"})
                # Some homes do not disclose addresses so checking if address found
                if address:
                    address = address[0].text.replace("\n", "").strip()
                    city = address[17:].split(",")[1]
                    street = address[17:].split(",")[0]
                    garage = user.find_all('ul', {'class': "listBulleted mbn"})[0].text.replace("\n", "").strip().find("garage")
                    fireplace = user.find_all('span', {'id': "corepropertydescription"})[0].text.replace("\n", "").strip().find("fire")
                    pool = user.find_all('span', {'id': "corepropertydescription"})[0].text.replace("\n", "").strip().find("pool")
                    centralAir = user.find_all('span', {'id': "corepropertydescription"})[0].text.replace("\n", "").strip().find("Air Condition")

                    # Flags
                    isgarage = 'n'
                    ispool = 'n'
                    isfireplace = 'n'
                    isaircond = 'n'

                    if(garage):
                        isgarage = 'y'
                    if (pool):
                        ispool = 'y'
                    if (fireplace):
                        isfireplace = 'y'
                    if (centralAir):
                        isaircond = 'y'

                    # Separator "|" is used to separate the data
                    home_addresses.append(street + "|" + city + "|" + isgarage + "|" + ispool + "|" + isfireplace + "|" + isaircond)
            except Exception:
                print("Error scraping data from website")
        count+=1
    write_street_address(home_addresses, county)


# Write data to CSV files
def write_street_address(addresses, county):
    filename = "trulia-street-addresses-"+county+".csv"
    path = os.getcwd() + '/more_trulia_housing_data'
    fullpath = os.path.join(path, filename)
    try:
        with open(fullpath, "w") as street_file:
              for home in addresses:
                street=home.split('|')[0]
                city = home.split('|')[1]
                garage = home.split('|')[2]
                pool = home.split('|')[3]
                fireplace = home.split('|')[4]
                aircond = home.split('|')[5]
                writer=csv.writer(street_file)
                writer.writerow([street,city,garage,pool,fireplace,aircond])
    except Exception:
        print("Error writing data to file.")

# List of counties from which data is scraped
counties = {
    "bergen": "34003_c",
    "essex": "34013_c",
    "hudson": "34017_c",
    "morris": "34027_c",
    "passaic": "34031_c",
    "union": "34039_c",
    "warren": "34041_c"
}

for county, county_id in counties.items():
    get_data(county_id)