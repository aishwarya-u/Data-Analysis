import requests, re
from bs4 import BeautifulSoup
import csv
import pandas as pd
import locale
from locale import atof

locale.setlocale(locale.LC_NUMERIC, '')

def fetchhtml(url):
    PRETEND_AGENT = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:11.0) Gecko/20100101 Firefox/11.0'
    content = requests.get(url, headers={'User-Agent': PRETEND_AGENT}).content
    soup = BeautifulSoup(content, "html.parser")
    return soup

def geturl(url,page):
    url = url + str(page) + '_p/'
    return url

def write_to_csv(data): 
    print("writing data to file...")
    with open('data-with-year-remaining.csv', 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerows(data)   

def scrapedata():
    count = 31
    home_inventory = []

    # For iterating through pages just set limitPage to number of pages you want to iterate
    limitpage = 70

    # Property count, will give you the total number of user properties scraped
    homecount = 1

    # Base url of the website
    url = "https://www.trulia.com/NJ/Jersey_City/"

    while (count < limitpage):
        soup = fetchhtml(geturl(url,count))

        # link of the page which we can scrape
        homelink = ["https://www.trulia.com" + x.get("href") for x in soup.find_all('a', {'class': "tileLink"})]

        # Total number of houses present in given link
        for link in homelink:
            house =  fetchhtml(link.strip())
            attributes = []
            home_detail = []

            # scraping from the heading of the house address
            street = house.find_all('span', {'class': "headingDoubleSuper h2 typeWeightNormal mvn ptn"})[0].text.replace("\n","").strip()
            address = house.find_all('span', {'class': "headlineDoubleSub typeWeightNormal typeLowlight man"})[0].text.replace("\n", "").strip()
            address = re.sub('\s+', ' ', address)
            city = address.split(",")[0]
            state = address.replace(" ","").split(",")[1][0:2]
            zip_code = address.replace(" ","").split(",")[1][2:7]

            # scraping these fields from feature description of the house at bottom of the page
            square_feet = 'n/a'
            year_built = 'n/a'
            
            year_built_input = house.find('input', {'id': "property_detail_year_built_org"})
            square_feet_input = house.find('input', {'id': "property_detail_sqft_org"})

            if(square_feet_input != None):
                square_feet = square_feet_input.get('value')

            if(year_built_input != None):
                year_built = year_built_input.get('value')
            
            
            listedBulletin = house.find('ul', {'class': "listBulleted mbn"})

            if(listedBulletin != None):
                details = listedBulletin.findAll('li')
            else:
                details = []

            for li in details:
                attributes.append(str(li).replace("<li>","")
                                        .replace("</li>","")
                                        .replace("Price: $",""))

            home_detail = [link, street, city, state, zip_code, square_feet, year_built]
            home_detail.extend(attributes)
            home_inventory.append(home_detail)

            homecount += 1
        count += 1

        # writing data to CSV file
        write_to_csv(home_inventory)


# cleaning data
def clean_data(home_csv):       
    home_data = pd.read_csv(home_csv, encoding='latin1', na_values='n/a')
    home_data['price'] = home_data[['price']].applymap(atof)
    home_data = home_data.dropna()
    home_data['square_feet'] = home_data[['square_feet']].applymap(atof)
    home_data.to_csv("home_detail-cleaned.csv", index=False)
    
    return

# cleaning data
def clean_data_with_year(home_csv):
    home_data = pd.read_csv(home_csv, encoding='latin1', na_values='n/a')
    home_data['price'] = home_data[['price']].applymap(atof)
    home_data = home_data.dropna()
    home_data.to_csv("home_detail-cleaned-year.csv", index=False)

if __name__=='__main__':
    # scrapedata()
    home_data_string = "data-with-year.csv"
    clean_data_with_year(home_data_string)