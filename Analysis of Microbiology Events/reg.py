import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.ticker as tkr
from sklearn.linear_model import LinearRegression
import numpy as np

def run_regression_org_results(filename):
    
    microbiology_data = pd.read_csv(filename, encoding="latin1")
    
    org = microbiology_data[['org_id']]
    spec = microbiology_data[['spec_id']]
    
    model = LinearRegression().fit(org, spec)
    
    
    
    
    #m = model.coef[0]
    #b = model.intercept_
    #print("formula y = {0}x + {1}".format(m,b))
    
    ax = plt.subplot()
    plt.xticks(fontsize= 6)
    plt.yticks(fontsize= 6)
    
    plt.scatter(org, spec , color="purple", s=10)
    
    plt.plot(org, model.predict(spec), color='black', linewidth=1)

    plt.title("Organism over Result")
    plt.ylabel("Spec")
    plt.xlabel("Org")

    plt.show()
    
    

if __name__=='__main__':

    microbiology_data_string = "data.csv"
    run_regression_org_results(microbiology_data_string)

