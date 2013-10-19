import data_io
import sys
import csv
from operator import itemgetter
import os
import json
import pickle
import pandas as pd

def main():
    print("Reading test data")
    test = pd.read_csv(sys.stdin)
    test.fillna(0, inplace=True)
    
    feature_names = list(test.columns)
    feature_names.remove("date_time")

    features = test[feature_names].values

    print("Loading the classifier")
    classifier = pickle.load(open(sys.argv[1]))

    print("Making predictions")
    predictions = classifier.predict_proba(features)[:,1]
    predictions = list(-1.0*predictions)
    recommendations = zip(test["srch_id"], test["prop_id"], predictions)

    print("Writing predictions to file")
    data_io.write_submission(recommendations, sys.argv[2])

if __name__=="__main__":
    main()
