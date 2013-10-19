# split into train test per srch_id
import sys
import csv
import random
import math
from features import train_features_map
from features import test_features_map

book_vs_click = int(sys.argv[1]) if len(sys.argv) > 1 else 5 # weight
add_tab = int(sys.argv[2]) if len(sys.argv) > 2 else 0
is_test = sys.argv[3] == 'test' if len(sys.argv) > 3 else False

features_map = train_features_map if not is_test else test_features_map

svm_file = sys.stdout

excluded_columns_names = ["srch_id", "date_time", "position", "random_bool", "click_bool", "gross_bookings_usd", "booking_bool"] # [1, 2, 15, 27, 52, 53, 54]
excluded_columns = [features_map[k] for k in excluded_columns_names if k in features_map]

click_col = features_map.get("click_bool", 0) # 51
book_col = features_map.get("booking_bool", 0) # 53
affinity_col = features_map.get("srch_query_affinity_score", 0) # 25

sep = '\t' if add_tab else ' '

reader = csv.reader(sys.stdin)
header = reader.next()

raw_features = 54
for line in reader:
    features = {}
    for i, v in enumerate(line):
        idx = i + 1
        if idx not in excluded_columns and v != 'NULL':
            if idx != affinity_col:
                features[idx] = float(v)
            else:
                features[idx] = math.pow(10, float(v))

    # add extra features
    # rating ratio
    vhr = features.get(features_map["visitor_hist_starrating"], 0)
    pr  = features.get(features_map["prop_starrating"], 0)
    if vhr != 0 and pr != 0 and pr != vhr:
        features[raw_features+1] = pr / vhr - 1
    # price ratio
    vhu = features.get(features_map["visitor_hist_adr_usd"], 0)
    pp = features.get(features_map["price_usd"], 0)
    if vhu != 0 and pp != 0 and pp != vhu:
        features[raw_features+2] = pp / vhu -1

    svml = ' '.join(['%s:%s' % (key, value) for (key, value) in sorted(features.iteritems())])
    target = 1 if (not is_test) and (line[book_col-1] == '1' or line[click_col-1] == '1') else -1
    row = "%d%s%s\n" % (target, sep, svml)

    if (not is_test) and line[book_col-1] == '1':
        weight = book_vs_click
    else:
        weight = 1
    svm_file.writelines([row] * weight)

