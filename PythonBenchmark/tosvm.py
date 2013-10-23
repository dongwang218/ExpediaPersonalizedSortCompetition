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
only_action = sys.argv[4] == 'action' if len(sys.argv) > 4 else False
historical = sys.argv[5] if len(sys.argv) > 5 else ""
regression = sys.argv[6] == 'regression' if len(sys.argv) > 6 else False

features_map = train_features_map if not is_test else test_features_map

svm_file = sys.stdout

excluded_columns_names = ["srch_id", "date_time", "position", "random_bool", "click_bool", "gross_bookings_usd", "booking_bool"] # [1, 2, 15, 27, 52, 53, 54]
if historical != "":
    # remove this prop_id
    excluded_columns_names.append("prop_id")

excluded_columns = [features_map[k] for k in excluded_columns_names if k in features_map]

click_col = features_map.get("click_bool", 0) # 51
book_col = features_map.get("booking_bool", 0) # 53
affinity_col = features_map.get("srch_query_affinity_score", 0) # 25
prop_id_col = features_map.get("prop_id", 0) # 8

sep = '\t' if add_tab else ' '

reader = csv.reader(sys.stdin)
header = reader.next()

raw_features = 54

default_ctr = 0.0447
default_conversion = 0.0279

def get_historical(hist_performance, prop_id):
  return hist_performance.get(prop_id, (default_ctr, default_conversion))

def read_historical(historical):
  hist = {}
  i_prior = 10
  if historical != "":
      hist_file = csv.reader(open(historical))
      for line in hist_file:
          id, i, c, b = map(float, line)
          hist[id] = ((c + default_ctr * i_prior) / (i + i_prior),
                      (b + default_conversion * i_prior) / (i + i_prior))
  return hist

hist_performance = read_historical(historical)

# test data is missing position in the middle so feature 15-50 should be 16-51
def renum_feature_id(id):
    if is_test and id >= 15 and id <= 50:
        return id+1
    else:
        return id

def emit_line(line):
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

    #historical
    if hist_performance:
        prop_id = int(line[prop_id_col-1])
        features[raw_features+3], features[raw_features+4] = get_historical(hist_performance, prop_id)
        #print "%d: %f %f" % (prop_id, features[raw_features+3], features[raw_features+4])

    svml = ' '.join(['%s:%s' % (renum_feature_id(key), value) for (key, value) in sorted(features.iteritems())])
    if regression:
        if is_test:
            target = 0
        elif line[book_col-1] == '1':
            target = 5
        elif line[click_col-1] == '1':
            target = 1
        else:
            target = 0
    else:
        target = 1 if (not is_test) and (line[book_col-1] == '1' or line[click_col-1] == '1') else -1
    row = "%d%s%s\n" % (target, sep, svml)

    if (not is_test) and line[book_col-1] == '1':
        weight = book_vs_click
    else:
        weight = 1
    svm_file.writelines([row] * weight)

def emit_lines(lines):
    # check if we have click or book
    if is_test or (not only_action) or ([l for l in lines if l[book_col-1] == '1' or l[click_col-1] == '1']):
        for l in lines:
            emit_line(l)

cur_srch_id = 0
lines = []
for line in reader:
    srch_id = line[0]
    if srch_id != cur_srch_id:
        if lines != []:
            emit_lines(lines)
        cur_srch_id = srch_id
        lines = []
    lines.append(line)

if lines != []:
    emit_lines(lines)
