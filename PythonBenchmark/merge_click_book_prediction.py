# split into train test per srch_id
import sys
import csv
import random

def removeNonAscii(s): return "".join(filter(lambda x: ord(x)<128, s))

reader = sys.stdin
header = reader.next()
writer = sys.stdout
writer.write(removeNonAscii(header))

cur_srch_id = 0
ordered_property_id = []
property_value = {} # map id to score
for line in reader:
    line = removeNonAscii(line)

    fs = line.split(',')
    assert(fs[0] == fs[4])

    srch_id = int(fs[0])
    if srch_id != cur_srch_id:
        for id in ordered_property_id:
            score = property_value[id]
            writer.write("%d,%d,%d\n" % (cur_srch_id, score, id))
        cur_srch_id = srch_id
        ordered_property_id = []
        property_value = {}

    ordered_property_id.append(int(fs[5]))
    score = min(int(fs[3]) * 5 + int(fs[2]), 5)
    property_value[int(fs[1])] = score

if ordered_property_id:
    for id in ordered_property_id:
        score = property_value[id]
        writer.write("%d,%d,%d\n" % (cur_srch_id, score, id))
