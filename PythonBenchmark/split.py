# split into train test per srch_id
import sys
import csv
import random

ratio = float(sys.argv[1]) # ratio of the split
train_file = csv.writer(open(sys.argv[2], 'wb'))
test_file = csv.writer(open(sys.argv[3], 'wb'))

reader = csv.reader(sys.stdin)
header = reader.next()
train_file.writerow(header)
test_file.writerow(header)

random.seed(1)

cur_srch_id = 0
choice_train = True
for line in reader:
    srch_id = line[0]
    if srch_id != cur_srch_id:
        cur_srch_id = srch_id
        choice_train = random.random() <= ratio

    if choice_train:
        train_file.writerow(line)
    else:
        test_file.writerow(line)

