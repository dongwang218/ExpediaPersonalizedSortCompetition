# srch_id, score
# evaluate avg ndcg@38
# cut -d',' -f1,52,54 ../data/train.csv | ruby -e 'while gets; a = chomp.split(","); score = a[2].to_i*5 + a[1].to_i; score = score > 5 ? 5 : score; puts "#{a[0]},#{score}"; end' | python calc_ndcg.py

import sys
import csv
import math

k=38

keep_first_line = int(sys.argv[1]) if len(sys.argv) > 1 else 0

def dcg(rel):
    # rel: list of relevance
    d = 0
    length = min(k, len(rel))
    for i in range(length):
        d += (2**rel[i] -1) / math.log(i+1 + 1, 2)
    return d

def ndcg(rel):
    # rel: list of relevances
    irel = sorted(rel, reverse = True)
    i_dcg = dcg(irel)
    m_dcg = dcg(rel)
    if i_dcg <= 0.0:
        return 1.0
    else:
        return m_dcg / i_dcg

cur_srch_id = 0
scores = []
sum_ndcg = 0.0
num_srch = 0
reader = sys.stdin
if keep_first_line == 0:
    header = reader.next()
for one_line in reader:
    line = one_line.rstrip('\n').split(',')
    srch_id = line[0]
    if srch_id != cur_srch_id:
        if scores != []:
            num_srch += 1
            sum_ndcg += ndcg(scores)
        cur_srch_id = srch_id
        scores = []

    score = int(line[1])
    assert(score == 5 or score == 1 or score == 0)
    scores.append(score)

if scores != []:
    num_srch += 1
    sum_ndcg += ndcg(scores[:38])

print "avg ndcg %f, num srch %d\n" % (sum_ndcg/num_srch, num_srch)
