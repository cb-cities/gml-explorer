import simplejson as json
import gzip
from pprint import pprint
import glob

# new =[]
# new_json = []

# for file in glob.glob("./out/roadlinks*.json.gz"):
# 	files = gzip.open(file)
# 	results_1 = json.load(files)
# 	length = len(results_1)
# 	new.append(length)
# 	new_json.extend(results_1)

gen = []
gen_json = []

for file in glob.glob("./results/roadlinks*.json.gz"):
	files = gzip.open(file)
	results_2 = json.load(files)
	length = len(results_2)
	gen.append(length)
	gen_json.extend(results_2)
	for record in results_2:
		if len(record) != 10:
			print 'hmm'
		print record

# print(sum(new))
print(sum(gen))

# pprint(gen_json[0])

# pprint(new_json[0])
