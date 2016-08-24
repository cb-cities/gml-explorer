import json
import gzip
from pprint import pprint
import glob

# one test, use glob to iterate over link file
links_files = gzip.open("../out/roadlinks1.json.gz")
links = json.load(links_files)

dif_files = gzip.open("./out/directedlinks.json.gz")
dif = json.load(dif_files)

print "Number of link files: ", len(links)
print "Number of restrictions (all types): ", len(dif)

print "Isolating One Way restrictions"

# Isolate One Way restrictions only

restrictions = []
for di in dif:
	if di['restriction'] == "One Way":
		restrictions.append(di)

print("Number of One Way restrictions: ", len(restrictions))

for link in links:
	directed = []
	undirected = []
	for restriction in restrictions:
		print restriction['toid'], link['toid']
		if restriction['toid'] == link['toid']:
			print "found one"
			directed.append(link)
		else:
			undirected.append(link)
	
# 	with gzip.open('./out/files/directed.json.gz','ab') as outfile:
# 		json.dump(directed,outfile,indent=3)
# 	with gzip.open('./out/files/undirected.json.gz','ab') as outfile:
# 		json.dump(undirected,outfile,indent=3)

# # generated_links = []

# # for undirected in undirected_pre:
# # 	old_link = undirected
# # 	negativeNode = undirected['positiveNode']
# # 	postivieNode = undirected['negativeNode']
# # 	toid = undirected['toid']
# # 	term = undirected['term']
# # 	# Reindex!
# # 	index = 1
# # 	nature = undirected['nature']
# # 	polyline = undirected['polyline'].reverse()
# # 	new_link = {'negativeNode': negativeNode,
# # 	'toid': toid,
# # 	'term': term,
# # 	'polyline': polyline,
# # 	'positiveNode': positiveNode,
# # 	'index': index,
# # 	'nature': nature}
# # 	generated_links.append(old_link)
# # 	generated_links.append(new_link)

# # with gzip.open('./out/files/links.json.gz', 'w') as outfile:
# # 	json.dump(generated_links, outfile, indent=3)