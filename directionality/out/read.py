import gzip
try:
    import simplejson as json
except ImportError, e:
    import json
    print e
from pprint import pprint

# For inspecting results

links_files = gzip.open("roadlinks.json.gz")
links = json.load(links_files)

print(links[0])
