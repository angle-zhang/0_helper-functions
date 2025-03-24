import osmnx as ox
import osmium
import os
import datetime


#restart R session: session > restart R
#reticulate::use_python("/usr/bin/python3")

# to access C:/ use "/mnt/c"

ox.settings.all_oneway=True

graph = ox.graph_from_place("Los Angeles County, California", network_type="drive", simplify=False)

ox.plot(cleaned_graph)
cleaned_graph = ox.consolidate_intersections(graph, rebuild_graph=True, tolerance=5, dead_ends=False)

# Change this to your desired download directory.
base_path = "/mnt/c/Users/angie/OneDrive/Desktop/data-analysis/0_shared-data/osm"

time=datetime.datetime.now().strftime("%d_%b_%Y")

filename = "osm_los_angeles_" + time
os.chdir(base_path)

ox.io.save_graph_xml(graph, filename + ".osm")

print("writing pbf file", filename)
# with open(filename, 'wb') as file: 
#   file.write(projected_graph)
# file.close()

# convert file to pbf from graph ml
with osmium.SimpleWriter(filename + '.osm.pbf') as  writer:
    for o in osmium.FileProcessor(filename+'.osm.xml'):
        writer.add(o)
writer.close()
