# -*- coding: utf-8 -*-
"""
Created on Tue Jul  9 16:08:15 2013

@author: mansmagnusson
"""

# Ladda ner XML-filer från riksdagen och sparar som txt.
# URL som innehåller information om vad som kn laddas ned.
# starturl = "http://data.riksdagen.se/dokumentlista/?rm=&typ=mot&d=&ts=&sn=&parti=&iid=&bet=&org=&kat=&sz=200&sort=c&utformat=xml&termlista=&p=1"
from os import chdir

project_path = "/Users/mansmagnusson/Desktop/Text Mining/Project/TextMiningProject2013"
chdir(project_path)

runfile(project_path+r'/Code/functions.py', wdir=project_path)
anf = RiksdagenApi("anförande")
anf.set_query("sz",100000)
anf.set_nodes_to_download("all")


for year in ["2010%2F11","2011%2F12","2012%2F13","2013%2F14"]:
    print year[0:4]
    anf.set_query("rm",year)
    anf.download_metadata("/Data/AnfMetadata"+year[0:4]+".csv")

for year in ["2010%2F11","2011%2F12","2012%2F13","2013%2F14"]:
    print year[0:4]
    anf.set_query("rm",year)
    anf.download_txt_files(quiet=False)


#anf.set_query("rm","2010%2F11")
#anf.download_txt_files(quiet=False)