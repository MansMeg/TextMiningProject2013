# -*- coding: utf-8 -*-
"""
Created on Tue Jul  9 16:08:15 2013

@author: mansmagnusson
"""
# Packages to use
import Queue
import os
from urllib import urlopen
import multiprocessing
import time
import random
import re
import nltk
import string
import threading
from HTMLParser import HTMLParser

import os
from lxml import etree
from urllib import urlopen
import csv

# Ladda ner XML-filer från riksdagen och sparar som txt.
# URL som innehåller information om vad som kn laddas ned.
starturl = "http://data.riksdagen.se/dokumentlista/?rm=&typ=mot&d=&ts=&sn=&parti=&iid=&bet=&org=&kat=&sz=10&sort=c&utformat=xml&termlista=&p=1"
os.chdir("/Users/mansmagnusson/Desktop/Text Mining/Project/TextMiningProject2013")
csvfile = 'Data/Motioner.csv'
links_to_download_file = 'Data/links_to_download_file.txt'
get_from_xml = ["dok_id","typ","subtyp","datum","titel","undertitel","dokument_url_text"]
url_text_xml = "dokument_url_text"

# Create files
if not os.path.exists(csvfile):
    newfile = open(csvfile, 'a')
    newfilewriter = csv.writer(newfile)
    newfilewriter.writerow(get_from_xml)
    newfile.close()
    
if not os.path.exists(links_to_download_file):
    to_dl_file = open(links_to_download_file, 'a').close()


# Loop över sidor (som uppdateras)
xmldoc = urlopen(starturl).read()
xmltree = etree.fromstring(xmldoc)
sidor_tot = xmltree.xpath('/dokumentlista/@sidor')[0]
sidor_tot = 3

url = starturl
pagecnt = 1
while(pagecnt <= sidor_tot):
    print str(pagecnt) + " of " + str(sidor_tot)
    pagecnt += 1 
    # Each page with data
    xmldoc = urlopen(url).read()
    xmltree = etree.fromstring(xmldoc)
    sida_no = xmltree.xpath('/dokumentlista/@sida')[0]
    #    if sida_no != str(pagecnt):
    #        print "ERROR: sida_no = " + str(sida_no) + " and pagecnt = " + str(pagecnt)
    #        break
    next_url = xmltree.xpath('/dokumentlista/@nasta_sida') # Parse the dokument files
    xmldocs = xmltree.xpath('/dokumentlista/dokument')
    # Create data lines
    xmldata = []
    list_to_download = []
    for doc in enumerate(xmldocs):
        # Create data line to csv-file
        nodelist = []
        for node in get_from_xml:
            nodelist.append(doc[1].xpath(node)[0].text)
        xmldata.append(nodelist)
        # Create list with files to download
        list_to_download.append(doc[1].xpath(url_text_xml)[0].text)
    with open(csvfile, 'a') as open_csvfile:
        csvwriter = csv.writer(open_csvfile)
        for line in xmldata:
            csvwriter.writerow(line)
        open_csvfile.close()
    with open(links_to_download_file, 'a') as dl_file:
        for txtfile in list_to_download:
            dl_file.write(txtfile + "\n")
        dl_file.close()
    # Enumerate pagecnt and change url
    url = next_url[0]

    

# Skapa en textfil som innehåller informationen från texterna csv-format

# Text file where we store already downloaded files
# one URL per line
downloaded_links_fn = "dl_links.txt"

# Ladda Quen med txtURL-s
txt_queue = Queue.Queue()

# Skapa en mapp där samtliga texter laddas ned från queuen





# Booleans to control running threads
link_finder_continue = True
downloader_continue  = True
running_threads      = []



# Blocking queue to store links we have found but not downloaded yet
motion_queue = Queue.Queue()

# URLs to sites to Crawl
google_play = "https://play.google.com/"
app_brain   = "http://www.appbrain.com/"

# Text file where we store already downloaded files
# one URL per line
downloaded_links_fn = "dl_links.txt"

# Create the downloaded links file  if it does not exist



# Gör om denna data till spase-format för att köra LDA i RStan och lda-paketet.




