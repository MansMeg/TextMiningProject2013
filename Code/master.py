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
starturl = "http://data.riksdagen.se/dokumentlista/?rm=&typ=mot&d=&ts=&sn=&parti=&iid=&bet=&org=&kat=&sz=200&sort=c&utformat=xml&termlista=&p=1"
project_path = "/Users/mansmagnusson/Desktop/Text Mining/Project/TextMiningProject2013"
os.chdir(project_path)
csvfile = 'Data/Motioner.csv'
links_to_download_file = 'Data/links_to_download_file.txt'
get_from_xml = ["id","dok_id","typ","datum","dokument_url_text"]
url_text_xml = "dokument_url_text"

runfile(project_path+r'/Code/functions.py', wdir=project_path)

# Create files
if not os.path.exists(csvfile):
    newfile = open(csvfile, 'a')
    newfilewriter = csv.writer(newfile)
    newfilewriter.writerow(get_from_xml)
    newfile.close()
    
if not os.path.exists(links_to_download_file):
    to_dl_file = open(links_to_download_file, 'a').close()

download_riksdagen(starturl)
    

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




