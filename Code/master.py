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
import lxml

# Ladda ner XML-filer från riksdagen och sparar som txt.

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




