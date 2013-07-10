# -*- coding: utf-8 -*-
"""
Created on Wed Jul 10 21:41:12 2013

@author: mansmagnusson
"""
#csvfile = 'Data/Motioner.csv'
#links_to_download_file = 'Data/links_to_download_file.txt'
#get_from_xml = ["id","dok_id","typ","datum","dokument_url_text"]
#url_text_xml = "dokument_url_text"

# Make class
#class riksdagenAPI
#    def __init__ # Skapar ett grundobjekt med information om vad som ska hämtas typ URL
#       #Kolla hur många sidor det rör dig om i XML:n och anpassa storleken efter detta
#
#    def download_id # Laddar ned information (ID) och sparar i klassen
#    
#    def download_metadata # Ange vilka noder i XML som ska laddas ned och skrivs till fil (csv)
#
#    def download_text # Laddar ned text


def download_riksdagen(starturl,
                       get_from_xml = ["id","dok_id","typ","datum","dokument_url_text"],
                       csvfile = 'Data/Motioner.csv',
                       links_to_download_file = 'Data/links_to_download_file.txt',
                       sidor_tot=None):
    from lxml import etree
    from urllib import urlopen
    import csv
    
    url_text_xml = "dokument_url_text"
    # Loop över sidor (som uppdateras)
    xmldoc = urlopen(starturl).read()
    xmltree = etree.fromstring(xmldoc)
    if sidor_tot == None:
        sidor_tot = int(xmltree.xpath('/dokumentlista/@sidor')[0])
    
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
                nodedata = doc[1].xpath(node)[0].text
                if nodedata != None:
                    nodelist.append(nodedata.encode('utf8'))
                else:
                    nodelist.append("Saknas")            
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
        # Change url
        if len(next_url) > 0:
            url = next_url[0]
