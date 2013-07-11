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
class RiksdagenApi():
    """Class to get data from Riksdagens API"""
    def __init__(self,data_typ=None):
        from urllib import urlopen
        from lxml import etree
        
        typer = ["dokument","anförande","votering","person"]
        urls=["http://data.riksdagen.se/dokumentlista",
              "http://data.riksdagen.se/anforandelista/",
              "http://data.riksdagen.se/voteringlista/",
              "http://data.riksdagen.se/personlista"]
        searchsize="sz"
        
        if data_typ in typer:
            self.typ = data_typ
            self.baseurl = urls[typer.index(data_typ)]
        else:
            print "Korrekt värde för 'data_typ' saknas ('"+str(data_typ)+"' angivet)"+".\nMöjliga alternativ är:\n"+",\n".join(typer)
            return
        
        # Download the homepage and query names
        baseHTML = etree.HTML(urlopen(self.baseurl).read())
        self.qnames = baseHTML.xpath("//@name")
        self.qnames_values = [""] * len(self.qnames)
        self.qnames_description = []
        for desc in baseHTML.xpath("//td"):
            if desc.text!=None:
                self.qnames_description.append(desc.text)
        
        # Set inital values
        self.set_query("sz","1")
        self.set_query("utformat","xml")
        
        # Get xml nodes
        xmltree = etree.fromstring(urlopen(self.create_query_url()).read())
        if self.typ=="anförande":
            nodes = xmltree.xpath("//anforande/*") # Fixa detta mer allmännt
        else: 
            print "No nodes could be found!"
            return
        self.nodes = []
        for node in nodes:
            self.nodes.append(node.tag)         
        
    def set_query(self,qname,value):
        self.qnames_values[self.qnames.index(qname)] = str(value)
    
    def show_query(self):
        print "Query:"
        for i in enumerate(self.qnames):
            print self.qnames_description[i[0]] + " (" + self.qnames[i[0]] + ")\t"+ self.qnames_values[i[0]]

    def create_query_url(self):
        temp_list = []
        for i in range(len(self.qnames)):
            temp_list.append(self.qnames[i] + "=" + self.qnames_values[i])
        return self.baseurl+"?"+"&".join(temp_list)

### To do later on

http://data.riksdagen.se/anforandelista/?rm=&anftyp=&d=&ts=&parti=&iid=&sz=1&utformat=xml

test.utformat
# To do:
    def download_metadata(filename="temp.csv"):
        
self.create_query_url()        
        
test = RiksdagenApi("test")
test = RiksdagenApi("anförande")        
test.show_query()
test.set_query("sz",100)
test.show_query()
test

test = RiksdagenApi("test")

    def add(self, x):
        self.data.append(x)
    def addtwice(self, x):
        self.add(x)
        self.add(x)
    
#    def __init__ # Skapar ett grundobjekt med information om vad som ska hämtas: person, dokument eller anförande typ URL
#   Ange vilka variabler som är möjliga att ange samt vilka sökkriterier som finns.
# Ladda ner den första XML-en och spara de olika möjliga noderna för att skriva ut.
#
#    def ange_urval
#   #Kolla hur många sidor det rör dig om i XML:n och anpassa storleken efter detta, samt anpassa output
#
#    def download_id # Laddar ned information (ID) och sparar i klassen
#    
#    def download_metadata # Ange vilka noder i XML som ska laddas ned och skrivs till fil (csv)
#   
#
#    def download_text # Laddar ned text


def download_riksdagen(starturl,
                       get_from_xml = ["id","typ","datum","dokument_url_text"],
                       csvfile = 'Data/Motioner.csv',
                       links_to_download_file = 'Data/links_to_download_file.txt',
                       sidor_tot=None):
    from lxml import etree
    from urllib import urlopen
    import csv
    
    parser = etree.HTMLParser()
    
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
