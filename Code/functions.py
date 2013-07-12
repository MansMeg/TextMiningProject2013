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
    """Class to get data from Riksdagens API
    Methods:
    set_query : set different values for the query to Riksdagens API
    set_nodes_to_download : set which nodes in the XML to download as Metadata
    download_metadata : Download metadata in xml nodes to csv-file
    download_txt_files : Download text data as txt-filers in a given directory
    """
    def __init__(self,data_typ=None):
        from urllib import urlopen
        from lxml import etree
        
        typer = ["dokument","anförande","votering","person"]
        urls=["http://data.riksdagen.se/dokumentlista",
              "http://data.riksdagen.se/anforandelista/",
              "http://data.riksdagen.se/voteringlista/",
              "http://data.riksdagen.se/personlista"]
        searchsize = "sz"
        outformat = "utformat"
        self.class_status=False        
        
        if data_typ in typer:
            self.typ = data_typ
            self.baseurl = urls[typer.index(data_typ)]
        else:
            return "Korrekt värde för 'data_typ' saknas ('"+str(data_typ)+"' angivet)"+".\nMöjliga alternativ är:\n"+",\n".join(typer)
        
        # Download the homepage and query names
        baseHTML = etree.HTML(urlopen(self.baseurl).read())
        self.qnames = baseHTML.xpath("//@name")
        self.qnames_values = [""] * len(self.qnames)
        self.qnames_description = []
        for desc in baseHTML.xpath("//td"):
            if desc.text!=None:
                self.qnames_description.append(desc.text)
        
        # Set inital values
        self.set_query(searchsize,"1")
        self.set_query(outformat,"xml")
        
        # Get xml nodes
        all_nodes = self.get_xml()
        self.all_nodes = []
        for node in all_nodes:
            self.all_nodes.append(node.tag)
        self.nodes_to_download = ["dok_id","anforande_nummer"]
        
        # Information
        self.baseinfo = "Working RiksdagenApi class\nTyp: " + self.typ        
        
        # Define wether class is correct
        self.class_status=True
    
    # nodes_to_choose = "anforandetext" url = "http://data.riksdagen.se/anforande/H009131-7"
    def get_xml(self,nodes_to_choose="*",url=None):
        from urllib import urlopen
        from lxml import etree
        
        if type(nodes_to_choose) == type(str()):
            nodes_to_choose = [nodes_to_choose]
        if url==None:
            url = self.create_query_url()
        # Get the XML
        xmltree = etree.fromstring(urlopen(url).read())
        if self.typ=="anförande":
            base = "//anforande/"
            nodes = xmltree.xpath("|".join([base + s for s in nodes_to_choose]))
        else: 
            print "The nodes could not be found!"
            return
        return nodes
    
    def set_query(self,qname,value):
        """Method set_query(qname,value)
        str,str -> None
        Set query to identify object to download from Riksdagen
        qname: set query variable (see class.__repr__)
        value: set query variable value (see riksdagens webbplats)
        """
        self.qnames_values[self.qnames.index(qname)] = str(value)
        
    def set_nodes_to_download(self,nodes):
        """Method set_nodes_to_download(nodes)
        list of str -> None
        Choose which nodes to download with method download_metadata.
        nodes: which nodes that should be downloaded (see class for all possibilities)   
        """
        if nodes=="all":
            self.nodes_to_download = self.all_nodes
        else: 
            self.nodes_to_download = nodes
    
    def __repr__(self):
        if self.class_status==True:
            print self.baseinfo
            print "Query:\n"
            for i in enumerate(self.qnames):
                print self.qnames_description[i[0]] + " (" + self.qnames[i[0]] + ")\t"+ self.qnames_values[i[0]]
            print "\nSamtlig metadata: \n" + ", ".join(self.all_nodes)
            print "\nMetadata att ladda ner: \n" + ", ".join(self.nodes_to_download)
            return ""
        else:
            return "Not a working RiksdagenApi class"
    
    def create_query_url(self):
        temp_list = [s[0] + "=" + s[1] for s in zip(self.qnames,self.qnames_values)]
        return self.baseurl+"?"+"&".join(temp_list)
    
    def download_metadata(self,filename="/temp.csv"):
        """Method download_metadata(filename)
        str -> file
        filename: the name of the file (including '/' and '.csv') were to save metadata
        """
        from csv import writer
        from os import getcwd
        xml_nodes = self.get_xml(self.nodes_to_download)

        with open(getcwd()+filename, 'w') as open_csvfile:
            csvwriter = writer(open_csvfile)
            csvwriter.writerow(self.nodes_to_download)
            cnt = 0
            for i in range(len(xml_nodes)//len(self.nodes_to_download)):
                line = []
                for j in range(len(self.nodes_to_download)):
                    line.append(xml_nodes[cnt].text)
                    cnt += 1
                csvwriter.writerow(line)
            open_csvfile.close()
            
    def download_txt_files(self,directory="/Textfiler/",quiet=True):
        """Method download_txt_files(directory="/Textfiler/")
        str -> None
        Downloads all the text data as txt-files to 'directory' in working directory.
        The functions checks already existing files and do only download files not already in the directory.
        directory: were to store the txt files  
        """
        from os import getcwd,listdir
        if self.typ=="anförande":
            index_nodes = ["dok_id","anforande_nummer"]
        else: 
            return "Nothing downloaded!"
        print "Laddar ned index..."

        xml_index_nodes = self.get_xml(index_nodes)
        
        print "Totalt " + str(len(xml_index_nodes)//2) + " som skall laddas ned..."
        
        if self.typ=="anförande":
            ids_to_download = [xml_index_nodes[2*i].text+"-"+xml_index_nodes[2*i+1].text for i in range(len(xml_index_nodes)//2)]
            base_adress = "http://data.riksdagen.se/anforande/"
        else: 
            return "Nothing downloaded!"

        print "Laddar ned txt-filer till:\n"+getcwd()+directory
        workdir = getcwd()
        files_downloaded = set(listdir(getcwd()+directory))
        if self.typ=="anförande":
            for id_to_download in ids_to_download:
                if id_to_download+".txt" not in files_downloaded:                
                    text = self.get_xml("anforandetext",base_adress+id_to_download)[0].text
                    with open(workdir+directory+id_to_download+".txt","w") as txtfile:
                        txtfile.write(text)
                        txtfile.close()
                    if not quiet:
                        print id_to_download


# Download txt-files from metadata method

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
