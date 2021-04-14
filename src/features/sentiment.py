# -*- coding: utf-8 -*-
#!/usr/bin/python

"""
Note that this script is ,made for python 2
"""

#import modules
import io, unicodedata
from afinn import Afinn
import os
import re

#add all text files from folder to a list of allFiles
allFiles = []
for root, dirs, files in os.walk('/home/lasse/Documents/InformationA'):
    for file in files:
        if file.endswith('.txt'):
            allFiles.append(file)

parti = "a"
avis = "information"
filename = "ia.csv"
header = "a_score,score,avis,parti,file\n"
with open(filename, "w") as f:
    f.write(header)
# apply Afinn da 32
afinn = Afinn(language='da')
# length normalized score
def normScore(text):
    score=afinn.score(text)/len(text.split())
    return score


def clean(txt):
    """
    cleans txt
    """
    txt = re.sub("[:\.\?()\&%,><@+\/\\\*]", "", txt)
    txt = re.sub("\[.*[\],\S*]", "", txt)
    txt = re.sub("-", "", txt)
    txt = re.sub("'", "", txt)
    txt=txt.lower()
    return txt


for file in allFiles:
    fileDirectory = '/home/lasse/Documents/InformationA/'+file
    f = io.open(fileDirectory,'r',encoding='utf8')
    text = f.read()
    text=clean(text) 
    meanscore = normScore(text)
    ascore = afinn.score(text)
    summary = "{},{},{},{},{}\n".format(meanscore, ascore, avis, parti, file)
    with open (filename, "a") as f:
        f.write(summary)

