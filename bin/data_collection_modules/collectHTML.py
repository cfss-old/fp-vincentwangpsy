# Import libraries
from urllib import request
import os
import re

from bs4 import BeautifulSoup

# Create directory if dataset directory does not already exist
cur_path = os.path.split(os.path.abspath(__file__))[0]
output_fldr = 'data/raw/'
output_dir = os.path.join(cur_path, output_fldr)
if not os.access(output_dir, os.F_OK):
    os.makedirs(output_dir)

# Get html file and save locally
def getHTML(url):
    searchStr = re.search('/[0-9]+-[0-9]+/$', url).group(0)[1:-1] # extract id from url for filenane
    response = request.urlopen(url)
    html = response.read().decode('utf-8')
    content = open(output_fldr + searchStr + '.html', 'w+')
    content.write(html)
    content.close()

# Get review links from category page
reviewLinks = []
def getURL(page):
    response = request.urlopen('http://www.gamespot.com/reviews/?page=' + str(page))
    html = response.read().decode('utf-8')
    html = BeautifulSoup(html, 'html5lib')

    chunks = html.select('#js-sort-filter-results .js-event-tracking')
    for chunk in chunks:
        result = chunk.get('href')
        reviewLinks.append(result)
    return reviewLinks

# Produce HTML files
# reviewLinks = getURL(98)
# for reviewLink in reviewLinks: getHTML('http://www.gamespot.com' + reviewLink)

# reviewLinks = getURL(137)
# for reviewLink in reviewLinks: getHTML('http://www.gamespot.com' + reviewLink)
# reviewLinks = getURL(138)
# for reviewLink in reviewLinks: getHTML('http://www.gamespot.com' + reviewLink)
