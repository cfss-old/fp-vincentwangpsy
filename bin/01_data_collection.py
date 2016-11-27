# Import libraries
from bs4 import BeautifulSoup
import os
import pandas as pd
from data_collection_modules import collectHTML as collect
from data_collection_modules import extractHTML as extract


# Produce HTML files
if True:

    reviewLinks = collect.getURL(155)
    for reviewLink in reviewLinks: collect.getHTML('http://www.gamespot.com' + reviewLink)
    reviewLinks = collect.getURL(156)
    for reviewLink in reviewLinks: collect.getHTML('http://www.gamespot.com' + reviewLink)


# Make soup, extract, and save as df
if False:

    # Make soup of all files in a specified folder
    HTMLfldr = '../data/raw/'
    datafldr = '../data/'

    soupCauldron = []
    def makeSoup(fldr):
        for filename in os.listdir(fldr):
            content = open(fldr + filename, 'r')
            soupCauldron.append(BeautifulSoup(content.read(), 'html5lib'))
            content.close()
        return soupCauldron

    soupCauldron = makeSoup(HTMLfldr)

    # Extract data from each HTML file and combine
    df_cb_main      = []
    df_cb_platform  = []
    df_cb_developer = []
    df_cb_publisher = []
    df_cb_genre     = []
    for soup in soupCauldron:
        df_main, df_platform, df_developer, df_publisher, df_genre = extract.extractSoup(soup, 'GameSpot')
        df_cb_main.append(df_main)
        df_cb_platform.append(df_platform)
        df_cb_developer.append(df_developer)
        df_cb_publisher.append(df_publisher)
        df_cb_genre.append(df_genre)

    df_cb_main      = pd.concat(df_cb_main, ignore_index = True)
    df_cb_platform  = pd.concat(df_cb_platform, ignore_index = True)
    df_cb_developer = pd.concat(df_cb_developer, ignore_index = True)
    df_cb_publisher = pd.concat(df_cb_publisher, ignore_index = True)
    df_cb_genre     = pd.concat(df_cb_genre, ignore_index = True)

    # Save data for future analysis
    df_cb_main.to_csv(datafldr + 'df_cb_main.csv')
    df_cb_platform.to_csv(datafldr + 'df_cb_platform.csv')
    df_cb_developer.to_csv(datafldr + 'df_cb_developer.csv')
    df_cb_publisher.to_csv(datafldr + 'df_cb_publisher.csv')
    df_cb_genre.to_csv(datafldr + 'df_cb_genre.csv')
