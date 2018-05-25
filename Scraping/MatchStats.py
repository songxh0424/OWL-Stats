import pandas as pd
from selenium import webdriver
from bs4 import BeautifulSoup
import re
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


def scrapeMatchID():
    """
    get the matchID of all past matches
    """
    url = 'https://www.winstonslab.com/events/event.php?id=86#matches'
    browser = webdriver.Chrome()
    browser.get(url)
    page = BeautifulSoup(browser.page_source, 'html5lib')
    past = page.find('div', {'id': 'past'}) 
    matches = past.findAll('tr')
    matchIDs = [m.attrs.get('matchid', None) for m in matches[1:]]
    browser.close()

    return matchIDs


# def scrapeMapStats(matchID, mapNum):
#     """
#     get the detailed stats of a single map from a match
#     """
#     url = 'https://www.winstonslab.com/matches/match.php?id=' + matchID
#     browser = webdriver.Chrome()
#     browser.get(url)
#     ## need to press the +Stats buttons first, put on hold
#     button = browser.find_element_by_xpath("//button[@matchid='%s' and @gamenumber='%s']" % (matchID, mapNum))
#     button.click()
#     stats = browser.find_element_by_id('detailedStatsDiv_' + mapNum)
#     page = 
#     stats = BeautifulSoup(stats.text, 'html5lib')


def scrapeMatchStats(matchID):
    """
    get the detailed stats of a match
    """
    url = 'https://www.winstonslab.com/matches/match.php?id=' + matchID
    browser = webdriver.Chrome()
    browser.get(url)
    page = BeautifulSoup(browser.page_source, 'html5lib')
    ## date
    date = page.find('span', {'id': 'tzDate_1'})
    date = list(date.children)[0].title()
    ## box scores
    box = page.find('div', {'class': 'names-and-score'})
    team1, team2 = tuple([x.text for x in box.findAll('a')[:2]])
    score = box.find('div', {'class': 'scores spoiler'}).text
    score = re.sub(r'[ \t\n]', '', score)
    score1, score2 = tuple(score.split('-'))
    ## map scores
    maps = page.findAll('div', {'class': 'map-wrapper'})
    mapStats = {}
    for i, map in enumerate(maps):
        mapName = map.find('div', {'class': 'mapname'}).text.strip()
        ## convert all br tag to \n
        for br in map.findAll('br'):
            br.replace_with('\n')
        mapScore = [re.sub(r'[\t\n]+', ' ', x.text.strip()) for x in map.findAll('div')[3:7]]
        dic = {}
        for x in mapScore:
            tmp = x.split(' ')
            dic[tmp[0]] = tmp[1:]
        mapStats['map' + str(i + 1)] = dic
    ## players match stats
    players = page.findAll('div', {'class': 'col-md-6'})[:2]
    tables = [x.findAll('table') for x in players]
    df1 = pd.read_html(str(tables[0]))[0]
    df2 = pd.read_html(str(tables[1]))[0]
    df1 = df1.iloc[:, :7]
    df2 = df2.iloc[:, :7]
    playerStats = {'team1': df1, 'team2': df2}
    ## detailed stats
    button = browser.find_element_by_xpath("//button[@matchid='%s' and @gamenumber='0']" % matchID)
    button.click()
    wait = WebDriverWait(browser, 10)
    stats = wait.until(EC.presence_of_element_located((By.ID, 'dst_0')))
    detailedStats = pd.read_html(stats.get_attribute('outerHTML'))[0]
    browser.close()

    results = {'team1': team1, 'team2': team2, 'score': [score1, score2], 'date': date,
               'playerStats': playerStats, 'mapStats': mapStats, 'detailedStats': detailedStats}

    return results 
