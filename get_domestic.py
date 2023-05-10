import pandas as pd
# from bs4 import BeautifulSoup
from selenium.webdriver.common.by import By
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

from utils.imdb_api import check_file
from utils.script_lab import sl_load_session
from utils.driver import get_driver

driver = get_driver()

for i in range(2010, 2023):
    driver.get("https://www.the-numbers.com/box-office-records/domestic/all-movies/cumulative/released-in-" + str(i))

    table = driver.find_elements(By.TAG_NAME, "table")[1]
    table_html = table.get_attribute("outerHTML")
    df = pd.read_html(table_html)[0]

    tras = []
    tr_anchors = [tr.find_elements(By.TAG_NAME, "a") for tr in table.find_elements(By.TAG_NAME, "tr")]

    for tra in tr_anchors:
        if tra:
            tras.append(tra[0].get_attribute("href"))

    df['link'] = tras

    df.to_csv(f'top_domestic/domestic-{i}.csv')

driver.close()