import pandas as pd
# from bs4 import BeautifulSoup
from selenium.webdriver.common.by import By
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

from utils.driver import get_driver

driver = get_driver()

for i in range(2014, 2015):
    df = pd.read_csv(f"top_domestic/domestic-{i}.csv")

    for index, row in list(df.iterrows())[:11]:
        if index <= 4: continue
        movie = row['Movie']
        link_w = row['link']
        link = link_w[:link_w.find('#')]

        intl = "#tab=international"
        people = "#tab=cast-and-crew"
        
        driver.get(link + intl)

        table = driver.find_elements(By.XPATH, "/html/body/div/div[3]/div[3]/div[5]/div[1]/center/table")
        if table: pd.read_html(table[0].get_attribute("outerHTML"))[0].to_csv(f'box_office_international/{i}_{index}_{movie}.csv')

        driver.get(link + people)

        tables = driver.find_element(By.ID, "cast-and-crew").find_elements(By.CLASS_NAME, "cast_new")
        df = pd.DataFrame()
        # print(tables)
        for tb in tables:
            title = tb.find_element(By.TAG_NAME, "h1")
            single_table = tb.find_elements(By.TAG_NAME, 'table')
            if single_table:
                table = pd.read_html(single_table[0].get_attribute("outerHTML"))[0]
                table['type'] = title.get_attribute("innerHTML")
                # print(title.get_attribute("innerHTML"))
                df = pd.concat([df, table])

        df.to_csv(f'cast_crew/{i}_{index}_{movie}.csv')

driver.close()