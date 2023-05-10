from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
 
options = Options()
# options.add_argument('--headless=new')
# options.add_argument('--no-sandbox')
# options.add_argument('--disable-dev-shm-usage')
options.add_experimental_option('prefs', {
"download.default_directory": "/home/safal/Desktop/Projects/capstone/capstone_scripts/scripts/script_lab/", #Change default directory for downloads
"download.prompt_for_download": False, #To auto download the file
"download.directory_upgrade": True,
"plugins.always_open_pdf_externally": True #It will not show PDF directly in chrome
})
driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)
 
def get_driver():
    return driver