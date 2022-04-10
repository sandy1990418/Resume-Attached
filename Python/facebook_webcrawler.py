# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# %%
# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# %%
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait as wait
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.chrome.options import Options

import time 
from bs4 import BeautifulSoup
from pprint import pprint
import requests
import csv
from pandas import Series,DataFrame
import unicodecsv
import pandas as pd
import numpy as np
import re

import os 
os.chdir("D:\\桌面\\KKDay\\爬蟲\\資料儲存")
# save env
import pickle  
# get geocoding 
from geopy.geocoders import GoogleV3
import geopy
from datetime import datetime
from datetime import date
from datetime import timedelta
import json


from selenium.webdriver.common.action_chains import ActionChains #move_to_element
from selenium.common.exceptions import StaleElementReferenceException #error > element is not attached to the page document
from selenium.common.exceptions import NoSuchElementException #點完所有按鈕時


from termcolor import colored
import time
from datetime import datetime
from datetime import date
from datetime import timedelta
from IPython.display import display
import pandas as pd


# %%
def scrolling_func(innerHeightOfWindow,totalOffset):
    # 每捲一次，休息幾秒
    sleepingSecond = 1
    # 在捲動到沒有元素動態產生前，持續捲動
    temp = totalOffset
    while True:
        # 每次移動高度
        totalOffset += 500;
        
        # 捲動的 js code
        js_scroll = "(function (){{window.scrollTo({{top:{}, behavior: 'smooth' }});}})();".format(totalOffset)
        # 執行 js code
        driver.execute_script(js_scroll)
        # 強制等待
        time.sleep(sleepingSecond)
        # 透過執行 js 語法來取得捲動後的高度
        innerHeightOfWindow = driver.execute_script('return window.document.documentElement.scrollHeight;');
        # 強制等待
        time.sleep(sleepingSecond)
    
        # 印出捲動距離
        print("innerHeightOfWindow: {}, totalOffset: {}".format(innerHeightOfWindow, totalOffset))

        dis = totalOffset - temp
        # 為了實驗功能，捲動超過一定的距離，就結束程式
        if dis > 1500:
            break
    return totalOffset#,innerHeightOfWindow


# %%
def date_manipulate(x):
    x =str(x)
    try:
        if x.partition("年")[0] == '2020': 
           x = x.replace('年', '-').replace('月', '-').replace('日', '')
           x = (datetime.strptime(x, '%Y-%m-%d')).date()
        elif x.find('小時')!= -1 :
           x = x.partition("小時")[0]
           y =  int(x)
           x = (datetime.now()-timedelta(hours=y)).date()
        elif x.find('下午')!= -1 :
           x = x.partition("下午")[0]
           x= "2021年"+x
           x = x.replace('年', '-').replace('月', '-').replace('日', '')
           x = (datetime.now()-timedelta(hours=y)).date()
        else :
           x= "2021年"+x
           x = x.replace('年', '-').replace('月', '-').replace('日', '')
           x = (datetime.strptime(x, '%Y-%m-%d')).date()
    except:
        pass
    return x

def  click_funciton():
    while(True):
        try:
            btn_more = driver.find_element_by_css_selector('#jsc_c_6c > div > div > span > div.o9v6fnle.cxmmr5t8.oygrvhab.hcukyx3x.c1et5uql.ii04i59q > div > div')
            ActionChains(driver).move_to_element(btn_more).perform()
            btn_more.click()
            time.sleep(0.1)
            print(btn_more)
        except StaleElementReferenceException: #element is not attached to the page document
            #define the web element once again
            btn_more = driver.find_element_by_css_selector('#jsc_c_2j > div > div > span > div > div:nth-child(4)')
            print(btn_more)
            ActionChains(driver).move_to_element(btn_more).perform()
            btn_more.click()
            time.sleep(0.1)
        except NoSuchElementException: #點完所有更多留言按鈕了
            break


# %%
# 啟動瀏覽器工具的選項
options = webdriver.ChromeOptions()
options.add_argument("--headless")              #不開啟實體瀏覽器背景執行
options.add_argument("--start-maximized")         #最大化視窗
options.add_argument("--incognito")               #開啟無痕模式
options.add_argument("--disable-popup-blocking ") #禁用彈出攔截


#s=Service(ChromeDriverManager().install())
driver = webdriver.Chrome(ChromeDriverManager().install(),options = options)
driver.maximize_window()

# Step 2) Navigate to Facebook
driver.get("https://www.facebook.com")
driver.implicitly_wait(15)

# Step 3) Search & Enter the Email or Phone field & Enter Password
username = driver.find_element_by_css_selector("#email")
password = driver.find_element_by_css_selector("#pass")
username.send_keys("sandy87418@gmail.com")
password.send_keys("love870418")
# Step 4) Click Login
driver.implicitly_wait(15)
password.submit()
time.sleep(3)

driver.get("https://www.facebook.com/VisitSingaporeOfficial")
soup =BeautifulSoup(driver.page_source,'lxml')
print(colored("\n[SUCCESS]:成功打開. \n", "green"))



column_names = ['post_url','post_id','post_date']
Singapore_facebook = pd.DataFrame(columns = column_names)
# 瀏覽器內部的高度
innerHeightOfWindow = 0
# 當前捲動的量(高度)
totalOffset = 0
index = 0
totalOffset2 = 0 


for use in range(1,61):

    ##顯示現在跑到哪
    print(use)

    ##得到facebook post id
    button=driver.find_elements_by_css_selector('span:nth-child(2) > span > a')
    time.sleep(0.5)

    ##滑動資料
    temp_take_long= scrolling_func(innerHeightOfWindow,totalOffset) 
    totalOffset+=2000
    innerHeightOfWindow+=(driver.execute_script('return window.document.documentElement.scrollHeight;')-innerHeightOfWindow)
    print(totalOffset,innerHeightOfWindow)

    for post_first_id in button:
        
        display(Singapore_facebook)
        temp_button=post_first_id
        print(temp_button)
        #Singapore_facebook.loc[index,['button']] = temp_button
        time.sleep(0.5)
        #driver.execute_script("arguments[0].scrollIntoView();",temp_button)
        driver.implicitly_wait(10)
        try:
            ##資料的指標
            index+=1
            ActionChains(driver).move_to_element(temp_button).perform()
            print(temp_button.get_attribute("href"))
            ##找到post id 的url後輸出
            post_url= temp_button.get_attribute("href")
            post_id = (temp_button.get_attribute("href")).partition("posts/")[2].partition("?__cft__[0]")[0]
            
            print(colored("\n[SUCCESS]:success button.click. \n", "green"))
            time.sleep(0.5)
            '''
            Singapore_facebook.loc[index,['post_content']] = post_content        
            '''
            temp_date = date_manipulate(temp_button.text)
            #Singapore_facebook.loc[index,['post_date']] = temp_date
            print(temp_button.text)

            temp_data = pd.DataFrame([[post_url,post_id,temp_date]], columns=['post_url','post_id','post_date'])
            Singapore_facebook = pd.concat([temp_data ,Singapore_facebook])
            print(colored("\n[SUCCESS]:success show post date. \n", "green"))
            time.sleep(0.5)
            '''
            driver.get(temp_button.get_attribute("href"))
            ##找到寫文字的地方
            post_content = 'Post_content'
            for id_line in range(1,8):
                print('第幾行',id_line)
                try:
                    line = 'div > div > span > div:nth-child('+str(id_line)+')'
                    post_content_line = driver.find_elements_by_css_selector(line)
                    time.sleep(0.5)
                    ##把有寫文字的行全部列出來
                    for  post1  in post_content_line:
                        temp = post1.text
                        post_content=post_content+' '+temp
                except:
                    pass
            print(post_content)
            '''
            if (datetime.strptime(str(temp_date), '%Y-%m-%d')).date()<(datetime.strptime('2021-5-1', '%Y-%m-%d')).date():
                break
        except:
            pass
        time.sleep(0.5)
        driver.implicitly_wait(5)
        if  any([use == 20 , use ==40, use ==50]) :
            name = 'Singapore_facebook_'+str(use)+'.pickle'
            with open(name, 'wb') as handle:
                pickle.dump(Singapore_facebook, handle, protocol=pickle.HIGHEST_PROTOCOL)
            time.sleep(5)
            continue
        else :
            continue
with open('phl_promotion_facebook.pickle', 'wb') as handle:
    pickle.dump(Singapore_facebook, handle, protocol=pickle.HIGHEST_PROTOCOL)
print(colored("\n[SUCCESS]:all done. \n", "red"))


## open data 
with open('phl_promotion_facebook.pickle', 'rb') as handle:
    Singapore_facebook = pickle.load(handle)


Singapore_facebook = Singapore_facebook.drop_duplicates('post_url')
Singapore_facebook = Singapore_facebook.reset_index(drop=True)


for i in range(Singapore_facebook.shape[0]):
    try:
        driver.get(Singapore_facebook['post_url'].iloc[i])
        print('資料現在跑到第幾列',i)
        text_name = 'div > div:nth-child(1) > div > div.rq0escxv.l9j0dhe7.du4w35lb > div > div > div.j83agx80.cbu4d94t.d6urw2fd.dp1hu0rb.l9j0dhe7.du4w35lb > div.l9j0dhe7.dp1hu0rb.cbu4d94t.j83agx80 > div.bp9cbjyn.j83agx80.cbu4d94t.d2edcug0 > div.d2edcug0.oh7imozk.tr9rh885.abvwweq7.ejjq64ki > div > div > div > div > div > div > div > div > div > div.rq0escxv.l9j0dhe7.du4w35lb.hybvsw6c.io0zqebd.m5lcvass.fbipl8qg.nwvqtn77.k4urcfbm.ni8dbmo4.stjgntxs.sbcfpzgs > div > div:nth-child(2) > div > div:nth-child(3) > div:nth-child(1)'
        post_content_all = driver.find_element_by_css_selector(text_name).text
        print(post_content_all)
        Singapore_facebook.loc[i,['post_content']] =post_content_all
    except:
        pass
    continue
driver.close()
driver.quit()


with open('phl_promotion_facebook_all_done_20210721.pickle', 'wb') as handle:
    pickle.dump(Singapore_facebook, handle, protocol=pickle.HIGHEST_PROTOCOL)
print(colored("\n[SUCCESS]:all done. \n", "red"))


