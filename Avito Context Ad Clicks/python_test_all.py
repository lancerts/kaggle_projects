# -*- coding: utf-8 -*-
import sqlite3
import csv
import sys
reload(sys)
sys.setdefaultencoding('utf-8')

TITLE = [ 'TestID','SearchID', 'AdID', 'Position', 'ObjectType', 'HistCTR', 
         'SearchDate', 'IPID', 'UserID', 'IsUserLoggedOn', 'SearchQuery',
         'SearchLocationID', 'SearchCategoryID', 'SearchParams', 'UserAgentID',
         'UserAgentFamilyID', 'UserAgentOSID', 'UserDeviceID', 'Params',
         'Price', 'Title', 'IsContext', 'CLevel', 'ParentCategoryID',
         'SubcategoryID', 'LLevel', 'RegionID', 'CityID','ViewDate','PhoneRequestDate']

conn = sqlite3.connect("C:/Users/tshao/data_Avito Context Ad Clicks/database.sqlite")
SearchStream = conn.cursor()
SearchInfo = conn.cursor()
UserInfo = conn.cursor()
AdsInfo = conn.cursor()
Category = conn.cursor()
Location = conn.cursor()
VisitsStream=conn.cursor()
PhoneRequestsStream=conn.cursor()
SearchStream.execute("select * from testSearchStream where ObjectType=3")
#SearchInfo.execute("CREATE UNIQUE INDEX searchid1 on SearchInfo (SearchID)")
#UserInfo.execute("CREATE UNIQUE INDEX userid1 on UserInfo (UserID)")
#AdsInfo.execute("CREATE UNIQUE INDEX adid1 on AdsInfo (AdID)")
#Category.execute("CREATE UNIQUE INDEX searchcategoryid1 on Category (CategoryID)")
#Location.execute("CREATE UNIQUE INDEX searchlocationid1 on Location (LocationID)")
#VisitsStream.execute("CREATE  INDEX viewdate1 on VisitsStream (ViewDate)")
PhoneRequestsStream.execute("CREATE INDEX phonedate1 on PhoneRequestsStream (PhoneRequestDate)")

output_file = open("C:/Users/tshao/data_Avito Context Ad Clicks/test.csv", "w")
open_file_object = csv.writer(output_file)
open_file_object.writerow(TITLE)

search = SearchStream.fetchmany(10000)
cnt = len(search)
rows = []
while search:
    for i in search:
        search_id = i[1]
        ad_id = i[2]
        SearchInfo.execute("select * from SearchInfo where SearchID="+str(search_id))
        AdsInfo.execute("select * from AdsInfo where AdID="+str(ad_id))
        search_info = SearchInfo.fetchone()
        ads_info = AdsInfo.fetchone()
        if search_info is None:
            if ads_info is None:
                ads_info = [0 for k in range(7)]
            search_info = [0 for k in range(9)]
            user_info = [0 for k in range(5)]
            category = [0 for k in range(4)]
            location = [0 for k in range(4)]
            visits=[0 for k in range(4)]
            phone=[0 for k in range(4)]
            row = list(i) + list(search_info[1:]) + list(user_info[1:]) + list(ads_info[3:]) + list(category[1:]) + list(location[1:])+list(visits[3:])+list(phone[3:])
            rows.append(row)
            continue
        user_id = search_info[3]
        location_id = search_info[6]
        category_id = search_info[7]
#        try:
        UserInfo.execute("select * from UserInfo where UserID="+str(user_id))
        Category.execute("select * from Category where CategoryID="+str(category_id))
        Location.execute("select * from Location where LocationID="+str(location_id))
        VisitsStream.execute("select * from VisitsStream where UserID="+str(user_id))
        PhoneRequestsStream.execute("select * from PhoneRequestsStream where UserID="+str(user_id))
#        except:
#            UserInfo.execute("select * from UserInfo where UserID="+str(user_id))
#            Category.execute("select * from Category where CategoryID="+str(category_id))
#            Location.execute("select * from Location where LocationID="+str(location_id))
        user_info = UserInfo.fetchone()
        category = Category.fetchone()
        location = Location.fetchone()
        visits=VisitsStream.fetchone()
        phone=PhoneRequestsStream.fetchone()
        if ads_info is None:
            ads_info = [0 for k in range(7)]
        if user_info is None:
            user_info = [0 for k in range(5)]
        if category is None:
            category = [0 for k in range(4)]
        if location is None:
            location = [0 for k in range(4)]
        if phone is None:
            phone = [0 for k in range(4)]
        if location is None:
            location = [0 for k in range(4)]            
        row = list(i) + list(search_info[1:]) + list(user_info[1:]) +list(ads_info[3:]) + list(category[1:]) + list(location[1:])+list(visits[3:])+list(phone[3:])
        rows.append(row)
#        if row[5]=='':
#            row[5]=0.0
#        change 'SearchParams', 'Params' and 'Title' into "0" and "1" variable
        row[10]=int(row[10]!='')
        row[13]=int(row[13]!='')
        row[18]=int(row[18]!='')
        row[20]=int(row[20]!='')
        try:
            row[19]=float(row[19])
        except:
            print(row[19])  
#        print (row)        
    open_file_object.writerows(rows)
    rows = []
    if cnt % 100000==0:
#        break
        print(cnt)
    search = SearchStream.fetchmany(10000)
    cnt += len(search)

output_file.close()