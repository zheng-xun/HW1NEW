#HW hand in
#call data
testtusers <- read.table('test_users.csv',sep=',', stringsAsFactors=T, header=T, nrows=500)
trainuser2 <- read.table('train_users_2.csv',sep=',', stringsAsFactors=T, header=T, nrows=500)
sessions <- read.table('sessions.csv',sep=',', stringsAsFactors=T, header=T, nrows=500)
install.packages("ggplot2")
install.packages("sqldf")
install.packages("reshape")
install.packages("randomForest")
library(ggplot2)
library(sqldf)
library(reshape2)
library(randomForest)

#將sessions內的Action_type檔案做合併
sessions$action_type[sessions$action_type==''] <- 'unknown'
sessionsActionType <- sqldf ("select user_id, 
                            case 
                            when action_type=='booking_request' then 'booking_request'
                            when action_type=='booking_response' then 'booking_response')
                            when action_type=='click' then 'click'
                            when action_type=='data' then 'data'
                            when action_type=='message_post' then 'message_post'
                            when action_type=='modify' then 'modify'
                            when action_type=='partner_callback' then 'partner_callback'
                            when action_type=='submit' then 'submit'
                            when action_type=='view' then 'view'
                            else 'other_action_type' end as action_type,
                            sum(secs_elapsed) as secs_elapsed
                            from sessions
                            group by user_id, action_type")

#將sessions內的device_type檔案做合併
sessionsDeviceType <- sqldf ("select user_id,
                            case when device_type=='Android App Unknown Phone/Tablet' then 'android_app_unknown_phone_tablet'
                            when device_type=='Android Phone' then 'android_phone'
                            when device_type=='Blackberry' then 'blackberry'
                            when device_type=='Chromebook' then 'chromebook'
                            when device_type=='iPad Tablet' then 'iPad_tablet'
                            when device_type=='iPodtouch' then 'iPodtouch'
                            when device_type=='Linux Desktop' then 'linux_desktop'
                            when device_type=='Mac Desktop' then 'mac_desktop'
                            when device_type=='Opera Phone' then 'opera_phone'
                            when device_type=='Tablet' then 'tablet'
                            when device_type=='Windows Desktop' then 'windows_desktop'
                            when device_type=='Windows Phone' then 'windows_phone'
                            else 'other_device_types' end as device_type,
                            sum(secs_elapsed) as secs_elapsed
                            from sessions
                            group by user_id, device_type")

# use reshape2 package to 'dcast' the above 2 tables separately
sessionsActionTypeNew <- subset(sessionsActionType, user_id!='')
sessionsDeviceTypeNew <- subset(sessionsDeviceType, user_id!='')
row.names(sessionsActionTypeNew) <- NULL
row.names(sessionsDeviceTypeNew) <- NULL
userActionType <- dcast(sessionsActionTypeNew, user_id~action_type, sum)
userDeviceType <- dcast(sessionsDeviceTypeNew, user_id~device_type, sum)
# merge userActionType & userDeviceType
userActionDeviceSecsElapsed <- merge(userActionType, userDeviceType, by="user_id", all.x=T, all.y=T)
# replace NA with 0
userActionDeviceSecsElapsed[is.na(userActionDeviceSecsElapsed)] <- 0

#將testusers內的first_device_type做合併
testusersdevice <- sqldf("select id,
                         case when first_device_type=='Android Phone' then 'Android Phone'
                         when first_device_type=='Android tablet' then 'Android tablet'
                         when first_device_type=='Desktop (Other)' then 'Desktop (Other)'
                         when first_device_type=='iPad' then 'iPad'
                         when first_device_type=='iPhone' then 'iPhone'
                         when first_device_type=='Mac Desktop' then 'Mac Desktop'
                         when first_device_type=='Other/Unknown' then 'Other/Unknown'
                         when first_device_type=='Windows Desktop' then 'Windows Desktop'
                         else 'other_first_device_type'
                         end as testusersdevice,
                         sum(signup_flow) as signup_flow
                         from testusers
                         group by id, testusersdevice")

# use reshape2 package to 'dcast' the above 2 tables separately
testusersFirstNew <- subset(testusersdevice, id!='')
row.names(testusersFirstNew) <- NULL
usertestusersFirst <- dcast(testusersFirstNew, id~signup_flow, sum)
                         
