library(ggplot2)
library(plotly)
library(dplyr)


cii = read.csv("E:/Term 2/Data Visualization/Data Set/Data Set/rajanand-crime-in-india/01_District_wise_crimes_committed_IPC_2001_2012.csv")

View(cii)
names(cii)
mpg
### scatter 
chart1 = cii %>% ggplot(aes(x=RAPE,y=KIDNAPPING...ABDUCTION,fill=STATE.UT)) + geom_jitter(aes(col=STATE.UT,frame = YEAR))

chart2 = ggplotly(chart1)

###########################################################################################################
### manufacture wise, class wise count of cars
data = mpg

head(mpg)
names(mpg)
##chart = data %>% ggplot(aes(x=manufacturer,y=class,fill = model)) + geom_count(aes(col=model,frame = model))


####################### for percentage
mpg1 = data %>% group_by(manufacturer) %>% summarise(total = n())
mpg2 = data %>% group_by(manufacturer,class) %>% 
  summarise(count = n(), 
            Per = count/mpg1$total[mpg1$manufacturer==manufacturer[1]]*100)

ggplot(data2 ,aes(x=manufacturer,y=Per,fill = class)) + 
  geom_bar(stat = 'identity',width = 0.6) +
  geom_text(aes(label=Per),position = position_stack(vjust = 0.5))



############################## for count 
data2 = data %>% group_by(manufacturer,class) %>% summarise(count = n())

chart3 = ggplot(data2 ,aes(x=manufacturer,y=count,fill = class)) + 
  geom_bar(stat = 'identity',width = 0.6) +
  geom_text(aes(label=count),position = position_stack(vjust = 0.5))


chart4 = ggplotly(chart3)
chart4

#############################################################################################
## Stack bar chart..
kidd = read.csv("E:/Term 2/Data Visualization/Data Set/Data Set/rajanand-crime-in-india/39_Specific_purpose_of_kidnapping_and_abduction.csv")
View(kidd)
names(kidd)


data3 = kidd %>% 
  select(ï..Area_Name,Year, Group_Name, Sub_Group_Name, K_A_Grand_Total) %>%
  group_by(ï..Area_Name,Sub_Group_Name) %>%
  summarise(Total = sum(as.numeric(K_A_Grand_Total)))

chart6 = ggplot(data3 ,aes(x=ï..Area_Name,Year, y=Total, fill = Sub_Group_Name)) + 
  geom_bar(stat = 'identity',position = "fill")



#######################################################################################
#### hitmap code

data3 = kidd %>% 
  select(ï..Area_Name,Year, Group_Name, Sub_Group_Name, K_A_Grand_Total) %>%
  group_by(ï..Area_Name,Sub_Group_Name) %>% 
  summarise(Total = sum(as.numeric(K_A_Grand_Total)))


chart1.1 = ggplot(data3,aes(y=ï..Area_Name,x=Sub_Group_Name)) +
  geom_tile(aes(fill = Total, col = "black")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90))



###################################################################



####################################################################################

dataset = tbl_df(mpg)
View(mpg)
head(dataset)
names(dataset)

## analysis the city mileage for each classs of car with different cylender



ggplot(dataset,aes(y=cty,x=class,fill = cyl)) + geom_bar(stat = 'identity',position = "fill")

ggplot(dataset,aes(y=cty,x=class,fill = cyl)) + boxplot()

chart1.2 = ggplot(datase,aes(x=class,y=cty))

############## boxplot #############

ggplot(dataset,aes(x=class, y=cty)) + geom_boxplot(aes(fill = factor(cyl)),varwidth = T)
 
############################################# 
 ######## histogram 
names(dataset)


ggplot(dataset,aes(x=displ)) + geom_histogram(aes(fill = class))
 
 
chart1.3 = ggplot(dataset,aes(displ)) + geom_histogram(aes(fill = class), bins = 3 ,col = "black")


##############################################################################################

data_eco = economics

View(data_eco)

chart1.4 = ggplot(data_eco,aes(data_eco$unemploy)) + geom_bar(stat = 'identity')


data_eco$Per_unemploy = round((data_eco$unemploy/data_eco$pop)*100,digits = 2)
chart4 = ggplot(data_eco,aes(x=date,y=unemploy,size = Per_unemploy)) + geom_line()  

##############################################################################################
library(lubridate)
brks = data_eco$date[seq(1,length(data_eco$date),12)]

lbls = lubridate::year(brks)

chart4 + scale_x_date(labels = lbls, breaks = brks) + theme(axis.text.x = element_text(angle = 90))

#######################################################################################
