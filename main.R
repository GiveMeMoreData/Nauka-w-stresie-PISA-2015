library("tidyverse")
library("haven")
library("dplyr")
library("ggplot2")
library(countrycode)

### PUT THIS FILE IN THE SAME DIRECTORY AS YOUR SAS FILES THAT YOU WANT TO USE
options(stringsAsFactors = FALSE)

### LOADING DATA ###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))      #sets working directory to file location


### you should have files below in the same directory as this file
data <- read_sas("res/cy6_ms_cmb_tch_qqq.sas7bdat")
students <- read_sas("res/cy6_ms_cmb_stu_qqq.sas7bdat")         # 500000x921 dataframe WATCH OUT!
cognitive <- read_sas("res/cognitive/cy6_ms_cmb_stu_cog.sas7bdat") # even bigger bad boy
cognitive_europe <- read.csv2("res/cognitive_europe.csv") # 
school <- read_sas("res/school/school.sas7bdat")
 




### CHOSEN QUESTIONS ###

# STUDENT QUESTIONS #

# I only store question number becouse they sometimes
# have subquestions and all are of type: 
# 2  uppper case letters: type of questionnaire etc ST (student questionnaire), SC (school questionnaire), PA (parent questionnaire), 
# 3 digits: question number ect 011 or 127
# 5 diggits or upper case letters: random code etc 00ATY or A114I
# so title of question looks like this: ST034AT49Q


## Unfortunatly after getting data from multiple questionnaires you have to join dataframes yourself
questions_ST <- c("ST",11,13,111,126,127,16,118,119,82,34,39,38,62,71,63,97,98,104,92,93,94,95,113,129,146,76,78) # CHOOSE YOUR OWN QUESTIONS
questions_SC <- c("SC") 



### EXTRACTING CHOSEN QUESTIONS ###

get_data_of_questions <- function(questions,database=students){
  
  # all titles of questions 
  all_questions_names <- colnames(database)
  
  # basic info about student, Country 3-letter code, School ID, Student ID, Language of questionnaire 3-digits
  basic_info <- database[,c(2,3,4,29)]
  
  # all questions is dataframe that will be returned 
  all_questions <- basic_info
  
  for(question in questions[-1]){
    
    # setting regexp to find all sub questions to one main question
    regEXP <- paste0(questions[1],"0*",toString(question),".{5}\\b")
    
    # extracting all subquestions to a question
    found_questions <- str_extract(all_questions_names,regEXP)
    
    # adding found subquestions to dataframe
    all_questions <-cbind(all_questions,database[found_questions[!is.na(found_questions)]])
    
  }
  return(all_questions)
}

#####
##### TWORZENIE COGNITIVE EUROPE
#####

cognitive_europe <- left_join(names,cognitive, by=c("genc3c"="CNT"))%>%
  select(-genc3c)

write.csv2(cognitive_europe,"res/cognitive_europe.csv",row.names = FALSE)


#####

data_of_all_questions <-  get_data_of_questions(questions_ST)

write_sas(data_of_all_questions,"Students\ questionnaire.sas")
help("write_sas")
wczytane_dane<- read.csv("Students\ questionnaire.csv")


###
### Różnica średniego czasu nauki dla kobiet i mężczyzn 
###


questions <- c("ST",71)


names <- codelist[ codelist$genc3c %in% unlist(unique(data["CNT"])),c("cow.name","genc3c")]

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")

## Kraje europejskie
names <- names[names$cow.name %in% europeanUnion,]



data <- get_data_of_questions(questions)

# Zmiana nazw krajów w data na pełne 
data <- left_join(names,data,by=c("genc3c"="CNT"))%>%
    select(-genc3c)

# Usunięcie pustych danych wierszy 

data <- data%>%
    filter(!is.na(ST071Q01NA) & !is.na(ST071Q02NA) & !is.na(ST071Q03NA) & !is.na(ST071Q04NA) & !is.na(ST071Q05NA) &
               ST071Q01NA<=30 & ST071Q02NA<=30 & ST071Q03NA<=30 & ST071Q04NA<=30 & ST071Q05NA<=30 & ST071Q01NA+ST071Q02NA+ST071Q03NA+ST071Q04NA+ST071Q05NA<150)%>%
    group_by(cow.name,ST004D01T)%>%
    mutate(all=n())




data <- data%>%
    filter(ST071Q01NA>=14 | ST071Q02NA>=14 | ST071Q03NA>=14 | ST071Q04NA>=14 | ST071Q05NA>=14)%>%
    group_by(Country=cow.name,Sex=ST004D01T)%>%
    summarise(count=n(),sum=sum(ST071Q01NA,ST071Q02NA,ST071Q03NA,ST071Q04NA,ST071Q05NA),all=max(all))%>%
    mutate(Value=sum/count)
    

# gives mixed resoults
write.csv(data,"res/sum_learning.csv",row.names = FALSE)





###
### only math, science and reading 
###
data <- get_data_of_questions(questions)

# Zmiana nazw krajów w data na pełne 
data <- left_join(names,data,by=c("genc3c"="CNT"))%>%
    select(-genc3c)

# Usunięcie pustych danych wierszy 




data <- data%>%
    group_by(CNT,ST004D01T)%>%
    mutate(all=n())


data <- data%>%
    filter(ST071Q01NA>=14 )%>%
    group_by(Country=CNT,Sex=ST004D01T)%>%
    summarise(count=n(),sum=sum(ST071Q01NA),all=max(all))%>%
    mutate(Value=count/all)

rysuj_mape_final(data,"Kto cześciej uczy się matmy powyżej 2 godzin?")


### 
### BAD TEACHERS
###

questions <- c("ST",39)



bad_teachers <- get_data_of_questions(questions)
bad_teachers <- bad_teachers[!is.na(bad_teachers$ST039Q01NA),]
bad_teachers_sum <- bad_teachers%>%
  select(CNT,ST004D01T)%>%
  mutate(diff=unlist(bad_teachers[2]^3+bad_teachers[3]^3+bad_teachers[4]^3+
                 bad_teachers[5]^3+(bad_teachers[6]*2)^3+(bad_teachers[7])^3))


bad_nr <- bad_teachers_sum%>%
  group_by(CNT,ST004D01T)%>%
  tally()


bad_val <- bad_teachers_sum%>%
  group_by(CNT,ST004D01T)%>%
  summarise(val=sum(diff,na.rm = TRUE))

bad <- left_join(bad_val,bad_nr, by=c("CNT","ST004D01T"))

bad2 <- bad%>%
  mutate(value=unlist(val/n))%>%
  select(CNT,ST004D01T,value)

write.csv(bad2,"bad_teachers.csv",row.names = FALSE)

###
### BAD TEACHERS 2 
###
  

questions <- c("ST",39)



bad_teachers <- get_data_of_questions(questions)
bad_teachers <- bad_teachers[!is.na(bad_teachers$ST039Q01NA),]
bad_teachers_sum <- bad_teachers%>%
  select(CNT,ST004D01T)%>%
  mutate(diff=unlist(bad_teachers[2]^3+bad_teachers[3]^3+bad_teachers[4]^3+
                       bad_teachers[5]^3+(bad_teachers[6]*2)^3+(bad_teachers[7])^3))


bad <- bad_teachers_sum%>%
  group_by(CNT,ST004D01T)%>%
  summarise(val=sum(diff,na.rm = TRUE),n=n())


bad2 <- bad%>%
  mutate(value=unlist(val/n))%>%
  select(CNT,ST004D01T,value)

girls <- bad2%>%
  filter(ST004D01T==1)%>%
  select(CNT,value)

boys <- bad2%>%
  filter(ST004D01T==2)%>%
  select(CNT,value)

write.csv(girls,"bad_tch_girls.csv",row.names = FALSE)
write.csv(boys,"bad_tch_boys.csv",row.names = FALSE)

is.na(bad_teachers_sum)

###
### SCHOOLS WITH HIGH ONE GENDER RATIO (only for boys/ ony for girls)
###

questions <- c("SC","1","2","3","53","59","13","14","17","61","64")
school_data <- get_data_of_questions(questions,school)

one_gender_sch <- school_data%>%
  mutate(one_gender_index=(school_data$SC002Q01TA-school_data$SC002Q02TA)/
           (school_data$SC002Q01TA+school_data$SC002Q02TA))%>%
  filter(SC002Q01TA+SC002Q02TA>50)%>%
  arrange(desc(one_gender_index))

cnt_one_gender_sch <- one_gender_sch%>%
  select(CNT,one_gender_index)%>%
  group_by(CNT)%>%
  summarise(sum=sum(abs(one_gender_index)),odchylenie=sum(one_gender_index),n=n())%>%
  mutate(sum=sum/n*100,odchylenie=odchylenie/n*100)%>%
  select(-n)

colnames(cnt_one_gender_sch) <- c("CNT","value","diff")
write.csv(cnt_one_gender_sch,"res/cnt_one_gender_sch.csv",row.names = FALSE)




cnt_class_size <- one_gender_sch[!is.na(one_gender_sch$SC003Q01TA),]%>%
  group_by(CNT)%>%
  summarise(value=sum(SC003Q01TA,na.omit=TRUE)/n())


write.csv(cnt_class_size,"res/cnt_class_size.csv",row.names = FALSE)
  
###
### stres z podziałem na płcie i kraje
###

questions <- c("ST",4,71,118,119)

data<- get_data_of_questions(questions)


names <- codelist[ codelist$genc3c %in% unlist(unique(data["CNT"])),c("cow.name","genc3c")]

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")

## Kraje europejskie
names <- names[names$cow.name %in% europeanUnion,]


stressed_students <- left_join(names,data, by=c("genc3c"="CNT"))%>%
  select(-cow.name)%>%
  group_by(genc3c,ST004D01T)%>%
  mutate(all=n())%>%
  filter(ST118Q01NA>2 | ST118Q02NA>2 |ST118Q03NA>2 | ST118Q04NA>2)%>%
  summarise(diff=n(),all=first(all))

stressed_girls_cnt <-stressed_students%>%
  filter(ST004D01T==1)%>%
  mutate(value=diff/all*100)%>%
  select(CNT=genc3c,value)

write.csv(stressed_girls_cnt,"res/stressed_girls_cnt.csv",row.names = FALSE)


stressed_stu_cnt <-stressed_students%>%
  mutate(value=diff/all*100)%>%
  select(Country=genc3c,Sex=ST004D01T,Value=value)

write.csv(stressed_stu_cnt,"res/stressed_stu_cnt.csv",row.names = FALSE)

###
### poziom zestresowania, a słabi nauczyciele
###


questions <- c("ST",39)



bad_teachers <- get_data_of_questions(questions)
bad_teachers <- bad_teachers[!is.na(bad_teachers$ST039Q01NA),]%>%
    select(-CNTSCHID)
bad_teachers_sum <- bad_teachers%>%
    select(Country=CNT,Id=CNTSTUID,Sex=ST004D01T)%>%
    mutate(bad_teacher_index=unlist(bad_teachers[3]+bad_teachers[4]+bad_teachers[5]+
                           bad_teachers[6]+(bad_teachers[7])+(bad_teachers[8])))


names <- codelist[ codelist$genc3c %in% unlist(unique(data["CNT"])),c("cow.name","genc3c")]

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")

## Kraje europejskie
names <- names[names$cow.name %in% europeanUnion,]


# Zmiana nazw krajów w data na pełne 


questions <- c("ST",71)
data <- get_data_of_questions(questions)%>%
    right_join(names,by=c("CNT"="genc3c"))%>%
    select(-CNT,Country=cow.name)

### To w ogóle nie to co chce, ale ciekawe warto wspomnieć 
data <- data%>%
    filter(!is.na(ST071Q01NA) & !is.na(ST071Q02NA) & !is.na(ST071Q03NA) & 
               ST071Q01NA<=30 & ST071Q02NA<=30 & ST071Q03NA<=30 & ST071Q01NA+ST071Q02NA+ST071Q03NA<90)%>%
    mutate(learn_time=ST071Q01NA+ST071Q02NA+ST071Q03NA)%>%
    select(Id=CNTSTUID,learn_time)%>%
    left_join(bad_teachers_sum,by=c("Id"="Id"))%>%
    left_join(names,by=c("Country"="genc3c"))%>%
    select(-Country,-Id,Stress=diff)%>%
    filter(!is.na(Stress)&!is.na(Sex)&!is.na(learn_time))

## Pytania o stres
questions <- c("ST",4,71,118,119)

data<- get_data_of_questions(questions)

stres <- left_join(names,data, by=c("genc3c"="CNT"))%>%
    mutate(stress_lvl=ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA)%>%
    select(Id=CNTSTUID,stress_lvl)%>%
    inner_join(bad_teachers_sum,by=c("Id"="Id"))%>%
    filter(!is.na(bad_teacher_index))


g1<- ggplot(stres)+
    geom_smooth(data=stres[stres$Sex==1,],aes(bad_teacher_index,stress_lvl),color="#ca0020",se=FALSE)+
    geom_smooth(data=stres[stres$Sex==2,],aes(bad_teacher_index,stress_lvl),color="#0571b0",se=FALSE)+
    theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        aspect.ratio = 0.7
    )

gg1 <- ggplot_build(g1)

df2 <- data.frame(
    x=gg1$data[[1]]$x,
    ymin = gg1$data[[1]]$y,
    ymax = gg1$data[[2]]$y
)
g2 <- g1 +
    geom_ribbon(data = df2, aes(x = x, ymin = ymin, ymax = ymax),
                fill = "grey", alpha = 0.4)+
    coord_cartesian(x=c(5,20),y=c(7.5,15.5),expand=FALSE)

p <- ggplot()+
    geom_smooth(aes(x=data[data$Sex==1,"Stress"],y=data[data$Sex==1,c("learn_time")]),color="#ca0020")+
    geom_smooth(aes(x=data[data$Sex==2,"Stress"],y=data[data$Sex==2,c("learn_time")]),color="#0571b0")+
    theme(
        panel.background = element_blank(),
        panel.grid.major.y = element_line(color = "grey"),
        aspect.ratio = 0.5
    )


ggplot(data,aes(stres,learn_time))+
    geom_smooth(se=FALSE)+
    coord_cartesian(xlim=c(6,22),ylim=c(0,20),expand=FALSE)

svglite("img/stress_badTeachers.svg", width = 10, height = 10)
g2
dev.off()

###
###  Czas nauki, a stres 
###

questions <- c("ST",4,71,118)

data<- get_data_of_questions(questions)


names <- codelist[ codelist$genc3c %in% unlist(unique(data["CNT"])),c("cow.name","genc3c")]

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","United Kingdom")

## Kraje europejskie
names <- names[names$cow.name %in% europeanUnion,]

# Dla krajóW
stressed_students <- left_join(names,data, by=c("genc3c"="CNT"))%>%
  select(-cow.name,Country=genc3c)%>%
  filter(ST071Q01NA<31,ST071Q02NA<31,ST071Q03NA<31,ST071Q04NA<31,ST071Q05NA<31 )%>%
  group_by(Country)%>%
  summarise(stress_lvl=sum(ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA,na.rm = TRUE)/n(),ave_learning_time=mean(c(ST071Q01NA,ST071Q02NA,ST071Q03NA),na.rm = TRUE))%>% # tylko czytanie
  select(Country,stress_lvl,ave_learning_time)

write.csv(stressed_students,"res/stres_aveLearningTime.csv",row.names = FALSE)

# Dla konkretnych studentów
stressed_student <- left_join(names,data, by=c("genc3c"="CNT"))%>%
    select(-cow.name,Country=genc3c)%>%
    filter(ST071Q01NA<31,ST071Q02NA<31,ST071Q03NA<31,ST071Q04NA<31,ST071Q05NA<31 )%>%
    mutate(stress_lvl=ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA,ave_learning_time=(ST071Q01NA+ST071Q02NA+ST071Q03NA)/3)%>% # tylko czytanie
    select(Country,Sex=ST004D01T, stress_lvl,ave_learning_time)

ggplot()+
    geom_smooth(data=stressed_student,aes(x=stress_lvl["Sex"==1,],y=ave_learning_time["Sex"==1,]))


plot <- ggplot(stressed_students, aes(x=stress_lvl,y=ave_learning_time))+
  labs(title = "Wykres zależności edniego czasu nauki od ogólnego poczucia stresu",subtitle = "DZIEWCZYNY")+
  geom_point()+
  geom_smooth()



ggsave(plot,filename = "img/stress_learn_girls.png",width = 10,height = 1.2*10)


##
## sprawdzenie czy zestresowani częściej poświęcają więcej czasu na jeden przedmiot
##

stressed_students <- left_join(names,data, by=c("genc3c"="CNT"))%>%
  select(-cow.name)%>%
  filter(genc3c=="POL",ST004D01T==1                                                 ## <---- TU ZMIENIAĆ PŁEĆ i kraj
         ,ST071Q01NA<31,ST071Q02NA<31,ST071Q03NA<31,ST071Q04NA<31,ST071Q05NA<31 )%>%
  
  group_by(CNTSTUID)%>%
  mutate( stress_lvl=sum(ST118Q01NA+ST118Q02NA+ST118Q03NA+ST118Q04NA+ST118Q05NA ),max_learning_time= (c(ST071Q01NA,ST071Q02NA,ST071Q03NA,ST071Q04NA,ST071Q05NA)))%>% # tylko czytanie
  select(stress_lvl,max_learning_time)

plot <- ggplot(stressed_students, aes(x=stress_lvl,y=max_learning_time))+
  labs(title = "Wykres zależności maksymalnego czasu nauki od ogólnego odczucia stresu ",subtitle = "DZIEWCZYNY")+
  stat_density_2d(aes(fill = stat(level)), geom = "polygon",alpha = 0.3)+
  coord_cartesian(ylim=c(0,17),expand = FALSE)+
  geom_smooth()+
  scale_fill_viridis_c()

ggsave(plot,filename = "img/stress_learn_max_girls.png",width = 1.2*10,height = 10)


 ### PLAYGROUND, have fun ###

# polskie dzieci bez w??asnego biurka, ile maj?? ksi????ek w domu
data_of_all_questions%>%
  filter(ST011Q09TA==2)%>%
  group_by(CNT,ST013Q01TA)%>%
  count()-> test_query


test_query%>%
  group_by(CNT)%>%
  summarise(count=sum(n)) -> test_count

test_query%>%
  filter(ST013Q01TA==5 | ST013Q01TA==6)%>%
  group_by(CNT)%>%
  summarise(sum=sum(n))-> test_sum


test <- data.frame(LOCATION=test_count$CNT,M=test_sum$sum/test_count$count*100)


write.csv(test,"res/250_ksiazek.csv")




  

#polskie dzieci bez w??asnego biurka, jaki etap edukacji chc?? uko??czy???
data_of_all_questions%>%
  filter(CNT=="POL",ST011Q09TA==2)%>%
  group_by(ST111Q01TA)%>%
  count()-> test_query2

data_of_all_questions%>%
  group_by(CNT,ST126Q01TA)%>%
  count() -> tq3


data_of_all_questions%>%
  filter(ST034Q06TA==2)


## Science phenomena ST129


data_of_all_questions%>%
  mutate(phenomena=ST129Q01TA+ST129Q02TA+ST129Q03TA+ST129Q04TA+ST129Q05TA+ST129Q06TA+ST129Q07TA+ST129Q08TA)%>%
  select(CNT,phenomena,ST078Q01NA,ST078Q08NA,ST078Q02NA,ST078Q06NA,ST078Q10NA,ST076Q01NA,ST076Q08NA,ST076Q02NA,ST076Q06NA,ST076Q10NA)->phenomena_df


phenomena_df%>%
  filter(ST078Q08NA==2 & ST076Q08NA==2&ST078Q01NA==2&ST076Q01NA==2)%>%
  group_by(phenomena)->phenomena_df2
  
  
ggplot(phenomena_df2,aes(x=phenomena))+
  geom_bar()
  