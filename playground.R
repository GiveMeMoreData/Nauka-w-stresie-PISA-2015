### Biblioteki ###

init <- function(){
  library(ggplot2)
  library(dplyr)
  require(maps)
  require(viridis)
  library(showtext) # do podrania czcionek z googla
  library(countrycode) # do przekształcenia kodów w pełne nazwy krajóW
  library(RColorBrewer)
  library(svglite)
  library(mapproj)
  options(stringsAsFactors = FALSE)
  
  
  
  font_add("Railway","Raleway-Regular.ttf") # nie widać różnicy w rstudio, ale widać różnicę po zapisaniu do plików
  font_families()
  
  
  
  ### Uzyskanie pełnych nazw krajów
  
  ### Funkcja Testy
  
  
  theme_set(
    theme_light() # or void
  )
  
  
}


init



#Ta funkcja będzie pasować do danych obrobionych przez program z interntu, którego kiedyś użyłaś do wyciągnięicia danych 
# Przyjmuje tą dziwną bazę z internetu, czyli kolumny ď.żLOCATION, SUBJECT, TIME, Value
rysuj_z_danych <- function(dane,tytul){
  
  theme_set(
    theme_void() # or void
  )
  
  ### Uzyskanie pełnych nazw krajów w danych
  names <- codelist[ codelist$genc3c %in% unlist(unique(dane["CNT"])),c("cow.name","genc3c")]
  
  europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                     "Czech Rep.","Denmark","Estonia","Finland","France",
                     "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                     "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                     "Portugal","Romania","Slovakia","Slovenia","Spain",
                     "Sweden","United Kingdom")
  
  # Ograniczenie do krajów europy
  names <- names[names$cow.name %in% europeanUnion,]
  
  
  
  plot_data <- dane%>%
    filter(TIME==2015,ď.żLOCATION!="OAVG", SUBJECT!="TOT")%>%  # Pozbycie się nieprzyjemnich danych 
    rename(CNT=ď.żLOCATION)%>%
    group_by(CNT)%>%
    summarise(diff=-diff(Value)/min(Value)*100)%>%
    right_join(names, by=c("CNT"="genc3c"))%>%
    select(-CNT)
  
  
  
  world_map <- map_data("world",region = plot_data$cow.name)
  world_map <- left_join(world_map,plot_data,by=c("region"="cow.name"))
  europe_map <- world_map[unlist(world_map[5]) %in% europeanUnion,]
  
  
  ggplot(europe_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(
          aes(fill=diff),
          color="lightgrey")+
    scale_fill_gradient2(
          name="Różnica w %",
          low="#fc363b", mid="white", high="#2766f6",
          guide = guide_legend(label.position = "bottom",
                           title.position = "top",
                           title.hjust = 0.5))+
    labs(title=tytul,
           subtitle = "Różnica wyników chłopców i dziewczyn",
           caption = "Data source: PISA 2015")+
    theme(
          text = element_text(family="Railway"),
          plot.title = element_text(size=20,hjust=0.5,face="bold"),
          plot.subtitle = element_text(size = 12,hjust=0.5),
          plot.caption = element_text(size=8,color="darkgrey"),
          plot.margin = unit(c(1,2,0,2),"cm"),
          legend.text = element_text(size=10),
          legend.title = element_text(face="bold",size = 8),
          legend.position = c(0.2,0.75),
          legend.direction = "horizontal",
          aspect.ratio = 1.2
          
    )
}

### Ta funkcja będzie pasować do danych wyjętych bezpośrednio z baz PISA
### Rysuje wykres różnicy pomiędzy chłopcami i dziewczynkami 
### Potrzebuje 3 kolumn Country, Sex, Value


dane <- stressed_stu_cnt
rysuj_z_danych2 <- function(dane,tytul){
  
  theme_set(
    theme_void() # or void
  )
  
  names <- codelist[ codelist$genc3c %in% unlist(unique(dane["Country"])),c("cow.name","genc3c")]
  
  europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                     "Czech Rep.","Denmark","Estonia","Finland","France",
                     "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                     "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                     "Portugal","Romania","Slovakia","Slovenia","Spain",
                     "Sweden","United Kingdom")
  
  ## Kraje europejskie
  names <- names[names$cow.name %in% europeanUnion,]
  
  plot_data <- dane%>%
    group_by(Country)%>%
    summarise(diff=diff(Value)/min(Value)*100)%>%
    right_join(names, by=c("Country"="genc3c"))%>%
    select(-Country)
  
  world_map <- map_data("world",region = plot_data$cow.name)
  world_map <- left_join(world_map,plot_data,by=c("region"="cow.name"))
  europe_map <- world_map[unlist(world_map[5]) %in% europeanUnion,]
  
  
  ggplot(europe_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(
      aes(fill=diff),
      color="lightgrey")+
    scale_fill_gradient2(
      name="Różnica w %",
      low="#fc363b", mid="white", high="#2766f6",
      guide = guide_legend(label.position = "bottom",
                           title.position = "top",
                           title.hjust = 0.5))+
    labs(title=tytul,
         subtitle = "Różnica wyników chłopców i dziewczyn",
         caption = "Data source: PISA 2015")+
    theme(
      text = element_text(family="Railway"),
      plot.title = element_text(size=20,hjust=0.5,face="bold"),
      plot.subtitle = element_text(size = 12,hjust=0.5),
      plot.caption = element_text(size=8,color="darkgrey"),
      plot.margin = unit(c(1,2,0,2),"cm"),
      legend.text = element_text(size=10),
      legend.title = element_text(face="bold",size = 8),
      legend.position = c(0.2,0.75),
      legend.direction = "horizontal",
      aspect.ratio = 1.2
      
    )
}

### Ta powinna robic to co 2 ale radzi sobie z krajami, które nie są w codelist
rysuj_z_danych3 <- function(dane,tytul){
  
  
  
  ### Uzyskanie pełnych nazw krajów w dane
  names <- codelist[ codelist$genc3c %in% unlist(unique(dane[1])),c("cow.name")]
  
  europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                     "Czech Rep.","Denmark","Estonia","Finland","France",
                     "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                     "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                     "Portugal","Romania","Slovakia","Slovenia","Spain",
                     "Sweden","United Kingdom")
  
  ### może wymagać edycji \/
  stu_15 <- dane[unlist(unique(dane[1]))%in%codelist$genc3c,]
  
  stu_15_diff <-data.frame(names=names,
                           diff=stu_15$M)
  
  world_map <- map_data("world",region = stu_15_diff$names)
  world_map <- left_join(world_map,stu_15_diff,by=c("region"="names"))
  europe_map <- world_map[unlist(world_map[5]) %in% europeanUnion,]
  
  
  ggplot(europe_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(
      aes(fill=diff),
      color="lightgrey")+
    scale_fill_gradient2(
      name="Różnica wyników w %",
      low="#fc363b", mid="white", high="#2766f6",
      guide = guide_legend(label.position = "bottom",
                           title.position = "top",
                           title.hjust = 0.5))+
    labs(title=tytul,
         subtitle = "Średnia dla każdego kraju",
         caption = "Data source: PISA 2015")+
    theme(
      text = element_text(family="Railway"),
      plot.title = element_text(size=20,hjust=0.5,face="bold"),
      plot.subtitle = element_text(size = 12,hjust=0.5),
      plot.caption = element_text(size=8,color="darkgrey"),
      plot.margin = unit(c(1,2,0,2),"cm"),
      legend.text = element_text(size=10),
      legend.title = element_text(face="bold",size = 8),
      legend.position = c(0.2,0.75),
      legend.direction = "horizontal",
      aspect.ratio = 1.2
      
    )
}

### 
### PRZYJMUJE DANE: Z PISA db, dla każdego kraju, bez podziału na płcie 
### Potrzebuje kolumn CNT, value

rysuj_z_danych4 <- function(dane,tytul){
  theme_set(
    theme_void() # or void
  )
  
  ### Uzyskanie pełnych nazw krajów w dane
  names <- codelist[ codelist$genc3c %in% unlist(unique(dane["CNT"])),c("cow.name","genc3c")]
  
  europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                     "Czech Rep.","Denmark","Estonia","Finland","France",
                     "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                     "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                     "Portugal","Romania","Slovakia","Slovenia","Spain",
                     "Sweden","United Kingdom")
  
  ## Kraje europejskie
  names <- names[names$cow.name %in% europeanUnion,]
  
  
  plot_data <- left_join(names,dane, by=c("genc3c"="CNT"))%>%
    select(cow.name,value)
  
  europe_map <- map_data("world",region = plot_data$cow.name)
  europe_map <- left_join(europe_map,plot_data,by=c("region"="cow.name"))
  
  
  ggplot(europe_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(
      aes(fill=value),
      color="lightgrey")+
    scale_fill_gradient2(
      name="Pytać autora :p (ogólnie %)",
      low="#fc363b", mid="white", high="#2766f6",
      guide = guide_legend(label.position = "bottom",
                           title.position = "top",
                           title.hjust = 0.5))+
    labs(title=tytul,
         subtitle = "Średnia dla każdego kraju",
         caption = "Data source: PISA 2015")+
    theme(
      text = element_text(family="Railway"),
      plot.title = element_text(size=20,hjust=0.5,face="bold"),
      plot.subtitle = element_text(size = 12,hjust=0.5),
      plot.caption = element_text(size=8,color="darkgrey"),
      plot.margin = unit(c(1,2,0,2),"cm"),
      legend.text = element_text(size=10),
      legend.title = element_text(face="bold",size = 8),
      legend.position = c(0.2,0.75),
      legend.direction = "horizontal",
      aspect.ratio = 1.2
      
    )
  
}

dane <- learn


## Country, Sex, Value
rysuj_mape_final <- function(dane,tytul){
    
    names <- codelist[ codelist$genc3c %in% unlist(unique(dane["Country"])),c("cow.name","genc3c")]
    
    europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                       "Czech Rep.","Denmark","Estonia","Finland","France",
                       "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                       "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                       "Portugal","Romania","Slovakia","Slovenia","Spain",
                       "Sweden","United Kingdom")
    
    ## Kraje europejskie
    names <- names[names$cow.name %in% europeanUnion,]
    
    
    
    
    
    plot_data <- dane%>%
        group_by(Country)%>%
        summarise(diff=diff(Value)/min(Value)*100)
    
    #%>%
    #right_join(names, by=c("Country"="genc3c"))%>%
    #    select(-Country)
    
    world_map <- map_data("world")
    world_map <- left_join(world_map,plot_data,by=c("region"="Country"))
    
    
    ggplot(world_map, aes(x = long, y = lat, group = group)) +
        geom_polygon(
            aes(fill=diff),color="white")+
        scale_fill_distiller(na.value = "#e3e3e3",type="seq",palette = "RdPu")+
        labs(title=tytul)+
        coord_map("conic",lat0=42,xlim=c(-10,30),ylim=c(35,71))+
        theme(
            text = element_text(family="Railway"),
            plot.title = element_text(size=20,hjust=0.5,face="bold"),
            legend.position = "None",
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()
            
        )
}

rysuj_mape_final(learn,"Kto częściej uczy się więcej?")


zapisz_do_pdf <- function(plot,filename){
  ggsave(plot,filename = filename,device = cairo_pdf,width = 6,height = 1.2*6)
}

zapisz_do_png <- function(plot,filename){
  ggsave(plot,filename = filename,width = 10,height = 1.2*10)
}

zapisz_do_svg <- function(plot,filename){
    svglite(filename, width = 10, height = 10)
    plot
    dev.off()
}
### Rysunki

###
###                 FINALNE WERSJE
###

###
### Różnica czasu uczenia się                                                                       FINAL
###

lrn_math <- read.csv("res/SelfStudyOver2h_maths.csv")
lrn_read <- read.csv("res/SelfStudyOver2h_reading.csv")
lrn_scie <- read.csv("res/SelfStudyOver2h_science.csv")


learn <- left_join(lrn_math,lrn_read,by=c("Location"="Location","Gender"="Gender"))%>%
    left_join(lrn_scie,by=c("Location"="Location","Gender"="Gender"))%>%
    mutate(Value=round((Value+Value.x+Value.y)/3,digits = 2),Sex=0+2*(Gender=="BOYS")+(Gender=="GIRLS"))%>%
    select(-Value.x,-Value.y,-Gender,Country=Location)

p <- rysuj_mape_final(learn,"Kto częściej uczy się więcej?")

p <- ggplot(plot_data)+
    geom_text(aes(x=1,y=reorder(Country,abs(diff)),label=paste0(Country," ",abs(round(diff,digits = 1)),"%"),color=abs(diff)),hjust=0.01,size=7)+
    scale_color_distiller(type="seq",palette = "RdPu",direction = 1)+
    theme(
        legend.position = "None",
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
    )

svglite("img/main_map_label_v2.svg", width = 10, height = 10)
p
dev.off()

zapisz_do_svg(p,"img/main_map.svg")

###
### Zależności stresu od uczenia się 
###








###
###                       FUNKCJA 1
###


math_stu <- data.frame(read.csv("res/math.csv"))
math <- rysuj_z_danych(math_stu,"Matematyka NOWE")
zapisz_do_png(math,"img/matematyka.png")

scie_stu <- read.csv("res/science.csv")
scie <- rysuj_z_danych(scie_stu,"Nauki ścisłe NOWE")
zapisz_do_png(scie,"img/nauki_scisle.png")

read_stu <- data.frame(read.csv("res/read.csv"))
read <- rysuj_z_danych(read_stu,"Czytanie NOWE")
zapisz_do_png(read,"img/czytanie.png")

###
###                       FUNKCJA 2
###


stressed_stu_cnt <- read.csv("res/stressed_stu_cnt.csv")
stressed_stu_cnt_plot <- rysuj_z_danych2(stressed_stu_cnt,"Różnica w zestresowaniu kobiet i mężczyzn")
zapisz_do_png(stressed_stu_cnt_plot,"img/stressed_stu_cnt.png")


###
### BAD TEACHERS
###


bad_teachers <- data.frame(read.csv("res/bad_teachers.csv"))
colnames(bad_teachers) <- c("Country","Sex","Value")
bad_plot <- rysuj_z_danych2(bad_teachers,"Źli nauczyciele")
zapisz_do_png(bad_plot,"img/bad_teachers_diff.png")








###
###                       FUNKCJA 4
###


###
###  Zestresowanie kobiet 
###


stressed_girls_cnt <- read.csv("res/stressed_girls_cnt.csv")
stressed_girls_cnt_plot <- rysuj_z_danych4(stressed_girls_cnt,"Procent zestresowanych kobiet")
zapisz_do_png(stressed_girls_cnt_plot,"img/stressed_girls_cnt.png")
ggplot(stressed_girls_cnt,aes(x=CNT,y=sort(value)))+
  geom_col()+
  geom_text(aes(label=value))


###
### BAD TEACHERS 
### ale dla 1 płci, by zobaczyć różnice odczuć w róznych krajach
###

bad_tch_girls <- data.frame(read.csv("res/bad_tch_girls.csv"))
tch_girls <- rysuj_z_danych4(bad_tch_girls,"Złe podejście do dziewczyn")
zapisz_do_png(tch_girls,"img/bad_tch_girls.png")

bad_tch_boys <- data.frame(read.csv("res/bad_tch_boys.csv"))
tch_boys <- rysuj_z_danych4(bad_tch_boys,"Złe podejście do chłopców")
zapisz_do_png(tch_boys,"img/bad_tch_boys.png")


###
### ONE GENDER SCHOOLS
### procent szkół zdominowanych przez jedną płeć 
### (różnica pomiędzy płcią w szkole większa niż 25%)
### uwzględniając tylko szkoły, które mają powyżej 50 uczniów 
###

# aktualnie nie pokazuje procent, tylko liczbe, by naprawić wytarczy zapisać baze w main jeszcze raz 
one_gender_sch <- read.csv("res/cnt_one_gender_sch.csv")
one_gender_sch_plot <- rysuj_z_danych4(one_gender_sch,"Częstość jednopółciowych szkół")
zapisz_do_png(one_gender_sch_plot,"img/one_gender_sch.png")


##
## Różnica w ilości szkół jednopółciowych kobiecych i męskich
##

one_gender_sch <- read.csv("res/cnt_one_gender_sch.csv")
colnames(one_gender_sch) <- c("CNT","diff","value")
one_gender_sch["value"] <- one_gender_sch["value"]*100
one_gender_sch_plot <- rysuj_z_danych4(one_gender_sch,"Różnica w ilości jednopółciowych szkół")
zapisz_do_png(one_gender_sch_plot,"img/one_gender_sch_diff.png")

###
### CLASSROOM SIZE
###

class_size <- read_csv("res/cnt_class_size.csv")
cnt_class_size_plot <- rysuj_z_danych4(cnt_class_size,"Średni rozmiar klasy")
zapisz_do_png(cnt_class_size_plot,"img/cnt_class_size.png")

###
### ŚREDNIE WYNIKI z testów
###

ave_math_stu <- data.frame(read.csv("res/AverageMathResult.csv"))
ave_math_stu <-  rename(ave_math_stu, CNT=LOCATION, value=M)
ave_math <- rysuj_z_danych4(ave_math_stu,"Matematyka Średnia")
zapisz_do_png(ave_math,"preska/ave_matematyka.png")

ave_scie_stu <- read.csv("res/AverageScienceResult.csv")
ave_scie_stu <-  rename(ave_scie_stu, CNT=LOCATION, value=M)
ave_scie <- rysuj_z_danych4(ave_scie_stu,"Nauki ścisłe Średnia")
zapisz_do_png(ave_scie,"preska/ave_nauki_scisle.png")

ave_read_stu <- data.frame(read.csv("res/AverageReadResult.csv"))
ave_read_stu <-  rename(ave_read_stu, CNT=LOCATION, value=M)
ave_read <- rysuj_z_danych4(ave_read_stu,"Czytanie Średnia")
zapisz_do_png(ave_read,"preska/ave_czytanie.png")

books <- read.csv("res/250_ksiazek.csv")
books <-  rename(books, CNT=LOCATION, value=M)
books_plot <- rysuj_z_danych4(books,"Procent społeczeństa\n z powyżej 250 książkami")
zapisz_do_png(books_plot,"books.png")
c <- codelist


ojcowie <- read.csv("res/WyksztalcenieOjcow.csv")

ojcowie <- ojcowie[order(ojcowie$srednie,decreasing = TRUE),]

