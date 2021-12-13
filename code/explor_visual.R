# Data exploration and visualizations
#-------------------------------------------------------------------------------

#Highlights:
#-------------------------------------------------------------------------------
# (1) We can see that the logs are indeed within the ACT and most are located in 
#Non Forest areas (map_plot_1).

# (2) The barplot shows the number of records by forest type in the dataset (dist_plot_1).

# (3) The dataset presents records for two order taxa in Reptilia 
#(lizards and snakes: Squamata, and turtles: Testudines) (facet_bar). The largest 
#number of records is for Squamata. In the visualization it is possible to 
#confirm which of the families of each order have the highest number of records.

# (4) It can be seen that the largest number of records in the two order taxa in 
#the dataset are found in non-forest areas (heat_map_1).

# (5) The largest number of records are occurrences of unknown nature, i.e. the way 
#in which the occurrence of the species was obtained is not recorded (heat_map_2).

# (6) It is possible to see who is the major supplier of records and their nature 
#on the ACT area (heat_map_2).
#-------------------------------------------------------------------------------


#Installing the required packages.

if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("ozmaps")) install.packages("ozmaps", dependencies = TRUE)

#Now we load the packages:

library(dplyr) #for data handling
library(ggplot2) #for graph visualizations
library(ozmaps) #for obtain border for maps visualizations

#Read the Downloaded the data
occ<-read.csv("code/downloaded_data.csv")

#-----------------------------------------------------------------------------------
# (1) We can see that the logs are indeed within the ACT and most are located in 
#Non Forest areas (map_plot_1) and most of them are from 'squamata' order (map_plot_1). 
#-------------------------------------------------------------------------------------

#polygon of ACT to make map plots

act<-ozmaps::ozmap_states %>% 
  filter(NAME == "Australian Capital Territory" | NAME=="Other Territories")

# (pre 1) check that the record points of the dataset are over ACT
ggplot() + 
  geom_sf(data = act, mapping = aes(), show.legend = FALSE) + 
  geom_point(data = occ, mapping = aes(x = decimalLongitude, y = decimalLatitude), 
             colour = "red", size=0.5) + 
  coord_sf(xlim=c(148.8,150.8), ylim = c(-36,-35))

# (1) Map of records w.r.t Forests of Australia 2013

a1<-ggplot() + 
  geom_sf(data = act, mapping = aes(), show.legend = FALSE) + 
  geom_point(data = subset(occ,cl10902!=""), 
             mapping = aes(x = decimalLongitude, 
                           y = decimalLatitude, colour = cl10902), size=0.5) + 
  coord_sf(xlim=c(148.8,150.8), ylim = c(-36,-35)) +
  scale_color_discrete(name = "Forests of Australia 2013")
a1
#save the visualization
ggsave("code/map_plot_1.png",plot=a1,width = 20,height = 15,units ="cm", dpi = 600)

# map of records occurencces by Order taxon

a1.1<- ggplot() + 
        geom_sf(data = act, mapping = aes(), show.legend = FALSE) + 
        geom_point(data = subset(occ, order!=""), mapping = aes(x = decimalLongitude, 
                                                                y = decimalLatitude, colour = order), size=0.5) + 
        coord_sf(xlim=c(148.8,150.8), ylim = c(-36,-35)) + 
        scale_color_discrete(name = "Order")

#save the visualization
ggsave("code/map_plot_2.png",plot=a1.1,width = 20,height = 15,units ="cm", dpi = 600)

#-----------------------------------------------------------------------------------------
# (2) The barplot shows the number of records by forest type in the dataset (dist_plot_1).
#-----------------------------------------------------------------------------------------

forest<-occ %>% group_by(cl10902) %>% summarise(n=n()) %>% 
  filter (cl10902!="") %>% arrange(desc(n))

a2<-ggplot(data= forest, aes(x=reorder(cl10902,n),y=n)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  geom_text(aes(label=n), position=position_dodge(1),hjust = -0.05) +
  ylim(0, 4300)+
  xlab("Forests of Australia 2013") + ylab("Nº records of Reptilia")
a2

#save the visualization
ggsave("code/dist_plot_1.png",plot=a2,width = 20,height = 15,units ="cm", dpi = 600)

# (2.1) The barplot shows the nature of the data record (dist_plot_2).

typeR<-occ %>% group_by(basisOfRecord) %>% summarise(n=n()) 
typeR #to view the table

a2.1<-ggplot(data=typeR, aes(x=reorder(basisOfRecord,n),y=n)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label=n), position=position_dodge(1),hjust = -0.05) +
  ylim(0, 4300)+coord_flip() + 
  xlab("Nature of records (basisOfRecord)") + 
  ylab("Nº records of Reptilia")
a2.1
#save the visualization
ggsave("code/dist_plot_2.png",plot=a2.1,width = 20,height = 15,units ="cm", dpi = 600)

# (pre-2.3) barplot of Order taxa
order<-occ %>% group_by(order) %>% summarise(n=n()) %>% filter(order!="")
ggplot(data= order, aes(x=order,y=n)) + geom_bar(stat = "identity") +
  coord_flip() + geom_text(aes(label=n), position=position_dodge(1),hjust = -0.05) +
  xlab("Order taxon") + ylab("nº records")

#------------------------------------------------------------------------------------------
# (3)  The barplot shows the number of records by family taxon faceted by order (facet_bar)
#------------------------------------------------------------------------------------------

order_family<-occ %>% group_by(order,family) %>% 
  summarise(n=n()) %>% filter(order!="" & family != "")

a3<-ggplot(data= order_family, aes(x=reorder(family,n),y=n)) + 
  geom_bar(stat = "identity", width = 0.5) + ylim(0, 4300)+ coord_flip() + 
  xlab("Family taxon") + ylab("Nº records") + 
  geom_text(aes(label=n), position=position_dodge(1),hjust = -0.05) + 
  facet_grid(rows=vars(order), scales = "free", space = "free")
a3
#save the visualization
ggsave("code/facet_bar.png",plot=a3,width = 20,height = 15,units ="cm", dpi = 600)


#-----------------------------------
# (4.1) "Order taxa" by type of forest
#-----------------------------------

order_by_forest<-occ %>% group_by(order,cl10902) %>% 
  summarise(n=n()) %>% filter(order!="" & cl10902 !="") %>% 
  arrange(desc(n),.by_group = TRUE)

ggplot(data= subset(occ, order !="" & cl10902!="") , aes(x=order,y=cl10902)) + 
  xlab("Order taxa") + 
  ylab("Forests of Australia 2013") + 
  geom_jitter(shape=16, position=position_jitter(0.3), alpha=0.15)

#heatmap
a4<-ggplot(order_by_forest, aes(x=order, y=cl10902, fill= n)) + 
  geom_tile() + geom_text(aes(label = n)) + 
  xlab("Order") + 
  ylab("Forests of Australia 2013") + 
  scale_fill_gradient(low = "#e5f5f9", high = "#2ca25f", name = "Nº records") + 
  theme(axis.text=element_text(size=8), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(fill = 'white', color = 'lightgray'))
a4
#save the visualization
ggsave("code/heat_map_1.png",plot=a4,width = 20,height = 15,units ="cm", dpi = 600)


#---------------------------------------------------
# (4.2) "natures of the records" by source of record
#---------------------------------------------------
# And which source mostly provides the different natures of the records?
#first check in a table:

basisR_by_rname<-occ %>% group_by(basisOfRecord,dataResourceName) %>% 
  summarise(n=n()) %>% arrange(desc(n),.by_group = TRUE)

#plot for better visualization
ggplot(data= occ , aes(x=basisOfRecord,y=dataResourceName)) + 
  xlab("basisOfRecord") + ylab("dataResourceName") + 
  geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.15) +
  theme(axis.text=element_text(size=8), 
        axis.text.x = element_text(angle = 45, hjust = 1))

# Heatmap 
a5<-ggplot(basisR_by_rname, aes(x=basisOfRecord, y=dataResourceName, fill= n)) + 
  geom_tile() + geom_text(aes(label = n)) + 
  xlab("Nature of records (basisOfRecord)") + 
  ylab("Source of the record (dataResourceName)") + 
  scale_fill_gradient(low = "#e5f5f9", high = "#2ca25f", name = "Nº records") + 
  theme(axis.text=element_text(size=8), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.background = element_rect(fill = 'white', color = 'lightgray'))
a5
#save the visualization
ggsave("code/heat_map_2.png",plot=a5,width = 20,height = 15,units ="cm", dpi = 600)
