#!/usr/bin/Rscript


#===============================================================================
# File Names       : 05.visualization.R 
# Date             : 31st Oct 2021
# Authors          : David Yen-Cheih Liao
# Purpose          : replicate figures in the paper
#                    Figure 6 ; Figure  7; Figure 9; Figure 10; Figure 11; 
#                    Figure 12 (Appendix 2); Figure 13 (Appendix 3);
#                    Figure 14 (Appendix 5); Figure 16 (Appendix 6)
# Required Dataset : redguard_estimates.csv;incident-group.csv;kyw_object.RData
#                    CoNLL-U-redgaurds.csv
# Output Data      : 
#===============================================================================



timer_task05 <- system.time({

# REQUIRED PACKAGES
# (No need to load the packages if replication is running in the pacakge )
#===============================================================================

# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(
#   ggplot2, ggpubr, ggrepel, wesanderson, ggraph,        # Visualization Toolkit
#   cowplot, lattice, ggraph, igraph, ggforce,
#   tidyverse, lubridate, dplyr, purrr, tibble,           # Tidyverse Toolkit 
#   tidyr, tidyr, readxl, data.table,                     
#   quanteda, tmcn, austin, udpipe, textrank              # NLP toolkit
# )
# 

  
# REQUIRED DATASET 
#===============================================================================
# redguard_estimates <- read_csv("data/redguard_estimates.csv", show_col_types = FALSE)
# individual_idea_point <- ead_csv("data/individual_idea_point.csv", show_col_types = FALSE)
# incident <- read_csv("data/incident-group.csv", show_col_types = FALSE)
# load("data/kyw_object.RData")
# load("data/pooled_outcome.RData")
# load("data/redgaurds_dfm.RData")


# Figure 4. An examples of grammar annotation reduced to noun, adjective and 
#           verb on Universal Dependencies.
#===============================================================================
udmodel_chinese <- udpipe_load_model(file = "chinese-gsdsimp-ud-2.5-191206.udpipe")
text_ch<- c("只要她反对毛主席，搞自己的一套，阳奉阴违，搞神秘化，我们就要反对她，
            揪出她！")
x <- udpipe_annotate(udmodel_chinese, x = text_ch)
x <- as.data.frame(x)

subtitle = "The Tokens of Part-of-Speech Tags Reduced to Noun, Verb and Adjective"
vertex_color = "#876635"
edge_color = "#3b1305"
size = 3
base_family =  "STHeiti"

x <- x[x$upos %in% c("NOUN", "VERB", "ADJ"),]
x <- x[!is.na(x$head_token_id), ]
x <- x[x$sentence_id %in% min(x$sentence_id), ]
edges <- x[x$head_token_id != 0, c("token_id", "head_token_id", "dep_rel")]
edges$label <- edges$dep_rel

relationshiptwo <- igraph::graph_from_data_frame(edges,
                                                 vertices = x[, c("token_id", "token", "upos")],
                                                 directed = TRUE) %>%
  ggraph::ggraph(layout = "linear") +
  ggraph::geom_edge_arc(ggplot2::aes(label = dep_rel, vjust = -0.20),
                        arrow = grid::arrow(length = ggplot2::unit(2, 'mm'), ends = "last", type = "closed"),
                        end_cap = ggraph::label_rect("wordswordswords"),
                        label_colour = edge_color,
                        check_overlap = TRUE, 
                        label_size = 3, 
                        family = "STHeiti") +
  ggraph::geom_node_label(ggplot2::aes(label = token), col = vertex_color, size = 3, fontface = "bold", family = "STHeiti" ) +
  ggraph::geom_node_text(aes(label = upos), nudge_y = -0.45, size = 2.5, repel = FALSE, family = "STHeiti", col = "#0c343d", fontface = "bold") +
  labs(title = "", subtitle = subtitle, caption = "")+
  theme_graph()

ggsave("images/relationship-b.png", 
       width = 7, height = 4.5, 
       units = "in", # other options c("in", "cm", "mm"), 
       dpi = 200)



# Figure 6. Visualisation of the co-occurrences for the top 15 most frequent 
#           co-occurring nouns and adjectives from documents within each major 
#           historical incidents
#===============================================================================

# split the data by each incident and save them into list object
# re-index the incident index in to numeric number in order 

incident_list <- split(incident , incident$incident_index)
names(incident_list) <- seq(1, length(names(incident_list))) 
conll$keyword_doc_id <- conll$doc_id
for (i in 1:length(incident_list)){
  conll$keyword_doc_id[ifelse(conll$keyword_doc_id %in% incident_list[[i]]$incident_index, TRUE, FALSE)]  <- i  
}

#  Figure 6. (a) The First Marxist-Leninist Wallposter

first_incident <- cooccurrence(x = subset(conll[conll$keyword_doc_id ==1,], upos %in% c("NOUN", "ADJ")), 
                               term = "lemma", 
                               group = c("doc_id", "paragraph_id", "sentence_id")) %>%
  head(15) %>%
  as_tibble()

colnames(first_incident)[3] <- "Co-Occurrence Windows"

# Append the English translation
first_incident$term1_chinese <- c("Class",      # 阶级
                                  "Chairman",   # 主席
                                  "Cultural",   # 文化
                                  "ism",        # 主义 
                                  "Chairman",   # 主席
                                  "Proletariat",# 无产 
                                  "Mao",        # 毛
                                  "Proletariat",# 无产
                                  "Chairman",   # 主席
                                  "Mass",       # 群众
                                  "ism",        # 主义
                                  "Personal",   # 个
                                  "Battle",     # 斗争
                                  "Thought",    # 思想
                                  "Bourgeois")  # 资产

first_incident$term2_chinese <- c("Revolution", # 革命
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Mao",        # 毛
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Class",      # 阶级
                                  "Class",      # 阶级
                                  "Revolution", # 革命
                                  "Class",      # 阶级
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Class")      # 阶级

first_incident$term1 <- paste(first_incident$term1, first_incident$term1_chinese, sep = "\n")
first_incident$term2 <- paste(first_incident$term2, first_incident$term2_chinese,sep = "\n")


#  Figure 6. (b) The Zhou Enlai’s Declaration
third_incident <- cooccurrence(x = subset(conll[conll$keyword_doc_id ==3,], upos %in% c("NOUN", "ADJ")), 
                               term = "lemma", 
                               group = c("doc_id", "paragraph_id", "sentence_id")) %>%
  head(15) %>%
  as_tibble()
colnames(third_incident)[3] <- "Co-Occurrence Windows"

# Append the English translation
third_incident$term1_chinese <- c("Class",      # 阶级
                                  "Chairman",   # 主席
                                  "ism",        # 主义
                                  "Proletarian",# 无产
                                  "Lines",      # 路线
                                  "Bourgeois",  # 资产
                                  "Thought",    # 思想
                                  "Cultural",   # 文化
                                  "Proletarian",# 无产
                                  "Lines",      # 路线
                                  "Chairman",   # 主席
                                  "Personal",   # 个
                                  "Mass",       # 群众
                                  "Mao",        # 毛
                                  "Bourgeois")  # 资产

third_incident$term2_chinese <- c("Revolution", # 革命 
                                  "Revolution", # 革命 
                                  "Revolution", # 革命 
                                  "Revolution", # 革命 
                                  "Revolution", # 革命 
                                  "Class",      # 阶级 
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Class",      # 阶级 
                                  "Class",      # 阶级 
                                  "Class",      # 阶级 
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Revolution") # 革命

third_incident$term1 <- paste(third_incident$term1, third_incident$term1_chinese, sep = "\n")
third_incident$term2 <- paste(third_incident$term2, third_incident$term2_chinese,sep = "\n")

# Figure 6. (c) The Announcement of New Public Security Regulations

fourth_incident <- cooccurrence(x = subset(conll[conll$keyword_doc_id == 4,], 
                                           upos %in% c("NOUN", "ADJ")), 
                                term = "lemma", 
                                group = c("doc_id", "paragraph_id", "sentence_id")) %>%
  head(15)  %>%
  as_tibble()

colnames(fourth_incident)[3] <- "Co-Occurrence Windows"

# Append the English translation
fourth_incident$term1_chinese <- c("Class",      # 阶级
                                   "ism",        # 主义
                                   "Proletarian",# 无产  
                                   "Chairman",   # 主席
                                   "Proletarian",# 无产
                                   "Lines",      # 路线
                                   "Mass",       # 群众
                                   "ism",        # 主义
                                   "Cultural",   # 文化
                                   "Lines",      # 路线
                                   "Bourgeois",  # 资产
                                   "Mao",        # 毛
                                   "ist",        # 分子
                                   "ism",        # 主义
                                   "Chairman")   # 主席

fourth_incident$term2_chinese <- c("Revolution", # 革命
                                   "Revolution", # 革命
                                   "Revolution", # 革命
                                   "Revolution", # 革命
                                   "Class",      # 阶级
                                   "Revolution", # 革命
                                   "Revolution", # 革命
                                   "Class",      # 阶级
                                   "Revolution", # 革命
                                   "Class",      # 阶级
                                   "Class",      # 阶级
                                   "Revolution", # 革命
                                   "Revolution", # 革命
                                   "Capital",    # 资本
                                   "Mao")        # 毛

fourth_incident$term1 <- paste(fourth_incident$term1, fourth_incident$term1_chinese, sep = "\n")
fourth_incident$term2 <- paste(fourth_incident$term2, fourth_incident$term2_chinese,sep = "\n")

#  Figure 6. (d) The February Countercurrent
fifth_incident <- cooccurrence(x = subset(conll[conll$keyword_doc_id ==5,], upos %in% c("NOUN", "ADJ")), 
                               term = "lemma", 
                               group = c("doc_id", "paragraph_id", "sentence_id")) %>%
  head(15) %>%
  as_tibble()

colnames(fifth_incident)[3] <- "Co-Occurrence Windows"

# Append the English translation
fifth_incident$term1_chinese <- c("Class",      # 阶级
                                  "Proletariat",# 无产
                                  "Proletariat",# 无产 
                                  "Capital",    # 资产
                                  "Chairman",   # 主席
                                  "ism",        # 主义
                                  "Lines",      # 路线
                                  "Cultural",   # 文化
                                  "Chairman",   # 主席
                                  "Chairman",   # 主席
                                  "ism",        # 主义
                                  "Lines",      # 路线
                                  "Personal",   # 个
                                  "Personal",   # 个
                                  "Cultural")   # 文化

fifth_incident$term2_chinese <- c("Revolution", # 革命
                                  "Class",      # 阶级
                                  "Revolution", # 革命
                                  "Class",      # 阶级
                                  "Mao",        # 毛
                                  "Revolution", # 革命
                                  "Class",      # 阶级
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Class",      # 阶级
                                  "Class",      # 阶级
                                  "Revolution", # 革命
                                  "Revolution", # 革命
                                  "Class",      # 阶级
                                  "Class")      # 阶级

fifth_incident$term1 <- paste(fifth_incident$term1, fifth_incident$term1_chinese, sep = "\n")
fifth_incident$term2 <- paste(fifth_incident$term2, fifth_incident$term2_chinese, sep = "\n")

# run the plot Figure 6(a), Figure 6(b), Figure 6(c), and Figure 6(d) and save them together

major_incidents <- list(first_incident, third_incident, fourth_incident, fifth_incident)
edge_colour_set <- c("#d4db87", "grey", "#29000f", "#29000f", "#4b0909")
color_palette_set <- c("#DBB887", "#1380A1", "#588300",  "#e69138")

incidents_cooc <- NULL

for (i in 1:length(major_incidents)) {
  incidents_cooc[[i]] <-  major_incidents[[i]] %>%
    graph_from_data_frame()  %>%
    ggraph( layout = "kk") +
    geom_edge_link(aes(width = `Co-Occurrence Windows`, edge_alpha = `Co-Occurrence Windows`), edge_colour = edge_colour_set[[i]]) +
    geom_node_point(size = 75, shape =15, color = color_palette_set[[i]] , alpha = 0.95) +
    geom_node_point(size = 84, shape =22, color = "black") +
    geom_node_text(aes(label = name), size = 12, repel = FALSE, family = "STHeiti", col = "black", fontface = "bold") +
    theme_graph(base_family = "Arial Narrow") +
    labs(title = "", subtitle = "") +
    theme(text = element_text(family="STHeiti")) +
    theme_void() +
    theme(legend.key.size = unit(4.5, "cm"),  
          legend.key.height = unit(4.5, "cm"),  
          legend.key.width = unit(4.5, "cm"),  
          legend.title = element_text(size=30), 
          legend.text = element_text(size=28)) }


plot_names <- c("first_incident_p.png", "third_incident_p.png", "fourth_incident_p.png", "fifth_incident_p.png")
for (i in 1:4) {
  file_name <- paste("images/" ,plot_names[i] , ".png", sep="")
  print(incidents_cooc[[i]])
  ggsave(file_name, 
         width = 30, 
         height = 28, 
         units = "in",
         dpi = 150)
  
  dev.off()
}



# Figure 7. Four examples of the top 10 keyword phrases identified by TextRank 
#           within the documents of each politicalincident since 1966.
#===============================================================================

# Figure 7 (a) The First Marxist-Leninist Wallposter
incident_1st <- head(kyw_object[[1]]$keywords[nchar(kyw_object[[1]]$keywords$keyword)>2,], n = 10)
incident_1st["keyword_eng"] <- c("Chairman Mao",           # 毛主席
                                 "The Cultural Revolution",# 文化大革命 
                                 "The Red Guards",         # 红卫兵
                                 "The Great Revolution",   # 大革命
                                 "The Bourgeoisie",        # 资产阶级
                                 "Proletarian Culture",    # 无产阶级文化
                                 "Class Culture",          # 阶级文化大
                                 "Party Central Committee",# 党中央
                                 "The Proletariat",        # 无产阶级
                                 "The Revisionism")        # 修正主义

incident_1st["keyword_bi"] <- paste(incident_1st$keyword, "\n", incident_1st$keyword_eng)

# Figure 7 (b) The Zhou Enlai’s Declaration
incident_3rd <- head(kyw_object[[3]]$keywords[nchar(kyw_object[[3]]$keywords$keyword)>2,], n = 10)

incident_3rd["keyword_eng"] <- c("Chairman Mao",           # 毛主席
                                 "The Cultural Revolution",# 文化大革命 
                                 "Bourgeois Reaction",     # 资产阶级反动
                                 "Reactionary Forces",     # 反动路线
                                 "Class Reactionary Line", # 阶级反动路线
                                 "The Great revolution",   # 大革命
                                 "Proletarian Culture",    # 无产阶级文化
                                 "Class Culture",          # 阶级文化大
                                 "Proletarian Revolution", # 无产阶级革命
                                 "Revolutionary Line")     # 革命路线
incident_3rd["keyword_bi"] <- paste(incident_3rd$keyword, "\n", incident_3rd$keyword_eng)

# Figure 7 (c) The February Countercurrent
incident_4th <- head(kyw_object[[4]]$keywords[nchar(kyw_object[[4]]$keywords$keyword)>2,], n = 10)
incident_4th["keyword_eng"] <- c("Chairman Mao",           # 毛主席
                                 "The Cultural Revolution",# 文化大革命
                                 "Revolutionary Rebels",   # 革命造反派
                                 "Capitalist Roader",      # 当权派
                                 "Capitalist Road",        # 资本主义道路
                                 "Reactionary Line",       # 反动路线
                                 "Bourgeois Reaction",     # 资产阶级反动
                                 "Class Reactionary Line", # 阶级反动路线
                                 "Proletarian Culture",    # 无产阶级文化
                                 "Class Culture")          # 阶级文化大
incident_4th["keyword_bi"] <- paste(incident_4th$keyword, "\n",incident_4th$keyword_eng)


# Figure 7 (d) The Adopt a Correct Attitude toward the Little General

incident_5th <- head(kyw_object[[5]]$keywords[nchar(kyw_object[[5]]$keywords$keyword)>2,], n = 10)
incident_5th["keyword_eng"] <- c("Chairman Mao",          # 毛主席
                                 "Cultural Revolution",   # 文化大革命
                                 "Proletarian Revolution",# 无产阶级革命
                                 "Great Revolution",      # 大革命
                                 "Bourgeois Reaction",    # 资产阶级反动
                                 "Class Reactionary Line",# 阶级反动路线
                                 "Reactionary Line",      # 反动路线
                                 "Proletarian Culture",   # 无产阶级文化
                                 "Professional Revolutionary League", # 省革联
                                 "Class Culture Great")   # 阶级文化大
incident_5th["keyword_bi"] <- paste(incident_5th$keyword, "\n", incident_5th$keyword_eng)


# plot Figure 7 (a), Figure 7 (b), Figure 7 (c), and Figure 7(d) and save them together

major_incidents <- list(incident_1st, incident_3rd, incident_4th, incident_5th)
color_palette_set <- c("#DBB887", "#1380A1", "#588300",  "#e69138")


key_major_incidents <- NULL
for (i in  1:length(major_incidents)) {
  key_major_incidents[[i]] <- ggplot(data= major_incidents[[i]], aes(x=reorder(keyword_bi,freq), y=freq)) +
    geom_bar(stat="identity", fill= color_palette_set[[i]])+
    coord_flip() + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +  
    theme(text = element_text(family="STHeiti", size=14)) +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(plot.title = element_text(size=14)) +
    theme(text = element_text(size=14),
          axis.text.y = element_text(size=30),
          axis.text.x = element_text(size=22)) +
    theme(axis.title = element_text(size = 8))+     
    labs(title = "", x = "", y = "") +
    ylim(0, 400)
}


plot_names <- c("incident_1st", "incident_3rd", "incident_4th", "incident_5th")
for (i in 1:4) {
  file_name <- paste("images/" ,plot_names[i] , ".png", sep="")
  print(key_major_incidents[[i]])
  ggsave(file_name, 
         width = 16,
         height = 14, 
         units = "in",
         dpi = 150)
  dev.off()
}


# Figure 8 Smoothed density distributions of estimated positions for the Red 
#          Guard participants
#===============================================================================
colors <- c("Rebel" = "#972D15", "Conservative" = "#81A88D")

density <-ggplot(redguard_estimates, 
                    aes(x = x, fill = fact_eng, color = fact_eng)) +
  geom_density(alpha = 0.7) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  geom_rug(data = redguard_estimates[redguard_estimates$fact_eng %in% "Rebel", ], ) +
  geom_rug(data = redguard_estimates[redguard_estimates$fact_eng %in% "Conservative", ], 
           sides = "t") +
  ylim(0, 10) +
  xlim(-1, 1) +
  ylab(NULL) +
  theme_minimal_hgrid(14) +
  theme(legend.position = c(0.7, 0.8)) +
  scale_fill_manual(name = "The Red Gaurd Factions", 
                    values = c("#81A88D", "#972D15"), 
                    labels =c("The Conservatives","The Rebels"))+
  scale_colour_manual(name = "The Red Gaurd Factions", 
                      values = c("#81A88D", "#972D15"), 
                      labels = c("The Conservatives","The Rebels" )) +
  scale_y_continuous(breaks = function(x) to_integer(x, n = 10)) +
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y = element_blank()) +
  xlab("Smoothed Density Distributions of Estimated Ideal Positions")

ggsave("images/density.png", width = 8, height = 4, 
       units = "in", dpi = 150)




# Figure 9. Estimated positions for individual participant and organization
#===============================================================================
# Subset Rebel's position over the mean of the Conservative which includes: 
# 清華八八派, 清华大学八一一战斗小组, 北京一中少數派, 北京六中少數派, 
# 北京轻工业学院东方红公社, 北京家庭出身问题第三研究小组
rebel_outlier <- redguard_estimates[(redguard_estimates$x> mean(redguard_estimates[redguard_estimates$fact_eng %in% "Conservative",]$x) & redguard_estimates$fact_eng == "Rebel"),]$activist

# Subset Conservative's position over the mean of the Rebel which includes: 贺捷生
conservative_outlier <- redguard_estimates[(redguard_estimates$x< mean(redguard_estimates[redguard_estimates$fact_eng %in% "Rebel",]$x)& redguard_estimates$fact_eng == "Conservative"),]$activist

# Create a list for outliers
outlier <- c(rebel_outlier, conservative_outlier)

# Append the factions's English name to the dataframe
english_name <- c("Tsinghua 88 Faction",
                  "Tsinghua University 811 Combat Group",
                  "Beijing No. 1 High School Mnority Faction", 
                  "Beijing No. 6 High School Mnority Faction",
                  "He Jiesheng",               
                  "Beijing Institute of Light Industry East Is Red Commune",
                  "Beijing Family Origin Issue No. 3 Research Group")

redguard_estimates$english_unit <- redguard_estimates$activist
redguard_estimates[redguard_estimates$activist %in% outlier, "english_unit"] <- english_name

# Label the extreme activits/organizations
conservative <- c("贺捷生")
rebel <- c("清華八八派", "清华大学八一一战斗小组", "北京一中少數派", 
           "北京六中少數派","北京轻工业学院东方红公社", 
           "北京家庭出身问题第三研究小组")


ideal_point <- ggplot(redguard_estimates, aes(x = english_unit, y = x, colour = factor(fact_eng))) +
  geom_point(aes(x = reorder(english_unit, x), y = x, colour = fact_eng, fill = fact_eng), alpha = 0.5, size = 0.5) + 
  geom_pointrange(aes(ymin = lower, ymax = upper), alpha = 0.7, size = 0.6) +
  # labeling the faction
  geom_text(aes(x = english_unit, y = x, label = english_unit), 
            fontface = "bold",
            hjust = 1.5,
            data = redguard_estimates[redguard_estimates$activist %in% conservative,],
            color = "#40776f", family = "STHeiti",
            position=position_jitter(width = 1.05, height = -2.05), 
            size = 3 ) +   
  geom_text(aes(english_unit, x, label = english_unit), 
            fontface = "bold",
            hjust = -0.18,
            data = redguard_estimates[redguard_estimates$activist %in% rebel[rebel %in% c("清华大学八一一战斗小组", "北京轻工业学院东方红公社")],],
            color = "#301c35", family = "STHeiti",
            position = position_jitter(width = -3, height = -3),
            size = 2 ) +    
  geom_text(aes(english_unit, x, label = english_unit), 
            fontface = "bold",
            hjust = -0.18,
            data = redguard_estimates[redguard_estimates$activist %in% rebel[!rebel %in% c("清华大学八一一战斗小组", "北京轻工业学院东方红公社")],],
            color = "#301c35", family = "STHeiti",
            position = position_jitter(width = -5.05, height = -3),
            size = 3 ) +  
  coord_flip() + 
  ylab("") + 
  xlab("") +
  # Mean of Conservative
  geom_hline(yintercept = mean(redguard_estimates[redguard_estimates$fact_eng %in% "Conservative",]$x),
             linetype="dashed", col = "#40776f") +
  annotate("text", x = 70, y = 0.4,
           label = "Mean of Estimated Ideal \n Point for the Conservatives", colour = "#40776f", 
           size = 4, alpha=1) +
  # Mean of Rebel
  geom_hline(yintercept = mean(redguard_estimates[redguard_estimates$fact_eng %in% "Rebel",]$x), 
             linetype="dashed", col = "#301c35") +
  annotate("text", x = 70, y = -0.35,
           label = "Mean of Estimated Ideal \n Point for the Rebels", colour = "#301c35", 
           size = 4, alpha = 1) +
  scale_fill_manual(name = "", values = c("#81A88D", "#972D15"), labels = c("The Conservatives","The Rebels"))+
  scale_colour_manual(name = "", values = c("#81A88D", "#972D15"), labels = c("The Conservatives","The Rebels"))+
  theme(text = element_text(size=14, family="STHeiti")) +
  theme(legend.text = element_text(size=30), size=rel(1))+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +  
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = "", x = "",
       y = "Estimated Ideal Points", 
       color = "Faction Types")  +
  theme(legend.position = c(1, .5),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  ylim(c(-1,1))

ggsave("images/ideal_point.png", width = 8.5, height = 7, 
       units = "in", dpi = 200)


# Figure 10. The estimated positions of the Red Guard participants by four major
#            political incident 
#===============================================================================
colors <- c("Rebel" = "#972D15", "Conservative" = "#81A88D")
incident_selects <- ggplot(individual_idea_point[individual_idea_point$incidents_index %in% c(1,2,3,4,5),], 
                           aes(x= x, y=fact_eng, color=fact_eng, group = fact_eng, fill = fact_eng)) +
  geom_violin(alpha= 0.6)  +
  facet_grid(rows = vars(incidents)) +
  geom_jitter(shape=16,  size = 1,
              position=position_jitter(seed = 42)) +
  xlim(-1,1) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_fill_manual(name = "The Red Gaurd Factions", 
                    values = c("#81A88D", "#972D15"), 
                    labels =c("The Conservatives","The Rebels"))+
  scale_colour_manual(name = "The Red Gaurd Factions", 
                      values = c("#81A88D", "#972D15"), 
                      labels = c("The Conservatives","The Rebels" )) +
  scale_x_continuous(breaks = function(x) to_integer(x, n = 10)) +
  theme_cowplot(12) +
  xlab("Estimated Ideal Points for Student Participants from Major Incidents") +
  ylab(NULL) +
  theme(legend.position = "none")  +
  xlim(-2,2) +
  theme(strip.text.y.right = element_text(angle = 0))

ggsave("images/incident_selects.png", width = 10, height = 6, 
       units = "in", dpi = 200)


# Figure 11. Top 20 most frequent TextRank-Keyword estimated by Wordfish Poisson
#            model
#===============================================================================
word_point <- data.frame(feature = rownames(pooled_outcome$means$beta), 
                         psi = pooled_outcome$means$alpha,
                         beta = pooled_outcome$means$beta)

top_keywords <- redgaurds_dfm %>%
  colSums() %>%
  as.data.frame() %>%
  arrange(desc(.)) %>%
  head(20) %>%
  rownames() 


top_keywords_list <- redgaurds_dfm %>%
  colSums() %>%
  as.data.frame() %>%
  arrange(desc(.)) 

top_keywords_phrases <- rownames_to_column(top_keywords_list)[nchar(rownames(top_keywords_list))>2,] %>%
  head(30) %>%
  select(rowname) %>%
  pull()




# word_point[word_point$feature %in% top_keywords, ]$feature

word_point[word_point$feature %in% top_keywords, "top_keywords"] <- c("Revolution",
                                                                      "Chairman Mao",
                                                                      "Class",
                                                                      "ism",
                                                                      "Battles",
                                                                      "Masses",
                                                                      "The Cultural Revolution",
                                                                      "Thoughts",
                                                                      "Lines",
                                                                      "Red Guards",
                                                                      "Comrades",
                                                                      "Work",
                                                                      "Elements",
                                                                      "Rebellion",
                                                                      "People",
                                                                      "Great",
                                                                      "Proletariat",
                                                                      "Counter-revolution",
                                                                      "Reaction",
                                                                      "Capital")

word_point$keyword_bi <-paste(word_point$feature, word_point$top_keywords, sep = "\n")


# word_point[word_point$feature %in% top_keywords_phrases, ]$feature

word_point[word_point$feature %in% top_keywords_phrases, "top_keywords_phrases"] <- c("Chairman Mao",
                                                                 "The Cultural Revolution",
                                                                 "The Red Guards",
                                                                 "Proletariat",
                                                                 "Counter-revolution",
                                                                 "the monsters and freaks",
                                                                 "Vigorous",
                                                                 "Together",
                                                                 "Rightists",
                                                                 "Is it right?",
                                                                 "petite bourgeoisie" ,
                                                                 "some people",
                                                                 "Big-character poster",
                                                                 "under conditions",
                                                                 "in the world",
                                                                 "mainly",
                                                                 "increasing vigilance",
                                                                 "mostly",
                                                                 "headquarters",
                                                                 "Mao Zedong",
                                                                 "Royalists",
                                                                 "douchepants",
                                                                 "Accidentalist",
                                                                 "Can't",
                                                                 "Call",
                                                                 "Can't see",
                                                                 "The State Council of PRC",
                                                                 "denunciations and purges",
                                                                 "Old man",
                                                                 "Guest house")


word_point$phrases_bi <-paste(word_point$feature, word_point$top_keywords_phrases, sep = "\n")


y_max <- max(word_point[word_point$feature %in% top_keywords,]$psi) + 1 
y_min <- min(word_point[word_point$feature %in% top_keywords,]$psi) - 1




theme_set(theme_bw(16))
estimated_x <-  ggplot(data = word_point, aes(x = beta, y = psi, label = feature)) + 
  geom_text(colour = "grey70",family = "STHeiti", alpha = 0.4, size = 1.5) +
  geom_text(aes(beta, psi, label = keyword_bi, fontface = "bold"), 
            data = word_point[word_point$feature %in% top_keywords,],
            color = "#CC8400", family = "STHeiti", position=position_jitter(width = 0.9, height = 1), size = 1.5 , alpha = 0.9) +
  geom_text(aes(beta, psi, label = phrases_bi, fontface = "bold"), 
            data = word_point[word_point$feature %in% top_keywords_phrases,],
            color = "#cc1e00", family = "STHeiti", position=position_jitter(width = 0.9, height = 1), size = 1.5 , alpha = 0.9) +  theme_bw() +
  facet_zoom(xlim = c(-2, 2), ylim = c(y_min,y_max)) +
  theme_cowplot(12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(family="STHeiti")) +  
  xlab("Estimated x") +
  ylab("Estimates Ψ") 


ggsave("images/estimated_x.png", width = 5, height = 3, 
       units = "in", dpi = 150)



# Figure 12. Frequency statistics of parts of speech from the Red Guard 
#            publications (Appendix 2).
#===============================================================================
stats <- txt_freq(conll$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
pos_vis <- ggplot(data=stats, aes(x=key, y=freq_pct)) +
  geom_bar(stat="identity", fill="#b47846") +
  geom_bar(data=stats[stats$key %in% c("NOUN", "VERB", "ADJ"),], aes(x=key, y=freq_pct), stat="identity", fill="steelblue")+
  theme_minimal()  +
  xlab("Universal Parts of Speech") +
  ylab("Frequency Percentage of Occurrence") +
  theme_half_open() +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) 

ggsave("images/pos_vis.png", width = 10, height = 6, 
       units = "in", dpi = 150)


# Figure  13. An  example  of  complete  grammar  annotation  on  Universal 
#             Dependencies.   
#===============================================================================
udmodel_chinese <- udpipe_load_model(file = "chinese-gsdsimp-ud-2.5-191206.udpipe")
text_ch <- c("只要她反对毛主席，搞自己的一套，阳奉阴违，搞神秘化，我们就要反对她，
            揪出她！")
x <- udpipe_annotate(udmodel_chinese, x = text_ch)
x <- as.data.frame(x)

subtitle = "The Original Sentence Annotated in UD Framework"
vertex_color = "#876635"
edge_color = "#3b1305"
size = 3
base_family =  "STHeiti"

x <- x[!is.na(x$head_token_id), ]
x <- x[x$sentence_id %in% min(x$sentence_id), ]
edges <- x[x$head_token_id != 0, c("token_id", "head_token_id", "dep_rel")]
edges$label <- edges$dep_rel

relationshipone <- igraph::graph_from_data_frame(edges,
                                                 vertices = x[, c("token_id", "token", "upos")],
                                                 directed = TRUE) %>%
  ggraph::ggraph(layout = "linear") +
  ggraph::geom_edge_arc(ggplot2::aes(label = dep_rel, vjust = -0.20),
                        arrow = grid::arrow(length = ggplot2::unit(2, 'mm'), ends = "last", type = "closed"),
                        end_cap = ggraph::label_rect("wordswordswords"),
                        label_colour = edge_color,
                        check_overlap = TRUE, 
                        label_size = 3, 
                        family = "STHeiti") +
  ggraph::geom_node_label(ggplot2::aes(label = token), col = vertex_color, size = 3, fontface = "bold", family = "STHeiti" ) +
  ggraph::geom_node_text(aes(label = upos), nudge_y = -0.45, size = 2.5, repel = FALSE, family = "STHeiti", col = "#0c343d", fontface = "bold") +
  labs(title = "", subtitle = subtitle, caption = "")+
  theme_graph()

ggsave("images/relationship-a.png", width = 10, height = 8, 
       units = "in", dpi = 160)


# Figure 14. Most frequent key phrases in top 10 identified by TextRank from 
#            each incidents
#===============================================================================
time <- list("25 May 1966 - 17 Aug 1966",
             "18 Aug 1966 - 26 Sep 1966",
             "26 Sep 1966 - 16 Jan 1967",
             "17 Jan 1967 - 15 Feb 1967",
             "16 Feb 1967 -  1 Apr 1967",
             " 2 Apr 1967 - 19 Jul 1967",
             "20 Jul 1967 - 22 Aug 1967",
             "22 Aug 1967 - 22 Apr 1968",
             "23 Apr 1968 - 27 Jul 1968",
             "28 Jul 1968 -  3 Jan 1967",
             " 4 Jan 1967")

incidents <- list("The First Marxist-Leninist Wall Poster",
                  "The Red August",
                  "The Zhou Enlai's Declaration",
                  "The Announcement of New Public \n Security Regulations", 
                  "The February Countercurrent",
                  "The Little General",
                  "The Wuhan Incident",
                  "The First Great Tiananmen Rally",
                  "The 100-day Clashes",
                  "The Mao Zedong's Summons",
                  "The Shanghai January Storm")

color_palette_set <- c("#DBB887",
                       "#838b8b", 
                       "#1380A1", 
                       "#588300", 
                       "#e69138", 
                       "#838b8b",
                       "#838b8b", 
                       "#838b8b",
                       "#838b8b",
                       "#838b8b",
                       "#838b8b")

key <- NULL
for (i in  1:length(kyw_object)) {
  key[[i]] <- ggplot(data= head(kyw_object[[i]]$keywords[nchar(kyw_object[[i]]$keywords$keyword)>2,], n = 10), aes(x=reorder(keyword,freq), y=freq)) +
    geom_bar(stat="identity", fill=color_palette_set[[i]])+
    coord_flip() + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +  
    theme(text = element_text(family="STHeiti", size=9)) +
    theme(panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    theme(plot.title = element_text(size=26, face="bold")) +
    theme(text = element_text(size=26),
          axis.text.y = element_text(size=22),
          axis.text.x = element_text(size=20)) +
    theme(axis.text=element_text(size=22),
          axis.title=element_text(size=22,face="bold")) +
    annotate("text", x=1, y=500, label= time[[i]], size = 7.5) + 
    labs(title = incidents[[i]], x = "", y = "") +
    ylim(c(0,1000))
}


keywords <- annotate_figure(
  ggarrange(key[[1]], key[[2]], key[[3]], 
            key[[4]], key[[5]], key[[6]],
            key[[7]], key[[8]], key[[9]], 
            key[[10]],key[[11]], 
            ncol = 3, nrow = 4),
  top = text_grob("", color = "black", 
                  face = "bold", size = 18)
)

ggsave("images/keywords.png", width = 27, height = 20, 
       units = "in", dpi = 200)


# Figure 15. The Estimated Positions of Each Major Historical Incident, 1966-1967
#            (Appendix 6.)
#===============================================================================
colors <- c("Rebel" = "#972D15", "Conservative" = "#81A88D")
incident_full <- ggplot(individual_idea_point, 
                        aes(x= x, y=fact_eng, color=fact_eng, group = fact_eng,fill = fact_eng)) +
  geom_violin(alpha= 0.6)  +
  facet_grid(rows = vars(incidents)) +
  geom_jitter(shape=16,  size = 1,
              position=position_jitter(seed = 42) ) +
  xlim(-1,1) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  #theme(legend.position = c(0.9, 0.9)) +
  scale_fill_manual(name = "The Red Gaurd Factions", 
                    values = c("#81A88D", "#972D15"), 
                    labels =c("The Conservatives","The Rebels"))+
  scale_colour_manual(name = "The Red Gaurd Factions", 
                      values = c("#81A88D", "#972D15"), 
                      labels = c("The Conservatives","The Rebels" )) +
  scale_x_continuous(breaks = function(x) to_integer(x, n = 10)) +
  theme_cowplot(12) +
  xlab("Estimated Ideal Points for Student Participants") +
  ylab(NULL) +
  theme(legend.position = "none")  +
  xlim(-2,2) +
  theme(strip.text.y.right = element_text(angle = 0))

ggsave("images/incident_full.png", width = 10, height = 6, 
       units = "in", 
       dpi = 200)


#====================================END========================================

})

cat(" ====================\n",
    "=",
    "Task 05 Is Done!", "=", "\n",
    "====================",
    "\n Core used :",  detectCores(), 
    "\n Time spent \n", 
    names(timer_task05[1]), ":",   timer_task05[[1]], "\n",
    names(timer_task05[2]), " :",  timer_task05[[2]], "\n",
    names(timer_task05[3]), "  :", timer_task05[[3]], "\n",
    "====================\n")

