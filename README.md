## redguards: Factionalism and the Red Guards under Mao's China: Ideal Point Estimation Using Text Data <img src="https://avatars3.githubusercontent.com/u/77121644?s=400&amp;u=49ca6038b83b629a86d391bb2e4d19f8995918a5&amp;v=4" width="130" height="145" align="right"/> <br /> 

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R](https://github.com/davidycliao/redguards/actions/workflows/r.yml/badge.svg)](https://github.com/davidycliao/redguards/actions/workflows/r.yml)
[![codecov](https://codecov.io/gh/davidycliao/redguards/branch/master/graph/badge.svg?token=9EWD4E1NCB)](https://codecov.io/gh/davidycliao/redguards)



###  Abstract  


This article estimates student participants' ideal points  during the Cultural Revolution through analyzing expressed political views in propaganda publication. In this paper, we design a new strain of text scaling method as we called Slogan-based Wordfish (Sworfish) that takes advantage of the facilitation of the TextRank algorithm to extract the most representative major noun phrases, and estimates Bayesian Generalized Wordfish with those extracted text variables. Our findings point to evidence of re(de)alignments within the factions and demonstrate how the students from different areas follow Mao Zedong and Xiaohongshu (Little Red Book) then fall into a fratricidal conflict that divided families, the classes and the society.
how student participants were dynamically divided into multiple factions that  fought for controls the movement across times. The results estimated by our estimation approach are shown to be consistent with the important works of factionalism emerging in the Cultural Revolution.

**Keywords**：*Text as Data, Textrank, Keyword Extraction,  the Cultural Revolution*  <br>
**Documents**:  [`slides`](https://raw.githack.com/davidycliao/redguards/master/slides/slides.pdf)



<br />

### Replication  


This is a designed package for replicating the estimates and findings in the article of [**Factionalism and the Red Guards under Mao's China: Ideal Point Estimation Using Text Data**](https://raw.githack.com/davidycliao/redguards/master/slides/slides.pdf). In this paper, we design a new strain of text scaling method as we called `SWORDFISH`  (Slogan-based Features Wordfish) that takes advantage of the facilitation of the TextRank algorithm to extract the most representative keywords (such as noun collocation phrases) and scale those extracted text variables with Bayesian IRT Generalized Wordfish Model implanted by [Imai, Lo, and Olmsted (2016)](https://imai.fas.harvard.edu/research/files/fastideal.pdf) based on the  Slapin and Proksch’s “Wordfish”(2008).

<p align="center">
  <img width="640" height="410" src="https://raw.githack.com/davidycliao/redguards/master/images/network_example.png" >
</p>


The Red Guard documents analyzed in the paper are archived in [The Databases for The History of Contemporary Chinese Political Movements](http://ccrd.usc.cuhk.edu.hk/Default.aspx?msg=%25u6ca1%25u6709%25u8ba2%25u9605%25uff0c%25u6b22%25u8fce%25u8ba2%25u9605%25uff01) (香港中文大學中國當代運動史數據庫) by The Chinese University of Hong Kong. Please note that replicating the analyses initially requires the access to the original corpus of the textual data. We, as authors and data users, do not fully have the copyright of the sources analyzed in the paper. To comply with the terms of service, we cannot share the textual files publicly. However, we are providing pre-processed textual files parsed on CoNLL-U format and document-term-matrix to replicate the analyses of the last stage. The pre-processed textual materials can be found at [data](https://github.com/davidycliao/redguards/tree/master/data). 

<p align="center">
  <img width="640" height="440" src="https://raw.githack.com/davidycliao/redguards/master/images/ideal_point.png" >
</p>



The source code in `replication-code` for replicating the estimates for this paper includes four parts for replication for all tables and figures that appear in both the main paper and the online supplemental materials: 

- __01.tokenization-in-udpipe.R__ (tokenization and part-of-speech tagging on Universal Dependencies via pre-trained model)
- __02.keywords-extraction.R__ second, (keyword extractions using TextRank)
- __03.pooled-ideal-point-estimates.R__ (textual documents merged by individual participants and estimated by Wordfish scaling method)
- __04.incident-ideal-points-estimates.R__ (textual documents estimated by Wordfish scaling method through time)
- __05.visualization.R__  (data visualization and findings)


<p align="center">
  <img width="640" height="500" src="https://raw.githack.com/davidycliao/redguards/master/images/network_plot.png" >
</p>



Replicating the comparable estimates for this paper is easy. Simply follow the description of [the reference](), folk or download this repo, and run `run_replication()` in the Rstudio console. The results and figures will automatically generated by the source codes and stored in `replication-figures`. 



<p align="center">
  <img width="640" height="480" src="https://raw.githack.com/davidycliao/redguards/master/images/incident_full.png" >
</p>



<br />


## Getting started

Install the release version of [_R_](https://cran.r-project.org/mirrors.html) (preferably version 3.6 or above),  [RStudio](https://rstudio.com/products/rstudio/download/#download) and  `usethis` and `devtools` from CRAN with `install.packages(c("usethis", "devtools")) `.

```
install.packages(c("usethis", "devtools"))
library(usethis)
library(devtools)
```

Download the `redguards` repository from GitHub with `use_course` and tick Yes or Definitely, automatically bringing you to the redguards project.

```
usethis::use_course(create_download_url("https://github.com/davidycliao/redguards"))
```

Then, build the project package by `install()` and load it. 
```
devtools::install()
library(redguards)
```


Last, start replication with `run_replication()`. 
```
run_replication()
```

Please note that replicating the figures requires installing `STHeiti font` in local computer beforehand to present Chinese characters.


## Cite
```
@misc{redguards,
    howpublished = {\url{https://github.com/davidycliao/redguards}},
    title = {Factionalism and the Red Guards under Mao's China: Ideal Point Estimation Using Text Data },
    author = {David Yen-Chieh Liao and Yi-Nung Tsai and Daniel Tene and Dechun Zhang},
    publisher = {GitHub},
    year = {2021}
}
```



