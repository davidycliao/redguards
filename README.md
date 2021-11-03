## Factionalism and the Red Guards under Mao's China: Ideal Point Estimation Using Text Data <img src="https://avatars3.githubusercontent.com/u/77121644?s=400&amp;u=49ca6038b83b629a86d391bb2e4d19f8995918a5&amp;v=4" width="130" height="145" align="right"/> <br />


This is a designed package for replicating the estimates and findings in the article of [**Factionalism and the Red Guards under Mao's China: Ideal Point Estimation Using Text Data**](https://raw.githack.com/davidycliao/redgaurds/master/paper/epsa.pdf). In this paper, we design a new strain of text scaling method called (SWORDFISH, Slogan Wordfish) that takes advantage of the facilitation of the TextRank algorithm to extract the most representative keywords (as known as noun collocation phrases) and scale those extracted text variable via  Wordfish developed by Slapin and Proksch (2008).

The Red Guard documents analyzed in the paper are archived in [The Databases for The History of Contemporary Chinese Political Movements](http://ccrd.usc.cuhk.edu.hk/Default.aspx?msg=%25u6ca1%25u6709%25u8ba2%25u9605%25uff0c%25u6b22%25u8fce%25u8ba2%25u9605%25uff01) (香港中文大學中國當代運動史數據庫) by The Chinese University of Hong Kong. We, as authors, do not fully have the copyright of the sources analyzed in the paper. Please note that replicating the analyses requires access to the original corpus of the textual data. We cannot share publicly to comply with the terms of service regulated by The Chinese University of Hong Kong. We encourage you to purchase original textual data from them if interested. However, we are providing preprocessed textual files parsed on CoNLL-U format and document-term-matrix to replicate the analyses of the last stage in this paper. The preprocessed textual materials can be found [here]().

The source code for replicating the estimates for this paper includes three stages: i) tokenization and part-of-speech tagging on Universal Dependencies; ii) keyword extractions; and last iii) scaling the text feature using Wordfish. Replicating the comparable estimates for this paper is easy. Simply run the command below if **redguard** package is installed. Please see the [replication reference]().


```
run_replication()
```

#### *Please note that replicating the figures requires installing STHeiti font in the computer beforehand to present Chinese characters.

## Major Packages :package:

You will need the latest installation of [`R`](https://cran.r-project.org/mirrors.html) (preferably version 4.0 or 3.6 above) and [`RStudio`](https://rstudio.com/products/rstudio/download/#download). Some of the analysis might take lots of rams. So it would be better if replicating the results on HPC.

-   [quanteda: Quantitative Analysis of Textual Data ( quanteda_3.0.0 )](https://quanteda.io) [`JOSS`](https://joss.theoj.org/papers/10.21105/joss.00774) [`Doc`](https://quanteda.io/reference/) [`CRAN`](https://cran.r-project.org/web/packages/quanteda/index.html)

-   [austin: A package for doing things with words ( austin_0.2 )](https://conjugateprior.github.io/austin/) [`GitHub`](https://github.com/conjugateprior/austin)

-   [emIRT: EM Algorithms for Estimating Item Response Theory Models ( emIRT_0.0.11 )](https://github.com/kosukeimai/emIRT) [`Doc`](https://cran.r-project.org/web/packages/emIRT/emIRT.pdf) [`CRAN`](https://cran.r-project.org/web/packages/emIRT/index.html)

-   [udpipe: Tokenization, Parts of Speech Tagging, Lemmatization and Dependency Parsing with the 'UDPipe' 'NLP' Toolkit ( udpipe_0.8.6 )](https://bnosac.github.io/udpipe/en/) [`Doc`](https://bnosac.github.io/udpipe/docs/doc1.html) [`CRAN`](https://cran.r-project.org/web/packages/udpipe/index.html)

-   [Universal Dependencies 2.5 Models for UDPipe ( chinese-gsdsimp-ud-2.5-191206 )](https://lindat.mff.cuni.cz/repository/xmlui/handle/11234/1-3131)

-   [textrank: Summarize Text by Ranking Sentences and Finding Keywords ( textrank_0.3.1 )](https://github.com/bnosac/textrank) [`Doc`](https://cran.r-project.org/web/packages/textrank/vignettes/textrank.html) [`CRAN`](https://cran.r-project.org/web/packages/textrank/index.html) [`Doc`](https://mlr3book.mlr-org.com/)

-   [tmcn: A Text Mining Toolkit for Chinese ( tmcn_0.2-13 )](https://cran.r-project.org/web/packages/tmcn/index.html) [`Doc`](https://cran.r-project.org/web/packages/tmcn/tmcn.pdf)


## Acknowledge

-   The authors gratefully acknowledge [Joint Library of Humanities and Social Sciences, Academia Sinica in Taiwan](https://hslib.sinica.edu.tw/eng/frontpage) (台灣中央研究院人文社會科學聯合圖書館) provided us with access to the database and the use of the High Performance Computing Facility (CERES) and its associated support services at [The University of Essex](https://www.essex.ac.uk/student/it-services/high-performance-computing-(hpc)) in the completion of this work.

## Cite:

For citing this work, you can refer to the present GitHub project. For example, with BibTeX:
```
@misc{redgaurds,
      howpublished = {\url{https://github.com/davidycliao/redgaurds}},
      title = {Factionalism and the Red Guards under Mao’s China: Ideal Point Estimation Using Text Data},
      author = {David Yen-Chieh Liao, Yi-Nung Tsai, and Dechun Zhang},
      publisher = {GitHub},
      year = {2021}
}
```