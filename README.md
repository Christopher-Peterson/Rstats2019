# Rstats Peer-led Seminar, Fall 2019
This is the repository for the peer-led R seminar, hosted by UT-Austin's [Center for Computational Biology and Bioinformatics](http://ccbb.biosci.utexas.edu/).  The goal for this class is to provide practical data science programmatic experience using R and RStudio. 

The class will meet on Thursdays from 2-3 pm in PAT 617.

# Schedule

- **09/26**: General Introduction and Basic Data Visualization with ggplot2
- **10/03**: Data Manipulation with dplyr
- **10/10**: Tidying and reshaping data with tidyr
- **10/17**: Exploratory Data Analysis
- **10/24**: Function Writing
- **10/31**: Lists and Automation with purrr
- **11/07**: Advanced ggplot2 and cowplot
- **11/14**: TBD
- **11/21**: TBD
- **11/28**: Thanksgiving
- **12/05**: TBD

# Installation and Setup Instructions

We will be using the recent version of [R (v. 3.6.1)](https://cran.r-project.org/); older versions may not be able to run some of the code and packages we're using.  Please download and install it on your computer.  Most of our work will be done in [RStudio](https://https://www.rstudio.com/products/rstudio/download/#download), which will also need to be installed.  Please make sure that your version of RStudio is at least 1.2.

Once R is setup, please install the latest version of the `tidyverse` (`install.packages("tidyverse")`).  Starting with the third session we'll be new functions introduced in version 1.0.0 `tidyr`, which was released September 13; you may need to install it separately to ensure you have the most up-to-date version (`install.packages("tidyr")`).  We also require the `cowplot` package.

## Course materials

Course materials are being managed by [Git](https://git-scm.com/) and [hosted on Github](https://github.com/Christopher-Peterson/Rstats2018).  You can either download the files directly (click the "Clone or download" button in the repository, then "Download ZIP"), or through RStudio (the recommended option).  You'll need to have Git installed and configured for use with RStudio [(instructions)](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN).  Once you do that, follow the instructions on that page for creating a new project from a remote repository.  

# Textbook

For the first part of the class, we will be following along with [R for Data Science](http://r4ds.had.co.nz/index.html), a free online textbook.  This book will provide additional examples and cover some areas in more depth than we can get to in person.

# Additional resources

Claus Wilke offers a class called SDS 348: Computational Biology and Bioinformatics that teaches R and Python. He makes it freely available online (https://wilkelab.org/classes/SDS348_spring_2019.html). Work through the lectures and assignments for additional practice.

RStudio produces cheatsheets for things such as data import, transformation, and visualization. Great for quick reference while you're coding (https://rstudio.com/resources/cheatsheets/).

