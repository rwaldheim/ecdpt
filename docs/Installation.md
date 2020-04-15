---
layout: page
title: "Installation"
permalink: /installation/
theme: jekyll-theme-dinky
---

[UNDER CONSTRUCTION]

We're excited to see you want to start analyzing with us!

To summarize the the below procedure, you will require the following (if you do not already):
- [git](https://git-scm.com/)*
- a github account (https://github.com)*
- [R](https://www.r-project.org/)
- [RStudio](https://rstudio.com/)

Items marked with (*) are optional, but recommended

If you do not have any of the above, no worries! You can follow the step-by-step guides below. There are two installation methods: with and without git. The git method is recommended as it allows for easy updating of all future updates, but the other (.zip) method also works and both are outlined below. However, it is worth noting that if you would ever want to start contributing to this repository, the git method is then required.

## Installation with git (recommended)

This method used the distributed version control software (DVCS) and namesake of GitHub: git. If you would like to learn more about how git works outside of this quick tutorial, please see GitHub's amazing tutorials found [here](https://try.github.io/).

### Installing git

The first step is to install git. Some operating systems come with git pre-installed. To check if you already have git, open up your terminal (Mac/Linux) or Command Prompt (Windows) and type `git` and press the enter/return key. If you don't see a prompt saying something similar to `` `git` is an unrecognized command``, congratulations! You already have git installed on your machine. 

If you do not have git installed, install it [here](https://git-scm.com/downloads), making sure to download the correct version for your operating system. Follow all the default prompts during the installation.

### Making a GitHub Account

You do not need to create a GitHub account to simply access the code and upgrade it, however it is required if you wish to contribute to the repository. You can easily make one in the top right corner of this page (if you are not already signed in) or the GitHub [homepage](https:/github.com).

### Installing R

R is a programming language, much like Java or Python, but it has a much heavier leaning towards statistical computing. 
1. You can download the language compiler [here](https://cran.r-project.org/mirrors.html), making sure to choose the mirror closest to you for the fastest performance.
2. Select the version that matches your operating system.
3. Select "base" then “Download R [version number] for [operating system]”.
4. Wait for the installation to complete.

### Installing RStudio

The last step is to install RStudio. RStudio is a development platform meant to develop and edit the R scripts. It also included many handy features while editing and compiling, most notably for the purposes of this script is the [Shiny](https://shiny.rstudio.com/) package in which the user interface is generated. It can be installed [here](https://rstudio.com/products/rstudio/download/), choose the free license of "RStudio Desktop" and complete the installation with all default prompts.

## Installation without git (not recommended)

This method simply allows you to download the script and open it up in RStudio. If you do not have them already, install [R](#Installing-R) and [RStudio][#Installing-RStudio) per the above instructions. 

Next, navigate to the [repository homepage](https://github.com/2ryan09/arbinimport) on GitHub. Click on "Clone or Download" then "Download ZIP". This will download a compressed .zip file to your computer containing all the files in the repository.
