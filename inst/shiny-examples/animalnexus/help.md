---
title: "Using animalnexus"
output: html_document
---

## Using animalnexus

You can use animalnexus to visualize and transform RFID data. 

The first thing you need to do is select a data source: You can either use data stored in the animal__nexus__ database or you can upload your own. After that you can go either to the Visualizations tab or the Transformations tab to either map or download transformations of your data. You can adjust the default settings by going to the Settings Tab.

If you get stuck, click on help buttons (<button class="btn btn-default action-button help shiny-bound-input" id="current-help_update" type="button">?</button>) to get some guidance on any particular stage.

### 1. Getting data

#### a) From the Database

#### b) Importing Data

### 2. Visualizations
[feedr](http://github.com/steffilazerte/feedr) is an [R](https://cran.r-project.org/) package for loading, cleaning, transforming and visualizing data collected on individual movements to and from static stations, especially RFID data.

animal__nexus__ is a webapp that allows users to apply functions from `feedr` to their data without have to use or even install R. If you have `feedr` installed on your computer, you can run this webapp locally from R using the function `animalnexus()`. However you will not have database access.
