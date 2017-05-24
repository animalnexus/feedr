---
title: "FAQ"
output: html_document
---

## FAQ

### 1. What is the difference between `feedr` and animal__nexus__?
[feedr](http://github.com/steffilazerte/feedr) is an [R](https://cran.r-project.org/) package for loading, cleaning, transforming and visualizing data collected on individual movements to and from static stations, especially RFID data.

animal__nexus__ is a webapp that allows users to apply functions from `feedr` to their data without have to use or even install R. If you have `feedr` installed on your computer, you can run this webapp locally from R using the function `animalnexus()`. However you will not have database access.

### 2. How do I find out how to use `feedr`?
Check out our [tutorials](https://animalnexus.github.io/feedr/) for scripting with the `feedr` package

### 3. I have a function I wrote in R that would be useful, how can I get involved?
Contact [Stefanie LaZerte](mailto:slazerte@tru.ca), or make a pull request on [github](http://github.com/steffilazerte/feedr) we'd love to include your function!

### 4. I don't know anything about R, can I still get involved?
Absolutely, there are many ways to get involved:

- [let us know](mailto:slazerte@tru.ca) if you find any bugs or errors
- [let us know](mailto:slazerte@tru.ca) what functionality would be useful
- [share data](mailto:dhill@tru.ca) with our database
- use the feedr R package or use the animal__nexus__ webapp and cite us in your publications!

### 5. Is my data secure if I share it?
For a given value of secure, yes. If you choose to share your data for visualizations only, you data won't be available for download and users won't be able to access the raw data.

However, our system is not the kind of secure system you would expect from banks or online stores. This means that if someone were to malishly attempt to grab your data from our site, they might succeed. We believe that this is unlikely, but if your data must be __absolutely__ secure, we don't recommend sharing it with us.

### 6. Is this a cloud hosting service for my data?
No. The purpose of the animal__nexus__ database is to share data for scientific or educational purposes. It is not a data backup service.
