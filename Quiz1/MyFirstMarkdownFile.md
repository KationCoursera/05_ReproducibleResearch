---
title: "My first R Markdown file"
author: "Jure Bordon"
date: "Sunday, February 15, 2015"
output: html_document
---

This is my first R markdown file.

Here, we're going to load some data.

```{r}
library(datasets)
data(airquality)
summary(airquality)
```

Let's first make a pair of plot of the data.

```{r}
pairs(airquality)
```


