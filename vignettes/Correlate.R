---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = F, fig.width = 5, fig.height = 10)
library(imager)
suppressPackageStartupMessages(library(tidyverse))
library(ShoeprintCleanR)
library(tools)
```

## Heuristic Explore
```{r readin}
shoe_paths <- list.files("inst", pattern = ".tif", full.names = T)
shoes <- lapply(shoe_paths, load.image) %>% as.imlist()
plot(shoes)
```

```{r}
edgekern <- matrix(-2, nrow = 3, ncol = 3)
edgekern[2,2] <- 9
# edgekern[1,1] <- edgekern[3,3] <- edgekern[1,3] <- edgekern[3,1] <- -1

newshoes <- shoes %>% map(~ .x %>% correlate(as.cimg(edgekern)) %>% renorm(min = 0, max = 255) %>% {255 - .} %>% quantize_colors(4) )

plot(newshoes)
```
