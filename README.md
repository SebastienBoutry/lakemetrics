# lakemetrics
Morphologie des lacs / Méthode de Jensen
---
title: "Lake Metrics"
author: "Sébastien Boutry"
date: "`r Sys.Date()`"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{a_start}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---


```{r setup, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```

## Préambule

### Instalation de `{lakemetrics}`

La version du paquet `{lakemetrics}` peut se télécharger via le site Github pour cela on aura besoin du paquet `{remotes}`: 

```{r install, eval=FALSE}
remotes::install_github("SebastienBoutry/lakemetrics")
```
