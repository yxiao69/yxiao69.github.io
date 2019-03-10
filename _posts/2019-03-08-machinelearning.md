---
title: "Random Forest Project"
date: 2019-03-08
tags: [Create new R package on Weighted Random Forest]
header:
  image: "/images/machine/percept.jpg"
excerpt: "Create new R package on Weighted Random Forest"
mathjax: "true"
---

## Design weighted random forest
Manipulate random forest at tree level using importance score and
multi-match info for each feature

For each tree  $$j=1,…,ntree$$,compute weight as $$w_j$$

For each subject $$i=1,…,N$$,compute prediction as $$v_i$$  based on OBB prediction  $$v_{ij}$$  

$$v_i=\frac{\sum_{j=1}^{ntree} w_j*v_{ij}}{\sum_{j=1}^{ntree} w_j}$$

## Manipulate info at tree level
Extract sample prediction at each tree;

Get the feature usage info at each tree

![alt](/images/machine/p2.jpg)
![alt](/images/machine/p3.jpg)

## Generate simulation
Generate dataset with features and predictors, predictors are used to create y. Pathway are small datasets comprised of randomly sample of features and predictors. we create artificial true pathway by fix number of predictors.

This simulate metabolomics data, in which features matching to multiple pathway owing to the LC-MS m/z matching bias.   

When lower the weight of tree with multi-match feature, hopefully, false pathway which have artificial high accuracy owing to multi-match will be suppressed. Thus , true pathway related to disease will be distinguished from the false.

![alt](/images/machine/p4.jpg)


##  Testify the effectiveness

Plot number of true predictor in tree vs. accuracy, adjusted by number of feature each tree

The plot show  the higher true predictors, the higher accuracy!!

![alt](/images/machine/tree_detailcheck.gif)*figure 1*
