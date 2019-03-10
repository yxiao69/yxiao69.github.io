---
title: "Random Forest Project"
date: 2019-03-08
tags: [Create new R package on Weighted Random Forest]
header:
  image: "/images/machine/percept.jpg"
excerpt: "Create new R package on Weighted Random Forest"
mathjax: "true"
---

# Design weighted random forest
Manipulate random forest at tree level using importance score and
multi-match info for each feature

For each tree  $j=1,…,ntree$,compute weight as $w_j$

For each subject $i=1,…,N$,compute prediction as $v_i$  based on OBB prediction  $v_{ij}$  

$$v_i=\frac{\sum_{j=1}^{ntree} w_j*v_{ij}}{\sum_{j=1}^{ntree} w_j}$$

# Manipulate info at tree level
Extract sample prediction at each tree;

Get the feature usage (appears in how many nodes) info at each tree

![alt]({{ site.url }}{{ site.baseurl }}/images/machine/p2.jpg)
![alt]({{ site.url }}{{ site.baseurl }}/images/machine/p3.jpg)
