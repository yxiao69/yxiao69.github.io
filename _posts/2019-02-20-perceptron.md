---
title: "Data Wrangling Project"
date: 2019-02-20
tags: [data wrangling, data science, messy data]
header:
  image: "/images/hiconet.png"
excerpt: "Generate matrix and annotation data from Immport "
mathjax: "true"
---

## Immport Datasets Wrangling
[GitHub](https://github.com/yxiao69/immport/wiki)


**Hieracrchical Community Network**(hiconet) is software for integration of multiple data types collected from a common group of subjects. We aim to construct data structure that can be applied in hiconnet, which will serve as reference for user.

### Data structure
**Project**: a collection of data of one or more types.

For multiple data types, common samples/subjects are expected.This is the unit HiCoNet works on. HiCoNet integrates DataMatrices within a DataSet.A DataSet should have at least one Society of data


**Society**: one data type
at least one of each DataMatrix, FeatureAnnotation, ObservationAnnotation

**Data Matrix**: a data matrix of continuous values that represent a biological state or concentration, of the same data type.

This can include different time points or treatments.

This is the unit community detection is based on.

**Observation Annotation**: meta data on samples.

This may include TimePoints and Treatments, often in biosample table from ImmPort DB

**Feature Annotation**: meta data on features

**Key annotation variables**: time point and treatment.

**Graph**: a graph/network for relationships in the data (e.g. used in loom format, loompy.org).

**Community**: a group of features within a society that share a similar pattern.

### Process
1. Connect to ImmPort using ImmuneSpaceR

```r
names <- c("SDY80","SDY180","SDY212","SDY269","SDY312")
flist <- list()

for (ii in 1:length(names)) {
  tmp <- CreateConnection(names[ii])
  flist[[ii]] <- tmp$getDataset("fcs_analyzed_result")
}
```
2. Write out the input files

```r
mainDir <- "~/Downloads"
subDir <- out

dir.create(file.path(mainDir, subDir))
setwd(file.path(mainDir, subDir))

for (ii in 1:length(flist)) {
 # Write out the files

  outname <- paste(names[ii],key,".tsv",sep="_")

  if (nrow(flist[[ii]]) == 0) {
    str <- paste(outname,"does not have any rows to write",sep=" ")
    print(str)
  } else {
     write_tsv(flist[[ii]],path=outname)
  }
}
```

3. Generate data matrix

The col1 is columns we group by which differs at different datasets

the col2 is the column we wish to summarize, which differs too.

```r
setwd(file.path(mainDir, subDir))

files <- list.files(pattern=paste0("*",key,"*"))

for (ii in 1:length(files)) {
  fcs <- read_tsv(files[ii])
  ids <- sapply(strsplit(fcs$`participant_id`,"\\."),`[`,1)
  observation_ID <- gsub(" ","",paste(ids,format(fcs$`study_time_collected`,nsmall=1),sep="_"))

# All we need is the first 10 columns from the data frame

   fcs.mat <- cbind(observation_ID,fcs)
   fcs.mat$observation_ID <- as.character(fcs.mat$observation_ID)
   fcs.mat$`Participant ID` <- ids
   data.matrix <- fcs.mat %>%
     group_by(UQ(as.name(col1)),observation_ID) %>%#`population_definition_reported`
     summarize(med=median(UQ(as.name(col2)))) %>% #`population_cell_number`
     spread(observation_ID,med)

    fname <- paste(strsplit(files[ii],"_")[[1]][1],"fcs_data_matrix.tsv",sep="_")
    write_tsv(data.matrix,fname)
}
```

4. Generate the Annotation file

limit the number of columns we need here.

remove any duplicates since this is simply an annotation file.
```r

setwd(file.path(mainDir, subDir))
files <- list.files(pattern=paste0("*",key,"*"))
#
for (ii in 1:length(files)) {
  fcs <- read_tsv(files[ii])
  ids <- sapply(strsplit(fcs$`participant_id`,"\\."),`[`,1)
  observation_ID <- gsub(" ","",paste(ids,format(fcs$`study_time_collected`,nsmall=1),sep="_"))

   fcs.tmp <- cbind(observation_ID,fcs)
   fcs.tmp$observation_ID <- as.character(fcs.tmp$observation_ID)
   fcs.tmp$`Participant ID` <- ids

      fcs.2 <- fcs.tmp[,1:8]

# Are there duplicates ?
      sum(duplicated(fcs.2))

# Get rid of duplicated rows (except the first occurrence thereof)
    fcs.3 <- fcs.2[!duplicated(fcs.2),]

    fname <- paste(strsplit(files[ii],"_")[[1]][1],key,"annotation.tsv",sep="_")
    write_tsv(fcs.3,fname)
}
```
