# Code used for "Modernism's Limits: Patterns of Exclusion in Scholarly
# Reading," Modern Language Association, Chicago, January 10, 2014.
#
# You are free to use and modify this code. Like this code, the analysis
# in the accompanying slides is my intellectual work; if you use any
# part of this work, please attribute it to me.
#
# Copyright (c) 2014 Andrew Goldstone


options(java.parameters="-Xmx2g")

library(reshape2)
library(plyr)
library(stringr)
library(tm)
library(ggplot2)
library(ineq)
library(dfrtopics) # my own package, forthcoming on github

# extract the finer-grained metadata from the note field
# N.B. I have noticed at least one instance where there is a missing period
# at the end of a field, in acc no. 1998070296

N1_field <- function(nn,field) {
    matches <- regexec(str_c(field,': ([^.]*)\\.'),nn)
    sapply(regmatches(nn,matches),`[`,2)
}

Y1_Date <- function(y) {
    result <- str_split_fixed(y,"/",4)
    mm_wrong <- grep("^[0-9][0-9]$",result[,2],invert=T)
    dd_wrong <- grep("^[0-9][0-9]$",result[,3],invert=T)
    # normalize to first month/day where missing
    result[mm_wrong,2] <- "01"
    result[dd_wrong,3] <- "01"
    as.Date(str_c(result[,1],result[,2],result[,3],sep="-"))
}

shared_titles <- function (mla,jstor) {
    t_m <- mla$T1[mla$pubtype=="journal article"]
    t_j <- jstor$title
    clean <- function (s) {
        gsub("[^a-z ]","",
             gsub('<span class="smallcaps">',"",
                  gsub("</span>","",
                     gsub("</?[ib]>","",
                          tolower(str_trim(s))))))
    }
    t_m <- clean(t_m)
    t_j <- clean(t_j)
    t_both <- intersect(t_m,t_j)
    list(shared=sort(t_both),
         mla_only=setdiff(t_m,t_j),
         jstor_only=setdiff(t_j,t_m),
         jstor_docs=which(t_j %in% t_both)
         )
}


docs_instances <- function (files,stoplist_file,output_file) {
    counts <- read_dfr(files=files)
    counts <- remove_rare(counts,rank_threshold=10000)
    docs <- docs_frame(counts)
    inst <- make_instances(docs,stoplist_file)
    write_instances(inst,output_file)

    inst
}

model_docs <- function(instance_file,output_dir,n_topics,seed=424242L) {
    inst <- read_instances(instance_file)
    model <- train_model(inst,n_topics=n_topics,threads=2L,n_iters=250,
                         seed=seed)
    doc_topics <- doc_topics_frame(model,smoothed=F,normalized=F)
    keys <- weighted_keys_frame(model,n_top_words=50L,
                                smoothed=F,normalized=F)

    result <- list(doc_topics=doc_topics,
                   wkf=keys,trainer=model,seed=seed)

    output_model(model_result=result,
                 output_dir=output_dir,
                 save_instances=F,
                 save_scaled=T)

    result
}                 




# corpus of titles
title_corpus <- function(bib,stemming="possessive",
                         stoplist=stopwords("en")) {
    corpus <- Corpus(VectorSource(bib$T1))

    # date metadata
    meta(corpus,"date") <- bib$date

    # weighting metadata
    meta(corpus,"doc_weight") <- bib$weight

    # Porter stem
    if(stemming=="porter") {
        corpus <- tm_map(corpus,stemDocument)
    } else if(stemming=="possessive") {
        corpus <- tm_map(corpus,
                         function (x) {
                             gsub("'s\\b","",x)
                         })
    }

    # and clean
    corpus <- tm_map(corpus,removePunctuation)
    corpus <- tm_map(corpus,tolower)
    corpus <- tm_map(corpus,removeWords,stoplist)

    corpus
}

title_term_year_matrix <- function(corpus,tdm=TermDocumentMatrix(corpus),
                                   interval="years") {
    years <- cut.Date(meta(corpus,"date")$date,interval,ordered=T)
    years <- droplevels(years)

    # indicator-matrix version of years
    Y <- Matrix(0,nrow=length(years),ncol=nlevels(years))
    Y[cbind(seq_along(years),years)] <- 1
    
    # simple_triplet_matrices are easy to manually coerce to sparseMatrix
    m_s <- sparseMatrix(i=tdm$i,j=tdm$j,x=tdm$v)

    # apply document weights
    wts <- Diagonal(n=ncol(m_s),x=meta(corpus,"doc_weight")$doc_weight)
    result <- m_s %*% wts

    result <- result %*% Y
    colnames(result) <- levels(years)
    rownames(result) <- rownames(tdm)
    result
}


# KW field to list of subjects
KW_subjects <- function(KW) {
    unique(sort(unlist(str_split(KW,';;'))))
}

explore_setup <- function() {
    explore <- list()
    # read in list of non-authors
    explore$non_authors <- readLines("non_authors.txt")
    # strip comments
    explore$non_authors <- str_trim(gsub("#.*$","",explore$non_authors))

    rels <- c("about","after","and","application of( theories of)?",
              "as","at","between","by","compared to","contributions of",
              "discusses","during",
              "especially","for","from","in","includes",
              "influence( of| on)","of","on","relationship to( the)?",
              "role( in| of)","sources in","study example",
              "the","theories of",
              "theory of theories of", # Ortega
              "to( and from)?",
              "treatment( in| of (the|decadence)?)?", # decadence: Swinburne
              "use( in| of)","with")
    explore$rel_pat <- str_c(rels,collapse="|")
    explore$rel_pat <- str_c("^(",explore$rel_pat,") ")
    explore
}

# keep only author names from a list of subject headings.
# For a listing, set non_author_value="", drop the empty strings, and apply 
# unique(sort())
subject_authors <- function(subjects,explore,non_author_value=NULL) {

    # find authors by looking for subjects that have a (YYYY- in them
    # not perfect, because some books have a date range, like the Recherche,
    # so we'll need a list of non-authors (explore$non_authors) below.

    indices <- grep('(.*) \\(\\d\\d\\d\\d\\??-',subjects,perl=T)
    authors <- subjects[indices]

    # Strip relation terms and subheading, twice. This is a childish way
    # to deal with the grammar of subject relations, which can nest to
    # arbitrary depth; but in practice two passes suffices.

    authors <- strip_subject_relation(authors,explore$rel_pat)
    authors <- strip_subject_relation(authors,explore$rel_pat)

    # MLAIB uses two U+00A0 NO-BREAK SPACEs in place of the death date
    # for living authors
    authors <- gsub('\\([-0-9? \\x{a0}/]*\\)',"",authors,perl=T)
    authors <- str_trim(authors)

    # A number of volumes called "The .*" make it through the filter
    indices <- indices[grep('^The .*',authors,invert=T)]
    authors <- grep('^The .*',authors,invert=T,value=T)

    # similarly, drop non-authors
    indices <- indices[!(authors %in% explore$non_authors)]
    authors <- authors[!(authors %in% explore$non_authors)]

    subjects[indices] <- authors

    # if a replacement value for non-authors is supplied, use it to wipe out
    # all the other subjects in subjects
    if(!is.null(non_author_value)) {
        subjects[-indices] <- non_author_value
    }
    subjects
}

subject_authors_last <- function(a) {
    tolower(gsub(",.*$","",a))
}

authors_in_titles_series <- function(
        bib,term_year,explore,
        authors=subject_authors_last(subject_authors(KW_subjects(bib$KW),
                                                     explore))) {

    authors <- sort(unique(authors))
    term_year <- term_year[rownames(term_year) %in% authors,]
    message(nrow(term_year)," last names matched in titles")
    series <- melt(as.matrix(term_year),
                   varnames=c("term","date"),value.name="count")
    series <- ddply(series,.(date),transform,freq=count/sum(count))
    series$date <- as.Date(series$date)

    series
}

authors_in_titles_plot <- function(series,geom="smooth") {
    series <- series[series$date >= as.Date("1970-01-01"),]
    p <- ggplot(series,
                aes(date,freq,group=term,color=term,alpha=0.1))
    if(geom=="smooth") {
        p <- p + geom_smooth(method="loess",se=F)
    } else if(geom=="line") {
        p <- p + geom_line()
    }
    clrs <- rep("black",length(unique(series$term)))
    names(clrs) <- unique(series$term)
    clrs["faulkner"] <- "red"
    clrs["joyce"] <- "green"
    clrs["eliot"] <- "blue"
    p <- p + scale_color_manual(values=clrs)
    p <- p + theme(legend.position="none")
    p
}

series_ranks <- function(series) {
    series$date <- as.character(series$date)
    ddply(series,.(date),transform,rank=length(freq) - rank(freq) + 1)

    # Then ask for the top n from the result:
    # result[result$rank <= n,]
}

subject_series <- function(bib,explore,interval="years") {
    bib$date <- cut.Date(bib$date,interval,ordered=T)
    kws <- ddply(bib,.(id),
                 function (d) {
                     data.frame(
        date=d$date,
        term=strip_subject_relation(unlist(str_split(d$KW,";;")),
                                    explore$rel_pat),
        weight=d$weight)
                 },
                 .progress="text")

    result <- ddply(kws,.(date,term),summarize,
                    count=sum(weight))
    result$date <- as.Date(result$date)
    ddply(result,.(date),transform,freq=count/sum(count))
}

subject_author_series <- function(series,explore) {
    series$term <- subject_authors(as.character(series$term),explore,"")
    series <- series[series$term != "",]
    all_terms <- unique(series$term)
    series <- ddply(series,.(date,term),summarize,
                    count=sum(count),freq=sum(freq),
                    .progress="text")
    series <- ddply(series,.(date),
                    function (d) {
                        fill <- data.frame(date=d$date[1],
                                           term=setdiff(all_terms,d$term),
                                           count=0,
                                           freq=0)
                        rbind(d,fill)
                    },
                    .progress="text")

    series
}


strip_subject_relation <- function(s,pat) {
    result <- gsub(pat,"",s,perl=T)

    # also strip subheading
    result <- gsub(":.*$","",result)
    result
}

series_ineq <- function(series) {
    ddply(series,.(date),summarize,g=Gini(count),var=var(freq))
}

series_delta <- function(series) {
    series <- ddply(series,.(term),transform,date=sort(date),
                    .progress="text")
    ddply(series,.(term),transform,
          change=c(0,count[-1] - count[-length(count)]),
          .progress="text")
}



# main script
# -----------

explore_main <- function (working_dir=".",model_dir="jstor_model") {
stopifnot(file.exists(working_dir))
old_wd <- getwd()
setwd(working_dir)
stopifnot(file.exists(model_dir))

# we'll return this eventually
b <- list()

# set up global values
b$explore <- explore_setup()

# regenerate aggregate file if needed
system("make ebsco.csv")

# import
b$mlaib <- read.table("ebsco.csv",
                  header=T,
                  sep=",",
                  quote='"',
                  stringsAsFactors=F,
                  comment.char="",
                  fill=F,
                  blank.lines.skip=F,
                  colClasses="character"
                  )

# extract out pubtype and accession nums
b$mlaib$pubtype <- N1_field(b$mlaib$N1,"Publication Type")
stopifnot(all(b$mlaib$pubtype != ""))
b$mlaib$id <- N1_field(b$mlaib$N1,"Accession Number")
stopifnot(all(b$mlaib$id != ""))

# some but not all items have month or other finer-grained time data;
# almost everything has a year
b$mlaib$date <- Y1_Date(b$mlaib$Y1)

# manual correction of missing dates
# 1. Boehmer, "Dreams, Cycles, and Advancing Forms of Chaos"
b$mlaib$date[b$mlaib$id == "1998070296"] <- as.Date("1998-01-01")

# 2. discard websites (4 undated)
message("Discarding ",sum(b$mlaib$pubtype=="website")," website entries")
b$mlaib <- b$mlaib[b$mlaib$pubtype != "website",]

stopifnot(nrow(b$mlaib) == 8995)
 
stopifnot(all(!is.na(b$mlaib$date)))

b$total_entries <- nrow(b$mlaib)

b$jstor <- read_metadata("jstor/citations.CSV")
shared <- shared_titles(b$mlaib,b$jstor)
shared_meta <- b$jstor[shared$jstor_docs,]
shared_titles_j <- sort(shared_meta$title)
shared_duped_j <- unique(shared_titles_j[duplicated(shared_titles_j)])
stopifnot(all(shared_duped_j==c(
    "Modernism\t",
    "Technology and Modernism\t",
    "What is a Boojum? Nonsense and Modernism\t",
    "What Was Modernism?\t")))
# manually checking these:
#
# 1. Of 11 items called just "Modernism" in MLA and 5 in JSTOR, only two
# are genuine overlaps: one by Pearlman in ConL (10.2307/1208629) and
# one by Bristol in Slavic & East European J (10.2307/307981)
#
# 2. Technology and Modernism: two distinct book reviews in jstor, of
# which only the 1988 Contemporary Lit review article (10.2307/4335590)
# is in MLA (the other (10.2307/1208529) is skipped presumably because
# it's a book review and not an article)
#
# 3. Boojum: legitimately a reprint of Holquist's essay in YFS, appears twice 
# in both datasets
#
# 4. Three "What Was Modernism" entries in MLA, two in JSTOR. MLA
# includes a book article reprint of the Levin as well as the two
# journal articles in JSTOR. 
#
# Thus we need only to remove:
#
# 1. The errant "Modernism" items
shared_meta <- shared_meta[shared_meta$title != "Modernism\t" |
                           shared_meta$id %in% c("10.2307/1208629\t",
                                                 "10.2307/307981\t"),]
# 2. The "Technology and Modernism" item missing in MLA
shared_meta <- shared_meta[shared_meta$id != '10.2307/1208529\t',]

message(nrow(shared_meta)," jstor items are also in MLA results")

# write out tm package stoplist
if(!file.exists("tm_stoplist.txt")) {
    writeLines(stopwords("en"),'tm_stoplist.txt')
}

shared_files <- file.path("jstor","wordcounts",id_filename(shared_meta$id))
stopifnot(all(file.exists(shared_files)))

b$shared <- shared_meta

stoplist_file <- "modeling_stoplist.txt"
inst_file <- "jstor_model/jstor.mallet"
if(!file.exists(inst_file)) {
    docs_instances(files=shared_files,
                   stoplist_file=stoplist_file,
                   output_file=inst_file)
} else {
    message(inst_file," exists; skipping mallet import")
}
if(!file.exists(file.path(model_dir,"doc_topics.csv"))) {
    model_docs(inst_file,model_dir,n_topics=16)
} else {
    message("Already found topic model files in ",model_dir)
    message("Skipping mallet run")
}

b$m <- list()
wkf_raw <- read.csv(file.path(model_dir,"keys.csv"),as.is=T)
params <- read.csv(file.path(model_dir,"params.csv"))
tw_raw <- as.matrix(read.table(file.path(model_dir,"topic_words.csv"),
                               header=F,sep=",",row.names=NULL,
                               colClasses=integer()))
b$m$wkf_blei <- tw_wkf(topic_word_scores(tw_raw,params$b),
                     readLines(file.path(model_dir,"vocab.txt")),
                     unique(wkf_raw$alpha))
b$m$doc_topics <- read.csv(file.path(model_dir,"doc_topics.csv"),as.is=T)


# ----------------
# For the rest, let's drop pre-1982 stuff
b$mlaib <- b$mlaib[b$mlaib$date >= as.Date("1982-01-01"),]
# and 2013, which has less than half the number of entries than 2012 right now
b$mlaib <- b$mlaib[b$mlaib$date < as.Date("2013-01-01"),]

# downweighting of book chapters: isolate book chapters using "CHAP"
# type and match on "T2" and "Y1" which holds the book title. Not a
# perfect guarantee that we won't accidentally conflate two collections
# with identical titles, but manual check of CHAPs with matching T2s
# and Y1s suggests it's mostly okay, except:

# 1. This check catches a book chapter by Melba Cuddy-Keane (acc. no.
# 2006533095) which is dated 2006 but seems to be in the same book as
# others with the same T2 dated 2008. Let's fix that by hand:
b$mlaib$Y1[b$mlaib$id=="2006533095"] <- "2008"

# 2. It also reveals a duplicate entry for "Madwomen on the Riviera: The 
# Fitzgeralds, Hemingway, and the Matter of Modernism," so let's remove the 
# later one, acc. no. 1999056705:
b$mlaib <- b$mlaib[b$mlaib$id != "1999056705", ]

# 3. We will leave untouched the distinction between two different editions of 
# the Cambridge Companion to Joyce. (cf. acc. nos. 1990021854 and 2004532092).

message("Calculating item weights")
b$mlaib <- ddply(b$mlaib,c("T2","Y1"),
             function(d) {
                 d$weight <- ifelse(d$TY=="CHAP",1 / length(d$T2),1)
                 d
             },
             .progress="text")

message("After filtering and hand-correction, ",
        nrow(b$mlaib)," items with total weight ",
        round(sum(b$mlaib$weight),2)," remain.")

message("Generating title corpus")
b$tc <- title_corpus(b$mlaib,stemming="possessive",
                   stoplist=readLines(stoplist_file))



message("Generating subject time series")
b$subjects <- subject_series(b$mlaib,b$explore,interval="2 years")
message("Deriving subject-author series")
b$subjects_authors <- subject_author_series(b$subjects,b$explore)
# add rank column
message("Adding author ranks per timeslice")
b$subjects_authors <- series_ranks(b$subjects_authors)

message("Tallying total hits for authors as subjects")
b$auth_totals <- ddply(b$subjects_authors,.(term),summarize,count=sum(count),
                       .progress="text")

message("Building title term-timeslice matrix")
b$tc_tym <- title_term_year_matrix(b$tc,interval="2 years")
b$last_names <- subject_authors_last(unique(sort(b$subjects_authors$term)))
message("Deriving authors-in-titles time series")
b$titles_authors <- authors_in_titles_series(b$mlaib,b$tc_tym,b$explore,
                                             b$last_names)

message("Finishing up...")

setwd(old_wd)
b
} # end of explore_main
