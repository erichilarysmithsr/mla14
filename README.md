# Modernism's Limits: Patterns of Exclusion in Scholarly Reading
## Accompanying Code

This repository contains source code I used to generate the [slides](http://andrewgoldstone.com/mla2014/slides.pdf) for my January 10, 2014 presentation at the [Modern Language Association](http://andrewgoldstone.com/mla2014) in Chicago, "Modernism's Limits: Patterns of Exclusion in Scholarly Reading."

I make it available here mostly to document my work. The code is not runnable, because I have not included the data; I also used some under-development code I am not ready to make public. However, some bits and pieces here might be useful to others who wish to think about patterns in the MLA bibliography data, especially titles and subject headings. I worked from a set of MLAIB records exported to RIS format.


## mlaib2014/aggregate_ris.py

RIS is an awkward format to scan in R (cf. [Wikipedia on RIS](http://en.wikipedia.org/wiki/RIS_%28file_format%29)), so this python script takes a set of RIS files from the command line and exports them in a single CSV on standard output. I then read this CSV into R.

## mlaib2014/explore.R

The main analysis script. After having been burned a bunch of times by the temptation to just list a bunch of commands, I now write R scripts like python scripts, with `library` commands followed by function definitions at the top and a main function at the bottom which I invoke manually in the interpreter after `source`ing the file. So the flow of execution begins, conceptually, with `explore_main()`.

The main possibly-reusable parts here are:

1. Information about the structure of MLA bibliography RIS exports. Take a look at the first part of `explore_main` and the `Y1_date` and `N1_field` functions. The former parses RIS's date field. The latter uses the extra metadata in the `N1` ("note") field. In particular, MLAIB places a unique identifier, the accession number, in this field. It looks like the extra metadata is delimited by colons before the field name and periods at the end of the data, but unfortunately I have found both missing periods and periods and colons within field data. The note is thus more like (imperfect) markup than structured data (cf. Jason Thomale's ["Interpreting MARC: Where’s the Bibliographic Data?"](http://journal.code4lib.org/articles/3832)). Nonetheless, I extract both accession numbers and publication types (finer-grained than the RIS `TY` field).

2. Code for extracting author names from a list of subject headings. This is all just regular-expression games. Here I relied on the fact that almost all author names are followed by dates of birth and death. Unfortunately some titles of works also have a date range, and I resorted to listing these by hand (`mlaib2014/non_authors.txt`). A fancier approach would make use of the Semantic Web, but at this scale (2500-odd names) it wasn't worth it.

3. Code for collating a list of MLAIB entries with JSTOR Data for Research metadata and wordcount files (`shared_titles` function plus the next piece of `explore_main`). What I did here was to download wordcounts for all the JSTOR entries with "modernism" in the title, and then filter out any JSTOR items with titles that weren't in my MLAIB list. Naturally this does not cover all of the MLAIB entries in my "modernism" keyword query, not even all the articles. And again the matching is an imperfect process, because we are not yet in that utopia in which we could simply match DOI fields. Instead, I hand-checked all the duplicate titles, as described in the comments in the source. 

4. Ideas about topic-modeling in this context. I use my own `dfrtopics` R package, which I have not yet publicly released, to run mallet on the JSTOR data. It's basically just a thin wrapper around David Mimno's R [mallet](http://cran.r-project.org/web/packages/mallet/index.html) with some extra utility functions I've accumulated over time. I have found that using the most frequent words in each topic is a problem when you have selected your texts by a keyword-based search query: in this case, most of the topics have "modernism" in a distractingly prominent position. Here the weighting formula of [Blei and Lafferty](http://www.cs.princeton.edu/~blei/papers/BleiLafferty2009.pdf) is useful for improving the face validity of topics.

5. Weighting by publication type. In counting hits for names in the MLAIB data, I found that edited collections are sociologically interesting. Such collections play tricks with the keyword-searching system; we might even say that this is an intentional feature of the genre. *Laurence Sterne in Modernism and Postmodernism* (Rodopi, 1995) suddenly gives Sterne 15 hits as a major focal author of modernism in 1995. Yet to treat this as the equivalent of 15 separate articles or books about Sterne and modernism seems like an error, especially given the low prestige my professional habitus tells me such edited collections have (the edited collection as such; the formulaic quality of "Author X in Epoch Y"; the low symbolic capital of the Dutch publisher for English studies). The most rigorous way to deal with this would be to operationalize "impact," but absent citation counts, circulation data, or a reliable scale of publisher prestige, I simply decided to penalize all edited collections so that the total weight of all chapters from an edited collection was equal to the weight of a book. The code to do this is the next piece of `explore_main`, in a single `ddply` call I am rather proud of. I decided to weight journal articles and books equally. 

6. More code tricks: my ways of tallying up time series in R may again be useful for others. See `subject_series()` for a straightforward method using `ddply` (multiple passes; this may not be the most efficient way). But for the time series of words in titles, I take a different, hackier approach in `title_term_year_matrix()`. I use [tm](http://cran.r-project.org/web/packages/tm/) to tokenize and remove stop words from titles. Then I get the term-document matrix from `tm`. Now comes the trick: I factor the document dates using `cut.Date` and convert the factor into an indicator matrix (taking advantage of what happens when you coerce an ordered factor to numeric; h/t someone on stackoverflow). I turn the `simple_triplet_matrix` returned by `tm` into a `sparseMatrix` and multiply it by the TDM on the left to get the matrix of term counts by date. This is a pretty roundabout way to express "split the matrix by document years and sum," but it's faster and scales up well, and I've found it helpful to know how to go back and forth between factors and indicator matrices.

7. As for that title-word corpus, I decided term-frequency was in fact the quantity of interest, though I again applied the penalty to edited collections. tf-idf is *not* what you want when looking at which names get recurrently cited.

8. For my particular dataset, I chose two-year intervals rather than one-year intervals for the time bins. This choice is reflected at the end of `explore_main()` in the two uses of `interval="2 years"` as parameters to my time-series constructors. Using a single year as an interval produces noisier trend-lines but doesn't affect the trends I was looking at.

## slides.Rmd and Makefile

I wrote the slides in R markdown and used [knitr](http://yihui.name/knitr/),  which allowed me to mix the R code for generating tables and charts with the source for generating my slides. Most of the plotting and tabling is straightforward; using [ggplot](http://ggplot2.org/) is smoother for me than it used to be, after much practice. My "symbolic economy" plot, showing the cumulative hit-count distribution, might be of interest. R's `cumsum` function is handy and easy to forget about.

On the hacking side of things, a few R markdown tricks are worth noting:

This setup requires passing the source through a series of processors: knitr to run R and produce markdown; [pandoc](http://johnmacfarlane.net/pandoc/) to convert markdown to appropriate LaTeX using the [beamer](https://bitbucket.org/rivanvx/beamer/wiki/Home) package; and xelatex to set the final PDF. All of this is wrapped up in the `Makefile`. I separate out some beamer formatting commands into `preamble-slides.tex` and `slides.template` (the latter is possibly superfluous). The `Makefile` invokes `latexmk` rather than `xelatex` directly; that's because I let `latexmk` take care of dependencies on included files and bibliography (yes, the citations are auto-generated with biblatex; I haven't included my bibliography database).

To continue the Makefile theme: I learned how to encode a dependency on external R source; knitr includes a `cache.extra` chunk option which you can tie to the modification date of the file. See the `setup` chunk.

Generating tables for display is a pain. I use the R [xtable](http://cran.r-project.org/web/packages/xtable/) package to produce LaTeX tables for typesetting, but the trick is figuring out the apparently totally random conjunction of parameters to `xtable()` and `print.xtable()` necessary to get a reasonable result. Notice, in particular, that if you wish to print a table of two columns from a data frame, you must first tell `xtable` how to align *three* columns and then pass `include.rownames=F` to `print`. As a further bonus, `xtable` insists on generating a LaTeX comment next to each table which markdown then escapes. So I strip these out using `sed` in the Makefile, not neglecting to devise a way to prevent this from destroying my `%` delimited pandoc title block. I can't begin to tell you how much fun it is to figure these things out the night before giving a talk.

For MLA, I failed to figure out why, despite using tikz graphics, my plots' typeface didn't match the slides'. After the talk I remembered why: knitr generates tikz plots in independent files and includes them as PDF's. You thus have to get knitr to set the typeface in those independent files. The rather-deeply-buried trick is to set an R option in the setup chunk:

```R
options(tikzXelatexPackages=c(
    "\\usepackage{tikz}\n",
    "\\usepackage[active,tightpage,xetex]{preview}\n",
    "\\usepackage{fontspec,xunicode}\n",
    "\\setmainfont{Gill Sans}\n",
    "\\PreviewEnvironment{pgfpicture}\n",
    "\\setlength\\PreviewBorder{0pt}\n"))
```

This is the default `tikzXelatexPackages` with the call to `setmainfont` stuck in. knitr reads this option and writes these commands into its plotting files. The R `options` mechanism remains one of the stupidest language-design decisions I have ever seen; [arRgh](https://github.com/tdsmith/aRrgh). I have not made this change to the source in this repository.

## mlaib2014/modeling_stoplist.txt

After some trial and error, I created this stoplist, which I used both for the topic model and for counting words in the title corpus, from a longer list compiled by Ted Underwood and me ([http://hdl.handle.net/2142/45709](http://hdl.handle.net/2142/45709)).

## Acknowledgments

I'd like to record my gratitude here to the MLA panel co-organizers, Richard So and Hoyt Long; the panel respondents, Matt Jockers and Amy Hungerford; Barbara Chen and Kathleen Fitzpatrick of MLA, Socorro Ortega of EBSCO, Dan Edelstein, and Annette Keogh for their help with the research on the data of the MLA bibliography of which this is a part; audiences at Stanford and the Modernist Studies Association who heard some related material; Ted Underwood, for collaboration on other work that has shaped this; and Anne DeWitt, who's talked it all over many times.

Andrew Goldstone  
January 15, 2014


