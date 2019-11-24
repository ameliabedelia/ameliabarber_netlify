---
title: Phylogeny vs. popularity in the birds of New Zealand
author: ''
date: '2019-11-24'
slug: nz-bird-phylogeny-popularity
categories: []
tags:
  - Rstats
  - Tidy Tuesday
  - data science
  - purrr
subtitle: ''
description: "Looking at the association between the phylogenetic relationship of New Zealand's birds and their popularity"
image: ''
showtoc: false
---



## What is the relationship between bird phylogeny and popularity?

<img src="/post/2019-11-24-new-zealand-bird-taxonomy-popularity_files/kakapo.jpg" alt="kakapo" width="500px"/>

The [Tidy Tuesday dataset](https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-11-19) this week is on New Zealand's "Bird of the Year 2019" so I thought it might be fun to look at how the phylogeny of the birds of NZ correlates with their popularity. Are the most popular birds closely related to each other? Or are popular birds scattered throughout the [phylogenetic tree](https://en.wikipedia.org/wiki/Phylogenetic_tree) of birds in the competition? 

_If you are a data scientist not familiar with evolutionary biology, a phylogeny is essentially a way of classifying organisms and their relationship to each other based on their evolutionary history - often measured by DNA similarity._

The packages we'll be using this week, in addition to the core tidyverse packages, are `wikitaxa` to help convert common names to scientific names, `treeio` to read the phylogenetic tree into R, `ggtree` to visualize the phylogeny, and `patchwork` to combine the tree with a bar chart of the vote data, so let's load these packages and get started. Ironically, we're also going to use the [`purrr`](https://purrr.tidyverse.org/) package a lot in our bird data exploration.


```r
library(tidyverse)
library(wikitaxa)
library(treeio)
library(ggtree)
library(patchwork)
```

We can also grab the data from the Tidy Tuesday repo.


```r
nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")
```

The  "Bird of the Year" voting was done in a rather complicated instant runoff system over several weeks, so if we look at the raw data, we can see that we have dates, hours, and vote ranks because people could vote for up to 5 birds. To simplify this, we'll just look at the total number of votes each bird received. 


```r
nz_bird
```

```
## # A tibble: 217,300 x 4
##    date        hour vote_rank bird_breed         
##    <date>     <dbl> <chr>     <chr>              
##  1 2019-10-28     8 vote_1    Gibson's Albatross 
##  2 2019-10-28     8 vote_2    Tūī                
##  3 2019-10-28     8 vote_3    Kākā               
##  4 2019-10-28     8 vote_4    Kākāpō             
##  5 2019-10-28     8 vote_5    Little Spotted Kiwi
##  6 2019-10-28     8 vote_1    Spotted Shag       
##  7 2019-10-28     8 vote_2    Fantail            
##  8 2019-10-28     8 vote_3    Weka               
##  9 2019-10-28     8 vote_4    <NA>               
## 10 2019-10-28     8 vote_5    <NA>               
## # … with 217,290 more rows
```

## Extract the common names of the top 30 birds

Overall, there were 85 different birds in the running for Bird of the Year.


```r
nz_bird %>%
    filter(!is.na(bird_breed)) %>% 
    distinct(bird_breed) %>% 
    nrow()
```

```
## [1] 85
```

For the sake of overcrowding in our phylogenetic tree, we're going to focus on top 30 in terms of total votes


```r
top_birds <- nz_bird %>%
    filter(!is.na(vote_rank) & !is.na(bird_breed)) %>%
    count(bird_breed) %>%
    arrange(desc(n)) %>%
    top_n(30) %>% 
    pull(bird_breed)

top_birds
```

```
##  [1] "Yellow-eyed penguin"       "Kākāpō"                   
##  [3] "Black Robin"               "Kākā"                     
##  [5] "Banded Dotterel"           "Kea"                      
##  [7] "Tūī"                       "Blue Duck"                
##  [9] "Fantail"                   "New Zealand Falcon"       
## [11] "Kererū"                    "Morepork"                 
## [13] "Antipodean Albatross"      "Little Penguin"           
## [15] "Orange-fronted Parakeet"   "Rockhopper Penguin"       
## [17] "Little Spotted Kiwi"       "Kingfisher"               
## [19] "Southern Brown Kiwi"       "Fiordland Crested Penguin"
## [21] "South Island Kōkako"       "Mōhua"                    
## [23] "Kōkako"                    "New Zealand Dotterel"     
## [25] "Rifleman"                  "Stitchbird"               
## [27] "Takahē"                    "Bittern"                  
## [29] "Fairy Tern"                "Saddleback"
```

Unfortunately, the dataset only provides us with the common names of the birds, while any sort of taxonomic database is going to require (or be more accurate) if we can provide the scientific names.

## Using `wikitaxa` to obtain for scientific names from a bird's common name

I initially tried to get the scientific names for the birds from their common name using the `comm2sci()` function from the `taxize` package, but the results were pretty incomplete and inaccurate, so I ended up using the `wikitaxa` package which provides an easy way to access the WikiSpecies API.

To search WikiSpecies, I took a vector of the top birds, and used purrr's `map()` function to search WikiSpecies for each bird name.


```r
bird_list <- top_birds %>% 
    set_names() %>% 
    map(wikitaxa::wt_wikispecies_search)
```

## Pluck hits from WikiSpecies results list

If we look at a sample result from our WikiSpecies search results, we can see the resulting list has a lot of stuff we don't really care about.


```r
bird_list[[1]]
```

```
## $batchcomplete
## [1] ""
## 
## $query
## $query$searchinfo
## $query$searchinfo$totalhits
## [1] 4
## 
## $query$searchinfo$suggestion
## [1] "yellow eyles pengi"
## 
## $query$searchinfo$suggestionsnippet
## [1] "yellow <em>eyles pengi</em>"
## 
## 
## $query$search
## # A tibble: 4 x 7
##      ns title       pageid   size wordcount snippet                  timestamp  
##   <int> <chr>        <int>  <int>     <int> <chr>                    <chr>      
## 1     0 Megadyptes… 3.21e4    800        74 " (Endangered) čeština:… 2019-09-18…
## 2     0 List of vi… 9.65e5 156654     14868 "<span class=\"searchma… 2018-02-15…
## 3     0 List of vi… 1.10e6 173807     16483 "<span class=\"searchma… 2018-05-23…
## 4     0 List of vi… 1.07e6 703111        61 "Begomovirus ssDNA(+/-)… 2019-11-24…
```

To grab the information we are most interested in (the search results themselves), we can use purrr's aptly named `pluck()` function to grab just the table with our potential species names.


```r
query_results <- map(bird_list, ~pluck(.x, "query", "search"))
```

As an example, here is what the database came up with for the second item in our list, the [Kākāpō](https://en.wikipedia.org/wiki/Kakapo):


```r
query_results[[2]] %>% 
    select(title)
```

```
## # A tibble: 3 x 1
##   title                  
##   <chr>                  
## 1 Strigops habroptila    
## 2 Strigops               
## 3 Ngaheremyia fuscipennis
```


## Condense multiple WikiSpecies hits down to a single (correct) scientific name

If we looked at the results for the Kākāpō above where the first search result is, indeed, the scientific name, we might assume that it would work to just take the first result for all our searches (we can also use `purrr::map_dfr()` to return a table of the results instead of a list).


```r
top_result <- map_dfr(query_results, ~head(.x, 1), .id = "common_name") %>% 
    select(common_name, latin_name = title)
```

However, if we take a closer look at the result, we can see that our strategy didn't work perfectly. For example, for the black robin, it picked a virus that infects black robins instead of the actual bird itself. And for the Kea, it chose Steven Chew Kea Foo, the ecologist for which (I assume) the bird was named, instead of the bird itself.

If this was a dataset with 1000 birds, I don't know how I would fix the problem. But because there were only a few mistakes to fix, I just manually did it by combining the results for all birds into a single dataframe, excluding the hits that I knew were wrong, and then using `dplyr::distinct()` to condense them down to one hit per species. There were also a couple birds that wikitaxa couldn't assign down to the species level from the common name, such as the [Rifleman](https://en.wikipedia.org/wiki/Rifleman_(bird)), so I googled them and replaced the genus name with a full species name (eg. _Acanthisitta_ to _Acanthisitta chloris_).



Unfortunately, the WikiSpecies search wasn't able to find matches for 3 of our 30 birds. To figure out which birds were missing results and save them for manual entry later, I used `purrr::keep()`.


```r
missing_birds <- keep(query_results, is.null) %>% 
    names()
missing_birds
```

```
## [1] "Banded Dotterel"           "Southern Brown Kiwi"      
## [3] "Fiordland Crested Penguin"
```

Again, if this was part of a bigger project, I would have tried to webscrap their Latin names somehow, but because it was only 3 entries, I just googled them and made a separate tibble of the missing birds, which I combined with the WikiSpecies list above.


```r
missing_bird_names <- tibble(
    common_name = missing_birds,
    latin_name = c("Charadrius bicinctus", "Apteryx australis", "Eudyptes pachyrhynchus")
)
```

Now at the end of it, we have a table with the common and scientific names of the top 30 birds in the New Zealand Bird of the Year voting, ready for phylogenetic analysis


```r
bird_names_complete <- bind_rows(bird_names, missing_bird_names)
bird_names_complete
```

```
## # A tibble: 30 x 2
##    common_name         latin_name                   
##    <chr>               <chr>                        
##  1 Yellow-eyed penguin Megadyptes antipodes         
##  2 Kākāpō              Strigops habroptila          
##  3 Black Robin         Petroica traversi            
##  4 Kākā                Nestor meridionalis          
##  5 Kea                 Nestor notabilis             
##  6 Tūī                 Prosthemadera novaeseelandiae
##  7 Blue Duck           Hymenolaimus malacorhynchos  
##  8 Fantail             Rhipidura maculipectus       
##  9 New Zealand Falcon  Falco novaeseelandiae        
## 10 Kererū              Hemiphaga novaeseelandiae    
## # … with 20 more rows
```

## Using NCBI Taxonomy to infer a phylogeny among the most popular birds of New Zealand

There are a lot of way to assign phylogenetic relationships, but because I am not a bird taxonomist and the overall goal was to look at bird popularity through the lens of species relationship, not create the most accurate NZ bird phylogeny, I decided to the use the [NCBI Common Tree](https://www.ncbi.nlm.nih.gov/taxonomy) to generate a phylogeny, rather than building my own from available sequence data.

To do this, I exported my list of newly-assembled Latin names to a text file, and uploaded that into the [NCBI Common Tree](https://www.ncbi.nlm.nih.gov/Taxonomy/CommonTree/wwwcmt.cgi) web portal


```r
plot_set <- bird_names_complete %>% 
    pull(latin_name)

write_lines(plot_set, "sci_names.txt")
```

Resulting in this lovely phylogeny:

![ncbi-screenshot](/post/2019-11-24-new-zealand-bird-taxonomy-popularity_files/Screenshot 2019-11-24 at 08.29.14.png)

The fantail (_Rhipidura maculipectus_) was not found in the NCBI taxonomy database, but it was able to place all the other birds into a phylogeny, which I then exported as a PHYLIP file to bring back into R.

## Using `treeio` to import and manipulate tree data

To bring the NCBI-generated phylogeny back into R, we can use `treeio::read.tree()`, which nicely allows us to convert the Newick text file into a tibble for tidyverse-style manipulation.  


```r
raw_tree <- treeio::read.tree(here::here("content/post/phyliptree_fix.phy")) %>% 
    as_tibble() %>% 
    mutate(label = str_replace_all(label, "_", " ")) %>% 
    left_join(bird_names_complete, by = c("label" = "latin_name")) %>%
    mutate(label = if_else(!is.na(common_name), glue::glue("{common_name} ({label})"),
                           label))
```

In this case, we're adding back the common names which got discarded when the list went to NCBI and using `glue()` to create nice tree labels that list both the common name and the scientific name.

We can then convert this back to a phylo object and visualize a cladogram of our popular New Zealand birds using `ggtree`.


```r
tree <- treeio::as.phylo(raw_tree)

ggtree(tree, branch.length = "none") +
    geom_nodelab(geom = "label", size = 3) +
    geom_tiplab() +
    coord_cartesian(clip = 'off') +
    theme(plot.margin=margin(6, 250, 6, 6))
```

<img src="/post/2019-11-24-new-zealand-bird-taxonomy-popularity_files/figure-html/unnamed-chunk-17-1.png" width="672" />

## Examining taxonomic relationship vs. bird popularity

**So how does phylogenetic relationship correlate with bird popularity?**

Let's first make a graph of how many votes each of the birds in our phylogeny got by going back to our original Tidy Tuesday NZ birds dataset, counting the number of votes for each bird species, and adding their Latin name to make the same "Common name (Latin name)" label we used in our tree.


```r
vote_df <- nz_bird %>%
    filter(!is.na(vote_rank) & !is.na(bird_breed)) %>%
    count(bird_breed) %>%
    arrange(desc(n)) %>%
    top_n(30) %>% 
    left_join(bird_names_complete, by = c("bird_breed" = "common_name")) %>% 
    mutate(label = glue::glue("{bird_breed} ({latin_name})")) %>% 
    select(label, n)

vote_df
```

```
## # A tibble: 30 x 2
##    label                                          n
##    <glue>                                     <int>
##  1 Yellow-eyed penguin (Megadyptes antipodes) 12022
##  2 Kākāpō (Strigops habroptila)               11161
##  3 Black Robin (Petroica traversi)             8304
##  4 Kākā (Nestor meridionalis)                  7655
##  5 Banded Dotterel (Charadrius bicinctus)      6692
##  6 Kea (Nestor notabilis)                      6364
##  7 Tūī (Prosthemadera novaeseelandiae)         6288
##  8 Blue Duck (Hymenolaimus malacorhynchos)     6194
##  9 Fantail (Rhipidura maculipectus)            6120
## 10 New Zealand Falcon (Falco novaeseelandiae)  5992
## # … with 20 more rows
```




Because ggtree objects play very nicely with the `tidyverse`, we can use `dplyr` verbs to take our tree, filter the tips where our species names are, use `select()` to get the y coordinate of each tip, and then join that with our calculated vote dataframe to create a bar chart with y positions that match our tree. We'll also highlight the 3 bird species who got the most votes in the New Zealand Bird of the Year competition in our phylogeny and in our plot to really distinguish them. 

(the plot doesn't look like much on its own)


```r
p2 <- p1 %>% 
    filter(isTip) %>% 
    select(label, y) %>%
    left_join(vote_df) %>%
    mutate(highlight = if_else(n > 8000, "yes", "no")) %>% 
    ggplot(aes(y, n, fill = highlight)) + 
    geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = colors) +
    coord_flip() +
    annotate("text", x = 0.5, y = 12500, label = "Total # Votes", hjust = 1) +
    xlim(0.5, 29.5) +
    theme_tree2()
p2
```

<img src="/post/2019-11-24-new-zealand-bird-taxonomy-popularity_files/figure-html/unnamed-chunk-20-1.png" width="672" />

But using `patchwork`, we can combine our tree with our bar chart into one seamless plot:


```r
p1 + p2 + plot_annotation(
    title = "Are closely related New Zealand birds equally popular?",
    subtitle = glue::glue("Among the 30 most popular birds in the 'NZ Bird of the Year competition'
                          The three bird species receiving the most votes are highlighted in teal."),
    caption = "Data: NZ Forest and Bird Organization\n Visualization @frau_dr_barber",
    theme = cowplot::theme_map()
    )
```

<img src="/post/2019-11-24-new-zealand-bird-taxonomy-popularity_files/figure-html/unnamed-chunk-21-1.png" width="768" />

### So, are the most popular birds closely related?

Not really. Looking at the plot, there doesn't seem to be a clear relationship between where a species is in the phylogeny and the number of votes they received. 

Additionally, the three birds receiving the most votes belong to different families. The overall winner, the yellow-eyed penguin is in the _Spheniscidae_ family, the second place kākāpō (of [R4DS](https://r4ds.had.co.nz/) cover fame) is in the _Psittacidae_ family, while the third place black robin is a _passeriformes_.

***

End note: the critically-endangered kākāpō is currently battling an aspergillosis outbreak *(incidentally, one of the main fungi I study in my day job)*, so if you have enjoyed this post, please consider donating to their rescue effort.  
https://www.doc.govt.nz/kakapo-donate

