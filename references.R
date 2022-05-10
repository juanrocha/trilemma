library(tidyverse)
library(XML)
library(RefManageR)

dat <- xmlParse("trilemma_refs.xml")

dat_df <- dat["//record"] |> xmlToDataFrame() |> 
    as_tibble() |> janitor::clean_names()

dat["//record/ref-type"] |> unlist(recursive = FALSE) 
l <- dat["//record/contributors"] |> unlist() #|> 
    # map(function (x) {str_extract(x, "<style*/style>")})

dat_df <- dat_df |> 
    select(-c(database:foreign_keys), -c(pub_location:num_vols)) |>  #pull(ref_type ) |> unique()
    mutate(ref_type = case_when(
        ref_type == 17~ "Article", 
        ref_type == 13~ "Misc",
        ref_type == 6~ "Book", 
        ref_type == 27~ "Report", 
        ref_type == 59~ "Misc",
        ref_type == 28~ "Book",
        ref_type == 19~ "Article",
        ref_type == 36~ "Misc",
        ref_type == 43~ "Article",
    )) |> rename(bibtype = ref_type, author = contributors, title = titles, year = dates, journal = periodical) |> 
    mutate(institution = case_when(bibtype == "Report" ~ publisher)) 

stops <- str_locate(dat_df$author, "^*,")

dat_df <- dat_df |> 
    mutate(stop = stops[,1]) 
dat_df$stop[is.na(dat_df$stop)] <- 5

dat_df <- dat_df |> 
    mutate(firstauthor = str_trunc(author, width = stop-1 , ellipsis = "")) |> 
    mutate(firstauthor = str_remove_all(firstauthor, "([:punct:]|[:space:])")) |> 
    mutate(key = str_c(firstauthor, year, sep = ":")) |> 
    group_by(key) |> 
    add_count() |> 
    mutate(ord = order(key)) |> 
    mutate(key = case_when(n > 1 ~ paste0(key, letters[ord]), TRUE ~ key)) |> 
    select(-firstauthor, -n, -ord,-stop) 
# 
# dat_df |>
#     as.BibEntry() |>
#     WriteBib(file = "references_old.bib")

dat_df |> 
    select(year, key) |> 
    print(n=121)


