
plot_compare <- function(ind, df = db, ccode=target, comparators=main_comparators, labels=F, reverse_labels=NULL, range=c(1990, 2021)) {

    return(df %>%
           filter(weo_countrycodeiso %in% c(ccode,comparators)) %>%
           filter((year >= range[1]) & (year <= range[2])) %>%
           ggplot(aes_string(x="year", y=ind, group="year", label="weo_countrycodeiso")) +
           geom_boxplot(outlier.shape = NA) +
           geom_text(alpha = ifelse(labels, 0.1, 0)) +
           geom_line(data = . %>% filter(weo_countrycodeiso == ccode), aes(group=NA), color="orange", size= 1) +
           geom_point(data = . %>% filter(weo_countrycodeiso == ccode), aes(group=NA), color="orange") +
           labs(x="Year", y=reverse_labels[ind])
    )
}

plot_compare_lines <- function(ind, df=db, ccode=target, comparators=main_comparators, labels=F, reverse_labels=NULL, range=c(1990, 2021)) {

    return(df %>%
           filter(weo_countrycodeiso %in% c(ccode,comparators)) %>%
           filter((year >= range[1]) & (year <= range[2])) %>%
           ggplot(aes_string(x="year", y=ind, label="weo_countrycodeiso")) +
           geom_line(data = . %>% filter(weo_countrycodeiso != ccode), aes(color=weo_countrycodeiso), alpha=0.4) +
           geom_line(data = . %>% filter(weo_countrycodeiso == ccode), color="orange", size= 1) +
           geom_point(data = . %>% filter(weo_countrycodeiso == ccode), color="orange") +
           labs(x="Year", y=reverse_labels[ind])
    )

}

plot_scatter <- function(ind1, ind2, df=db, ccode=target, comparators=main_comparators, labels=F, reverse_labels=NULL, range=c(1990, 2021)) {

    return(
           df %>%
               filter(weo_countrycodeiso %in% c(ccode, comparators)) %>%
               filter(year >= range[1] & year <= range[2]) %>%
               ggplot(aes_string(x=ind1, y=ind2, label="weo_countrycodeiso")) +
               geom_point(data = . %>% filter(weo_countrycodeiso != ccode), alpha=0.4) +
               geom_point(data = . %>% filter(weo_countrycodeiso == ccode), color="orange", size=2) +
               labs(
                    x=reverse_labels[ind1],
                    y=reverse_labels[ind2]
               )
    )

}
