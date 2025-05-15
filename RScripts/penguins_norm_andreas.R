---
  title: "Cute penguins"
format: 
  docx:
  reference-doc:  F:/Aktenschrank/Analysen/R/Rtemplate1.docx
editor: visual
execute: 
  echo: false
warning: false
output: asis
fig-dpi: 300
fig-width: 6
fig-height: 8
tbl-cap-location: top
---
  
  ```{r setupstuff}
pacman::p_load(conflicted,tidyverse,wrappedtools,
               rlist, flextable,
               patchwork, ggbeeswarm)
conflicts_prefer(dplyr::filter, dplyr::select)
set_flextable_defaults(big.mark = " ", 
                       font.size = 9, 
                       theme_fun = theme_vanilla,
                       padding.bottom = 1, 
                       padding.top = 3,
                       padding.left = 3,
                       padding.right = 4,
                       font.family = "xkcd Script"
)
theme_set(theme_bw())
theme_update(
  plot.title=element_text(family="xkcd Script"),
  plot.caption=element_text(family="xkcd Script"),
  axis.title=element_text(family="xkcd Script"),
  axis.text=element_text(family="xkcd Script"),
  strip.text=element_text(family="xkcd Script"))
```

```{r dataprep}
rawdata <- penguins |>
  drop_na() |> 
  mutate(year=factor(year))
```

# Test for Normal Distribution

We are testing all numerical measures for a ***gaussian*** distribution.

## Step 1: single measure, all penguins

```{r}
p_ks <- ksnormal(rawdata$body_mass,lillie = F) |> 
  formatP(ndigits = 5,mark = T, pretext = T)
p_sw <- shapiro.test(rawdata$body_mass) |> 
  pluck("p.value") |> 
  formatP(,ndigits = 5,mark = T, pretext = T)
ggplot(rawdata,aes(body_mass))+
  geom_density()+
  ggtitle(paste0("KS-Test: p ",p_ks,
                 " / Shapiro-Test: p ", p_sw))+
  labs(caption = paste0("KS-Test: p ",p_ks,
                        " / Shapiro-Test: p ", p_sw))+
  annotate(geom = 'label',
           family="xkcd Script",
           x = 5500, y = 4*10^-4,
           label=paste0("KS-Test: p ",p_ks,
                        "\nShapiro-Test: p ", p_sw))

ggplot(rawdata,aes(body_mass, color = "distribtion type"))+
  geom_line(stat = "density", linewidth = 1,
            aes(color="empirical"))+  
  stat_function(fun = dnorm, 
                args = list(mean = mean(rawdata$body_mass),
                            sd = sd(rawdata$body_mass)),
                aes(color = "theoretical"),
                linewidth = 1)+
  scale_color_manual(
    name = "distribution type",
    values = c("empirical" = "black",
               "theoretical" = "darkgreen"))+
  ggtitle(paste0("KS-Test: p ",p_ks,
                 " / Shapiro-Test: p ", p_sw))

```

## Step 2: single measure, by species/sex

```{r}
rawdata |> 
  group_by(species) |>
  summarize(
    `pGauss (KS)` = ksnormal(body_mass),
    `pGauss (Shapiro)` = shapiro.test(body_mass) |> 
      pluck("p.value")
  )
norm_test_out <- 
  rawdata |> 
  group_by(species, sex) |> 
  summarize(
    `pGauss (KS)` = ksnormal(body_mass),
    `pGauss (Shapiro)` = shapiro.test(body_mass) |> 
      pluck("p.value"),
    .groups = "drop")#
norm_test_out
ggplot(rawdata,aes(body_mass))+
  geom_density()+
  geom_label(data=norm_test_out,
             family="xkcd Script",
             x = Inf,#6000, 
             hjust=1.1,
             y = Inf,#1.2*10^-3
             vjust=1.1,
             size=2.5,
             aes(label=paste("p (KS):\n",formatP(`pGauss (KS)`))))+
  facet_grid(cols=vars(sex), rows = vars(species))

```

## Step 3: several Within Species

```{r}
#| fig-height: 4

numvarsV1 <- ColSeeker(namepattern = c("_"))
numvarsV2 <- ColSeeker(varclass = c('numeric',
                                    'integer'),
                       exclude = "year")

# ggplot in a loop

for(var_i in numvarsV1$names){
  plot_temp <- 
    ggplot(rawdata,aes(x = .data[[var_i]]))+
    geom_density()
  print(plot_temp)
}

quickcheck <- 
  rawdata |> 
  summarise(across(all_of(numvarsV1$names),
                   ksnormal))
if(interactive()){
  print(quickcheck)
}
#create a tibble for p-values with 0 rows
## what are the species?
sp_levels <- levels(rawdata$species)
normaltable <- tibble(Measurement='',
                      g1="",g2="",g3="",
                      .rows = 0)
colnames(normaltable)[-1] <- sp_levels

# create a list for plots
plotlist <- list()

#create loop structure
# loop index??
for(var_i in numvarsV2$names){
  # compute shapiro
  shapiro_p <-
    rawdata |> 
    group_by(species) |> 
    summarise(
      pShapiro=shapiro.test(
        .data[[var_i]]) |>
        pluck("p.value") |>
        formatP(mark = T))
  
  # put varname and p in results
  normaltable <- add_row(normaltable,
                         Measurement=var_i,
                         Adelie=shapiro_p$pShapiro[1],
                         Gentoo=shapiro_p$pShapiro[2],
                         Chinstrap=shapiro_p$pShapiro[3])
  
  # normaltable[nrow(normaltable),2:4] <-
  #   shapiro_p$pShapiro |>
  #   as.list()
  
  plot_hist <-
    rawdata |>
    ggplot(aes(x = .data[[var_i]],
               fill=species))+
    geom_histogram(color='black')+
    facet_grid(rows = vars(species),
               margins=TRUE)+
    scale_fill_discrete(
      "Species\n(p Shapiro)",
      labels = paste0(sp_levels,"\n(",
                      shapiro_p$pShapiro,")")
    )+
    guides(fill="none")
  # print(plot_hist)
  plotlist <- list.append(plotlist,plot_hist)
  names(plotlist)[length(plotlist)] <-
    paste0(var_i,'_hist')
  #+
  # ggtitle(paste(sp_levels,
  #               shapiro_p$pShapiro,collapse = "\n"))+
  # labs(caption = paste(sp_levels,
  #                      shapiro_p$pShapiro,
  #                      collapse = " / "))
  #+
  # theme(legend.position = 'bottom')
  plot_dens <-
    rawdata |>
    ggplot(aes(x = .data[[var_i]],
               fill=species))+
    geom_density()+
    facet_grid(rows = vars(species),
               cols=vars(sex),
               margins = TRUE)+
    scale_fill_discrete(
      "Species\n(p Shapiro)",
      labels = paste0(sp_levels,"\n(",
                      shapiro_p$pShapiro,")")
    )+
    scale_x_continuous(guide = guide_axis(n.dodge = 2))+
    guides(fill="none")+
    theme(legend.position="bottom")
  plotlist <- list.append(plotlist,plot_dens)
  names(plotlist)[length(plotlist)] <-
    paste0(var_i,'_dens')
  # print(plot_dens)
  print(plot_hist+plot_dens+ 
          plot_layout(#guides = "collect",
            widths = c(1,1.75)))
  cat('&nbsp;\n\n')
}
```


```{r}
# cat("&nbsp;\n\n")
cat("\\newpage\n\n")
normaltable |> 
  flextable() |> 
  bold(~str_detect(Adelie,"\\*"),j = 2,bold = T) |> 
  # bg(~str_detect(Adelie,"\\*"),j = 2,
  #    bg = 'deeppink') |> 
  bg(~str_detect(Chinstrap,"\\*"),j = 3,
     bg = 'deeppink') |> 
  bg(~str_detect(Gentoo,"\\*"),j = 4,
     bg = 'deeppink') |> 
  color(~!Adelie<.05,j = 2,
        color = "green") |> 
  set_caption("Test for normality within species") |> 
  set_table_properties(width = 1,layout = 'autofit')
cat('&nbsp;\n\n')

walk(plotlist,print)
```

## Step 4 several within Species and Sex

```{r}
normality_test <-
  rawdata |> 
  group_by(species, sex) |> 
  summarise(
    across(where(is.numeric),
           .fns = ~ksnormal(.x) |> 
             formatP(ndigits = 5,mark = T)),
    .groups = 'drop') |>
  pivot_longer(cols = where(is.character), # contains("_")
               names_to = "Measure",
               values_to = "p-value (KS-test)") |>
  arrange(Measure)|>
  pivot_wider(names_from = c(species,sex),
              # names_sep=":",
              # names_glue="{species} ({sex})",
              values_from = `p-value (KS-test)`)

normality_test |>
  # mutate(Variable=str_replace(Measure,
  #                             pattern="(.+)_(.+)_(.+)",
  #                             replacement="\\1 \\2 [\\3]" )) |> 
  # select(-Measure) |> 
  # select(Variable, everything()) |> 
  flextable() |>
  set_caption("Test for normality within species and sex") |> 
  separate_header(split = "_") |> 
  # theme_vader() |> 
  set_table_properties(width = 1,layout = 'autofit')
```