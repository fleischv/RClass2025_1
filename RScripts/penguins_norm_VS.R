pacman::p_load(conflicted,wrappedtools,tidyverse, here, ggbeeswarm, ggsci, ggsignif, ggthemes, ggridges, patchwork )
conflicts_prefer(dplyr::filter)

data(penguins)
head(penguins)#

numvars <- 
  ColSeeker(data = penguins,  # can be omitted, as it is the default 
            namepattern = "_")#searching for column 

#Explore Normallity in the Penguins data set. 
#Use figures and various Tests, in total sample as well as sub-groups


# ungrouped visual analysis 
G1<- penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `flipper_len`))+
  geom_density(alpha=.4, fill="pink")

G2 <- penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_len`))+
  geom_density(alpha=.4, fill="pink")

G3 <- penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_dep`))+
  geom_density(alpha=.4, fill="pink")

G4 <- penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `body_mass`))+
  geom_density(alpha=.4, fill="pink")

#alle Graphen nebeneinander darstellen lassen
G5 <-G1+G2+G3+G4


#creating all figures in a loop
for(var_i in numvars$name){
  plot_temp <-
    ggplot(penguins, aes(x=.data[[var_i]]))+
    geom_density()
  print(plot_temp)
  
}
#ungrouped testing

penguins_normal <- tibble(
  Variables=numvars$names,
  pKS=NA_real_,
  pSh=NA_real_
)

for(var_i in seq_len(numvars$count)){
  penguins_normal$pKS[var_i] <-
    ksnormal(penguins[[numvars$names[var_i]]])
  penguins_normal$pSh[var_i] <- 
    shapiro.test(penguins |> 
                   pull(numvars$names[var_i]))$p.value
}

head(penguins_normal)
penguins_normal <-penguins_normal |> 
  mutate(pKS=formatP(pKS,ndigits=3, mark = TRUE),
         pSh=formatP(pSh,ndigits=3, mark = TRUE))

for(var_i in numvars$names){
  # Hole die p-Werte für die aktuelle Variable
  p_val_KS <- penguins_normal$pKS[penguins_normal$Variables == var_i]
  p_val_Shapiro <- penguins_normal$pSh[penguins_normal$Variables == var_i]
  
  # Erstelle den Dichteplot
  plot_temp <- ggplot(penguins, aes(x = .data[[var_i]])) +
    geom_density(fill = "skyblue", alpha = 0.5) +
    ggtitle(paste0(var_i, " - KS-Test p = ", p_val_KS, 
                   " / Shapiro-Test p = ", p_val_Shapiro)) +
    annotate("text", 
             x = max(penguins[[var_i]], na.rm = TRUE), 
             y = 0.02, 
             label = paste0("KS: p = ", p_val_KS, "\nShapiro: p = ", p_val_Shapiro),
             color = "red", size = 4, angle = 0, hjust = 1) +
    labs(x = var_i, y = "Dichte")
  
  # Zeige den Plot
  print(plot_temp)
}


penguiens_normal1<- 
  penguins |>
  filter(!is.na(sex))|>
  summarize(across(all_of(numvars$names),
                   .fns = list(
                     pKS=~ksnormal(.x) |> 
                       formatP(mark = TRUE),
                     pSh=~shapiro.test(.x) |> 
                       pluck("p.value") |> 
                       formatP(mark = TRUE)))) |> 
  pivot_longer(everything(),
               names_to=c("Variable",".value"), 
               names_pattern = "^(.+)_(p.+)$") 


#grouped visual inspection

penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `flipper_len`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(species), cols = vars(sex), margins = TRUE)
penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `flipper_len`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(species), cols = vars(island), margins = TRUE)
penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `flipper_len`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(sex), cols = vars(island), margins = TRUE)

penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_len`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(species), cols = vars(sex), margins = TRUE)
penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_len`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(species), cols = vars(island), margins = TRUE)
penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_len`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(sex), cols = vars(island), margins = TRUE)

penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_dep`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(species), cols = vars(sex), margins = TRUE)
penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_dep`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(species), cols = vars(island), margins = TRUE)
penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_dep`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(sex), cols = vars(island), margins = TRUE)

penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `body_mass`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(species), cols = vars(sex), margins = TRUE)
penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `body_mass`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(species), cols = vars(island), margins = TRUE)
penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `body_mass`))+
  geom_density(alpha=.4, fill="pink")+
  facet_grid(rows=vars(sex), cols = vars(island), margins = TRUE)


  

#by sex

penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_len`,fill=sex))+
  geom_density(alpha=.4)

norm_data_penguins <- penguins|>
  drop_na()|>
  group_by(species, sex) |> 
  summarize(
    `pGauss (KS)` = ksnormal(body_mass),
    `pGauss (Shapiro)` = shapiro.test(body_mass) |> 
      pluck("p.value"),
    .groups = "drop")#
norm_data_penguins
ggplot(penguins,aes(body_mass))+
  geom_density()+
  geom_label(data=norm_data_penguins,
             family="xkcd Script",
             x = Inf,#6000, 
             hjust=1.1,
             y = Inf,#1.2*10^-3
             vjust=1.1,
             size=2.5,
             aes(label=paste("p (KS):\n",formatP(`pGauss (KS)`))))+
  facet_grid(cols=vars(sex), rows = vars(species))



penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_dep`,fill=sex))+
  geom_density(alpha=.4)

penguins |>
  filter(!is.na(sex))|>
  group_by(sex) |>
  summarize(
    n=n(),
    p_KS = ksnormal(`bill_dep`,lillie = FALSE),
    `pGauss (Shapiro)` = shapiro.test(`bill_len`)$p.value)

penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `flipper_len`,fill=sex))+
  geom_density(alpha=.4)

penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `body_mass`,fill=sex))+
  geom_density(alpha=.4)

#by species

penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_len`,fill=species))+
  geom_density(alpha=.4)

penguins |>
  filter(!is.na(sex))|>
  group_by(species) |>
  summarize(
    n=n(),
    p_KS = ksnormal(`bill_len`,lillie = FALSE),
    `pGauss (Shapiro)` = shapiro.test(`bill_len`)$p.value)


penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `flipper_len`,fill=species))+
  geom_density(alpha=.4)

penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `body_mass`,fill=species))+
  geom_density(alpha=.4)

# by island
penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `bill_len`,fill=island))+
  geom_density(alpha=.4)

penguins |>
  filter(!is.na(sex))|>
  group_by(island) |>
  summarize(
    n=n(),
    p_KS = ksnormal(`bill_len`,lillie = FALSE),
    `pGauss (Shapiro)` = shapiro.test(`bill_len`)$p.value)|>

  penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `flipper_len`,fill=island))+
  geom_density(alpha=.4)


penguins |>
  filter(!is.na(sex))|>
  ggplot(aes(x = `body_mass`,fill=island))+
  geom_density(alpha=.4)


#testing for normality with grouping
grouped_by_sex <- 
  penguins |>
  drop_na()|>
  group_by(sex) |>
  summarize(across(all_of(numvars$names),
                   .fns = list(
                     `pGauss (KS)`=~ksnormal(.x) |> 
                       formatP(mark = TRUE),
                     pSh=~shapiro.test(.x) |> 
                       pluck("p.value") |> 
                       formatP(mark = TRUE)))) |> 
  pivot_longer(-sex,
               names_to=c("Variable",".value"), 
               names_pattern = "^(.+_.+)_(.+)$") 


grouped_by_sex_species <- 
  penguins |>
  drop_na()|>
group_by(species, sex) |> 
  summarise(
    across(all_of(numvars$names),
           .fns = ~ksnormal(.x) |> 
             formatP(ndigits = 5,mark = T)),
    .groups = 'drop') |>
  pivot_longer(cols = all_of(numvars$names), # contains("_")
               names_to = "Measure",
               values_to = "p-value (KS-test)") |>
  arrange(Measure)|>
  pivot_wider(names_from = c(species,sex),
              # names_sep=":",
              # names_glue="{species} ({sex})",
              values_from = `p-value (KS-test)`)

ggplot(penguins,aes(body_mass))+
  geom_density()+
  geom_label(data=grouped_by_sex_species,
             family="xkcd Script",
             x = Inf,#6000, 
             hjust=1.1,
             y = Inf,#1.2*10^-3
             vjust=1.1,
             size=2.5)
            +
  facet_grid(cols=vars(sex), rows = vars(species))


grouped_by_species <- 
  penguins |>
  filter(!is.na(sex))|>
  group_by(species) |>
  summarize(across(all_of(numvars$names),
                   .fns = list(
                     pKS=~ksnormal(.x) |> 
                       formatP(mark = TRUE),
                     pSh=~shapiro.test(.x) |> 
                       pluck("p.value") |> 
                       formatP(mark = TRUE)))) |> 
  pivot_longer(-species,
               names_to=c("Variable",".value"), 
               names_pattern = "^(.+_.+)_(.+)$") 


grouped_by_islands <- 
  penguins |>
  filter(!is.na(sex))|>
  group_by(island) |>
  summarize(across(all_of(numvars$names),
                   .fns = list(
                     pKS=~ksnormal(.x) |> 
                       formatP(mark = TRUE),
                     pSh=~shapiro.test(.x) |> 
                       pluck("p.value") |> 
                       formatP(mark = TRUE)))) |> 
  pivot_longer(-island,
               names_to=c("Variable",".value"), 
               names_pattern = "^(.+_.+)_(.+)$") 

grouped_by_islands1 <- 
  penguins |>
  filter(!is.na(sex))|>
  group_by(island) |>
  summarize(across(all_of(numvars$names),
                   .fns = list(
                     pKS=~ksnormal(.x) |> 
                       formatP(mark = TRUE),
                     pSh=~shapiro.test(.x) |> 
                       pluck("p.value") |> 
                       formatP(mark = TRUE)))) |> 
  pivot_longer(-island,
               names_to=c("Variable",".value"), 
               names_pattern = "^(.+_.+)_(.+)$") 


ggplot(penguins,aes(body_mass))+
  geom_density()+
  geom_label(data=norm_test_out,
             family="xkcd Script",
             x = Inf,#6000, 
             hjust=1.1,
             y = Inf,#1.2*10^-3
             vjust=1.1,
             size=2.5,
             aes(label=paste("p (KS):\n",formatP(pKS))))+
  facet_grid(cols=vars(sex), rows = vars(species))


