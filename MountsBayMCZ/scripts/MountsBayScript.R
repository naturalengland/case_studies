#Mounts Bay MCZ
#attempt to implement: 
#Mount’s Bay rMCZ Intertidal Rock and Sediment Verification Survey 2013/2014

#July 2017
#Christoph Kratz

#markdown options ----
knitr::opts_knit$set(root.dir = getwd())
knitr::opts_knit$get("root.dir")

#packages ----
library("tidyverse")
library("stringr")
library("vegan")
library("ecodist")
library("forcats")

#import data ----
raw <- read.csv("data/RockQuads.csv", blank.lines.skip = T) #import csv files

#remove blank lines
raw <- raw %>% 
  filter(Quadrat != "") 

# Check column classes
#table(sapply(raw, class))
#cols.num <- colnames(raw)[-c(1:7, 108, 109)] 
raw <- data.frame(lapply(raw, gsub, pattern = "%", replacement = "", fixed = T)) #remove percent dsigns
raw <- data.frame(lapply(raw, str_replace_na , replacement = 0)) #change NA to 0 #change NA to 0

#tidy data ----
rock.quads <- raw %>% select(-c(X, X.1, X.2)) #drop unnecessary variables
rock.quads[7:106] <- lapply(rock.quads[7:106], as.character) # I think this is dropping values
rock.quads <- gather(rock.quads, 7:106, key = "species", value = "abundance", na.rm = T)#turns it into 'tidy' data
#note that this is imperfect as you need to count columns, need to improve this
rock.quads$abundance <- as.numeric(rock.quads$abundance) #make abundance numeric
rock.quads <- filter(rock.quads, abundance > 0)

#summarise data ----

rock.quads %>% 
  group_by(species = species) %>% 
  summarise(min = min(abundance), 
            ave = mean(abundance), 
            max = max(abundance), 
            sd = sd(abundance)
            ) %>%
  arrange(desc(max))

##problem here: seems to be dropping quite a few values,.  Probably happens at lapply


#row.names(rock.quads.spp) <- rock.quads.plot$Quadrat #this doesn't seem to persist in dplyr

#clean data noted that quadrat data here is a mixture of percent cover and count
#of individuals.  Assume for the time being that these can be treated as
#equivalent abundance figures, but will need to see how that goes.

#subset data ----
a1.112.quads <- 
  filter(rock.quads, Biotope == "A1.112") %>% #subset data for single biotope
  select(-Biotope:-Comments) %>% #remove plot info
  filter(abundance > 0) #remove blank rows

#transform data
a1.112.quads$abundance.sqrt <- sqrt(a1.112.quads$abundance)
a1.112.quads$abundance.log10 <- log10(a1.112.quads$abundance)

#spread data to species site matrix (for "vegan")
a1.112.quads.spp <- spread(a1.112.quads[c("Quadrat","species", "abundance.sqrt")], key = species, value = abundance.sqrt, fill = 0)
rownames(a1.112.quads.spp) <- a1.112.quads.spp$Quadrat
a1.112.quads.spp$Quadrat <- NULL


#Richness: How many species are there? ----
#number of taxa per sample


a1.112.quads %>% group_by(quadrat = Quadrat) %>% count() #tabulate species number

length(unique(a1.112.quads$species)) #total number of species recorded in habitat



#Abundance: Which species are most or least common? ----
#mean cover /  count
a1.112.quads %>% 
  group_by(species = species) %>% 
  summarise(ave = mean(abundance), sd = sd(abundance)) %>%
  arrange(desc(ave))

#mean cover /  count transformed
mean.cover.count <- a1.112.quads %>% 
  group_by(species = species) %>% 
  summarise(ave = mean(abundance.sqrt), sd = sd(abundance.sqrt)) %>%
  arrange(desc(ave))
mean.cover.count

levels(a1.112.quads$species) <- mean.cover.count$species[order(mean.cover.count$ave)]

ggplot(data = a1.112.quads, aes(x = species, y = abundance.sqrt)) +
  geom_boxplot() +
  coord_flip()

#try log transforming

ggplot(data = a1.112.quads, aes(x = species, y = abundance.log10)) +
  geom_boxplot() +
  coord_flip()

#mean frequency

# mean frequency by species and plot


# can specify additional possible row or column levels


# Diversity: Is the community dominated by any species, or is it even? ----
# Shannon Wiener diversity index
a1.112.sw <- diversity(x = a1.112.quads.spp, index = "shannon")

# Margalef species richness

a1.112.quads.n <- 
  a1.112.quads %>% 
  group_by(Quadrat) %>% 
  summarise(n = length(species), N = sum(abundance), Nsqrt = sum(abundance.sqrt))

a1.112.d <- (a1.112.quads.n$n -1)/log(a1.112.quads.n$N)
  
a1.112.d <- a1.112.quads.n %>% mutate(d = (n-1)/log(N))

# Pielou’s eveness

#Similarity ----


#calculate Bray Curtis dissimilarity - package 'vegan'
a1.112.quads.bc <- vegdist(a1.112.quads.spp, method = "bray") 
a1.112.quads.bc
summary(a1.112.quads.bc)

#calculate Bray Curtis dissimilarity - package 'ecodist'
a1.112.quads.bc2 <- distance(a1.112.quads.spp, method = "bray-curtis")
a1.112.quads.bc2
summary(a1.112.quads.bc2)
#cool, same result as using vegdist.  Maybe this one is more acceptable
#because it doesn't imply 'vegetation' in the title!!!


#Multidimensional scaling 
a1.112.quads.nmds <- nmds(a1.112.quads.bc) #carry out NMDS on bray curtis distance
summary(a1.112.quads.nmds)
a1.112.quads.nmds.min <- min(a1.112.quads.nmds, dims =2) ## choose the best two-dimensional solution to work with
#this needs to be done on all teh plots, otherwise it makes little sense. 


plot(a1.112.quads.nmds)
plot(a1.112.quads.nmds.min)
