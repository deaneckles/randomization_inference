library(igraph)
library(foreign)
library(Matrix)
library(lfe)
library(dplyr)
library(foreach)

cain.all <- read.dta("cai_data/0422allinforawnet.dta")
cais.all <- read.dta("cai_data/0422survey.dta")

cai.all <- read.csv("cai_data/0422analysis.csv")
cai.all$id <- as.character(cai.all$id)

cain.all <- subset(cain.all, !is.na(network_id) & network_id != 99)

cain <- cain.all

cain.el <- apply(as.matrix(cain[, c("id", "network_id")]), 2, as.character)
cain.el <- cain.el[!is.na(cain.el[, 2]), ]

ids.in.net <- unique(c(cain.el[, 1], cain.el[, 2]))
egos.in.net <- unique(cain.el[, 1])
ids.in.survey <- unique(cai.all$id)
ids.in.net.only <- ids.in.net[!ids.in.net %in% ids.in.survey]
egos.in.net.only <- egos.in.net[!egos.in.net %in% ids.in.survey]
ids.in.survey.only <- ids.in.survey[!ids.in.survey %in% ids.in.net]

cai <- subset(cai.all, id %in% egos.in.net)

g <- graph_from_edgelist(cain.el, directed = TRUE)

cain.peer.summary <- cain[!duplicated(cain$network_id),] %>%
  select(
    id = network_id,
    village = network_village,
    address = network_address,
    takeup_survey, delay, intensive, understanding
  ) %>%
  mutate(id = as.character(id)) %>%
  filter(!is.na(intensive))

peers.in.net <- unique(cain.peer.summary$id)
ids.in.survey <- unique(cai.all$id)
peers.in.net.only <- peers.in.net[!peers.in.net %in% ids.in.survey]

caic <- bind_rows(
  cai,
  cain.peer.summary[cain.peer.summary$id %in% peers.in.net.only,]
  )

head(vertex_attr(g, 'name'))
tmp.name <- vertex_attr(g, 'name')
caic$name <- caic$id
vertex_attr(g, index = as.character(caic$id)) <- as.list(caic)
vertex_attr(g, 'name') <- tmp.name
head(vertex_attr(g, 'id'))
head(vertex_attr(g, 'name'))
head(vertex_attr(g, 'village'))

g1 <- induced_subgraph(g, V(g)[which(!is.na(V(g)$intensive))])

# make adjacency matrix
A <- as_adj(g1, sparse = TRUE, names = T)
A.df <- as.data.frame(vertex_attr(g1))

A.df$intensive.0 <- ifelse(is.na(A.df$intensive), 0, A.df$intensive)
A.df$delay.0 <- ifelse(is.na(A.df$delay), 0, A.df$delay)
A.df$intensive.peers <- as.vector(A %*% A.df$intensive.0)
A.df$intensive.nond.peers <- as.vector(A %*% (A.df$intensive.0 * (1-A.df$delay.0)))
A.df$default.peers <- as.vector(A %*% A.df$default)
A.df$n.peers <- rowSums(A)
A.df$n.elig.peers <- as.vector(A %*% !is.na(A.df$intensive))

# compare my counts with the data
with(A.df, table(
  round(network_obs * network_rate_preintensive),
  intensive.nond.peers,
  useNA = "ifany"
))

with(A.df, table(
  network_obs,
  n.peers,
  useNA = "ifany"
))

with(A.df, table(
  n.peers,
  n.elig.peers
))

tmp <- merge(
  cain.s, A.df %>% select(id, intensive.nond.peers, network_obs, network_rate_preintensive))

with(tmp, table(
  round(network_obs * network_rate_preintensive),
  n.intensive.pre,
  useNA = "ifany"
))

###
# write simplified data
A.df.to.write <- A.df %>%
  select(
    id, address, region, village, takeup_survey, age, male,
    delay, intensive, info_none, intensive.nondelay.peers = intensive.nond.peers, n.peers
    )

write.table(
  A.df.to.write,
  file = "cai_data/cai.main.tsv",
  row.names = TRUE,
  sep = "\t"
)

save(A, file = "cai_data/cai.adjacency.RData")
