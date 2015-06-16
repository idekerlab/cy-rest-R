library(igraph)

library(RJSONIO)
library(httr)

port.number = 1234
base.url = paste("http://localhost:", toString(port.number), "/v1", sep="")

print(base.url)
version.url = paste(base.url, "version", sep="/")
cytoscape.version = GET(version.url)
cy.version = fromJSON(rawToChar(cytoscape.version$content))
print(cy.version)

source('toCytoscape.R')


#### 最短の代謝反応経路を探索する。

d <- read.table("data/eco_EM+TCA.txt")
g <- simplify(graph.data.frame(d,directed=T),remove.multiple=T,remove.loops=T)
g$name = "TCA"
cygraph <- toCytoscape(g)

# Send it to Cytoscape!
network.url = paste(base.url, "networks", sep="/")
res <- POST(url=network.url, body=cygraph, encode="json")
network.suid = unname(fromJSON(rawToChar(res$content)))

# D-Glucose =?=?=?=> 2-Oxoglutarate

sum <- get.all.shortest.paths(g,"D-Glucose","2-Oxoglutarate",mode="out")
V(g)[sum$res[[1]]]

E(g)$color <- "grey"
E(g,path=sum$res[[1]])$color <- "red"
E(g,path=sum$res[[1]])$width <- 3

plot(g,vertex.label=V(g)$name,vertex.size=5,layout=layout.fruchterman.reingold)


#### Essentialityとノード次数の関係を調べる
d <- read.table("ecoli_ppi_Hu_etal_2009.txt")
g <- simplify(graph.data.frame(d,directed=F),remove.multiple=T,remove.loops=T)

ess<-read.table("ecoli_proteins_essentiality_Baba2006MSB.txt",header=T)

target <- intersect(ess$gene,V(g)$name)

ess_select <- subset(ess,is.element(ess$gene,target)==T & duplicated(ess$gene)==F)
ess_select_ord <- ess_select[order(ess_select$gene),]


deg <- degree(g)
deg_select <- subset(deg,is.element(names(deg),target)==T)
deg_select_ord <- deg_select[order(names(deg_select))]

boxplot(log(deg_select_ord)~ess_select_ord$ess,ylab="log(Degree)")



#### Network motifを見つける

d <- read.table("regDB5.txt")
g0 <- simplify(graph.data.frame(d,directed=T),remove.multiple=T,remove.loops=T)

cls <- clusters(g0,"weak")
g <- delete.vertices(g0,subset(V(g0),cls$membership!=which(cls$csize==max(cls$csize))[[1]]))

c_real <- graph.motifs(g,3)

# Null modelにConfiguration modelを使う
deg_out <- degree(g,m="out")
deg_in <- degree(g,m="in")
c_rand <- data.frame()
for(i in 1:100){
g_rand <- degree.sequence.game(deg_out,deg_in)
c_rand <- rbind(c_rand,graph.motifs(g_rand,3))
}

# Null modelにEdge swiching algorithmによって生成されるRandomized networkを使う
g_unid <- delete.edges(g,subset(E(g),is.mutual(g,es=E(g))==T))
g_bid <- delete.edges(g,subset(E(g),is.mutual(g,es=E(g))==F))
g_bid_u <- as.undirected(g_bid)
c_rand <- data.frame()
for(i in 1:100){
g_rand_unid <- rewire(g_unid,niter=ecount(g_unid)*(50+log(ecount(g_unid)))/2)
g_rand_bid_u <- rewire(g_bid_u,niter=ecount(g_bid_u)*(50+log(ecount(g_bid_u)))/2)
g_rand_bid <- as.directed(g_rand_bid_u)
g_rand <- graph.union(g_rand_unid,g_rand_bid)
c_rand <- rbind(c_rand,graph.motifs(g_rand,3))
}

names(c_rand)<-0:15

(c_real-colMeans(c_rand))/sqrt(colMeans(c_rand**2)-colMeans(c_rand)**2)

sum <- graph.get.subisomorphisms.vf2(g,graph.isocreate(3,7))

V(g)[sum[[1]]+1]


#### コミュニティ抽出を行う

## Non-overlapping

# 代謝ネットワークを例として
d <- read.table("eco_EM+TCA.txt")
g <- simplify(graph.data.frame(d,directed=F),remove.multiple=T,remove.loops=T)
data <- fastgreedy.community(g)
V(g)$color <- data$membership
plot(g,vertex.size=5,vertex.label=V(g)$name,layout=layout.kamada.kawai)

dend<-as.dendrogram(data,use.modularity=T)
plot(dend)

data$membership


# PPI 抽出されたコミュニティにおけるタンパク質とその機能分類の表をresult.txtに出力する。

d <- read.table("yeast_ppi_CYGD.txt")
g <- simplify(graph.data.frame(d,directed=F),remove.multiple=T,remove.loops=T)

data <- fastgreedy.community(g)

mem <- data.frame(cbind(V(g)$name,data$membership))

funcat <- read.table("yeast_funcat_data.txt")
target<-intersect(funcat[[1]],V(g)$name)

mem_select <- subset(mem,is.element(mem[[1]],target)==T)
mem_select_ord <- mem_select[order(mem_select[[1]]),]

funcat_select <- subset(funcat,is.element(funcat[[1]],target)==T)
funcat_select_ord <- funcat_select[order(funcat_select[[1]]),]

sum <- data.frame(cbind(funcat_select_ord[1:2],mem_select_ord[1:2]))

write.table(sum,"result.txt",quote=F,col.names=F,row.names=F)


## Overlaping

library(linkcomm)

# LinkCommの場合
g <- read.table("data/eco_EM+TCA.txt")
data <- getLinkCommunities(g,hcmethod="single")
# data <- getLinkCommunities(g,directed=T,dirweight=verctor) in the case of directed networks with weighted edges
lc<-newLinkCommsAt(data,cutat=0.84)
plot(lc,type="graph")

lc$nodeclusters

# OCGの場合
oc <- getOCG.clusters(g)
plot(oc,type="graph")

oc$nodeclusters
