# Name: Sanzida Parvin
library(UserNetR)

data( lhds )
lhds
summary(lhds)

library(igraph)
library(intergraph)

class(lhds) # check that the lhds is a network object
lhdsi = asIgraph(lhds) # converts into igraph object

# Extract data frames with node and edge informatoin
lhdsi_ver <- as_data_frame(lhdsi,what="vertices")
lhdsi_edg <- as_data_frame(lhdsi,what="edges")

#Write CSV files
write.csv(lhdsi_ver, file = "lhds_V.csv") # csv file for Vertices
write.csv(lhdsi_edg, file = "lhds_e.csv") # csv file for Edges

# Density of the original network
graph.density(lhdsi)

# Create a subset of the lhdsi network using the population greater than 40 thousand(.04 million)
lhdsi_2 <- subgraph.edges(lhdsi, V(lhdsi)[popmil > .04])
graph.density(lhdsi_2)

# Modularity
table(V(lhdsi)$hivscreen)

# community detection algorithms
cw1 <- cluster_walktrap(lhdsi)
membership(cw1)
modularity(cw1)
plot(cw1,lhdsi, main="Walktrap")

ceb1 <- cluster_edge_betweenness(lhdsi)
modularity(ceb1)
membership(ceb1)
plot(ceb1, lhdsi,main="Edge Betweenness")

cfg1 <- cluster_fast_greedy(lhdsi)
modularity(cfg1)
membership(cfg1)
plot(cfg1, lhdsi,main="Fastgreedy")

clp1 <- cluster_label_prop(lhdsi)
modularity(clp1)
membership(clp1)
plot(clp1, lhdsi, main="Label Propagation")

cle1 <- cluster_leading_eigen(lhdsi)
modularity(cle1)
membership(cle1)
plot(cle1, lhdsi,main="Leading Eigenvector")

cl1 <- cluster_louvain(lhdsi)
modularity(cl1)
membership(cl1)
plot(cl1, lhdsi,main="Cluster Louvain")

result = rbind(modularity(cw1), modularity(ceb1), modularity(cfg1),modularity(clp1), modularity(cle1),modularity(cl1))
rownames(result) <- c("cw", "ceb","cfg","clp","cle","cl")
result

table(V(lhdsi)$hivscreen,membership(cw1))

compare(as.numeric(factor(V(lhdsi)$hivscreen)),cw1,method="adjusted.rand")
compare(cw1,ceb1,method="adjusted.rand")
compare(cw1,cfg1,method="adjusted.rand")

op <- par(mfrow=c(3,2),mar=c(3,0,2,0))
plot(ceb1,lhdsi,vertex.label=V(lhdsi)$hivscreen,main="Edge Betweenness")
plot(cfg1,lhdsi,vertex.label=V(lhdsi)$hivscreen,main="Fastgreedy")
plot(clp1,lhdsi,vertex.label=V(lhdsi)$hivscreen,main="Label Propagation")
plot(cle1,lhdsi,vertex.label=V(lhdsi)$hivscreen,main="Leading Eigenvector")
plot(cw1,lhdsi,vertex.label=V(lhdsi)$hivscreen,main="Walktrap")
plot(cl1,lhdsi,vertex.label=V(lhdsi)$hivscreen,main="Cluster Louvain")
par(op)


library(ergm)
null <- ergm( lhds ~ edges )
summary( null )
nullsim <- simulate(null, verbose = TRUE,seed = 5)

popeffects <- ergm(lhds ~ edges + nodecov('popmil'))
summary(popeffects)
popsim <- simulate(popeffects, verbose = TRUE, seed = 5) #Simulations based on population effect model


diffhomophily2 <- ergm( lhds ~ edges +
                          nodecov( 'popmil' ) +
                          nodefactor( 'years' ) +
                          nodematch('hivscreen', diff=T, keep=2) +
                          nodematch('nutrition', diff=T, keep=2) +
                          nodematch('state') )

summary( diffhomophily2 )
diff2sim <- simulate(diffhomophily2, verbose = TRUE, seed = 5)

rowgof <- rbind(summary(lhds ~ edges + degree(0:5) + triangle),
                summary(nullsim ~ edges + degree(0:5) + triangle),
                summary(popsim ~ edges + degree(0:5) + triangle),
                summary(diff2sim ~ edges + degree(0:5) + triangle) )

rownames(rowgof) <- c("lhds", "Null","Main effects","Diff homophily 2")
rowgof

#Goodness-of-fit simulations for the diffhomophily2 model
diff2_gof <- gof( diffhomophily2, GOF = ~degree + espartners + dspartners, verbose = T, burnin = 10000,
                  interval = 10000, control.gof.ergm(seed = 567) )
diff2_gof

#Graphic goodness-of-fit for the diffhomophily2 network
dev.off()
par( mfrow = c( 2,3 ) )
plot( diff2_gof, cex.lab = 1.5, cex.axis = 1.5 )
plot(diff2_gof, cex.lab = 1.5, cex.axis = 1.5, plotlogodds = T )



















