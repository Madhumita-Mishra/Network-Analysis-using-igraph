#-----------Network analysis of nytimes data------------------#
#Useful to understand how many connections a specific word has with other words. 

# Using network terminology, 
#Our keywords are the 'nodes' in a network, which are called vertices.
#Connections are called edges. 

#install.packages("igraph")
library(igraph)

##Reading the "nytimes.csv"  data and Clean the document
nytimes<-read.csv("C:\\Users\\Documents\\Text Mining in R\\nytimes.csv")

#Converting to a dataframe only the Subject column 
subject<-data.frame(nytimes$Subject)

library(tm)
library(dplyr)

#Building a Text Corpus
#Source for the corpus

nytimes.corpus<-Corpus(VectorSource(nytimes$Subject))

#Cleaning the data
#Removing stop words
#trying to remove the http which refer to the url's

nytimes.corpus<-tm_map(nytimes.corpus,tolower)  #Converting to lowercase
nytimes.corpus<-tm_map(nytimes.corpus,stripWhitespace)  #Removing extra white space
nytimes.corpus<-tm_map(nytimes.corpus,removePunctuation ) #Removing punctuations
nytimes.corpus<-tm_map(nytimes.corpus,removeNumbers)  #Removing numbers
my_stopwords<-c(stopwords('english'),'http*')  #Removing Stopwords
nytimes.corpus<-tm_map(nytimes.corpus,removeWords,my_stopwords)

# Convert corpus to tdm
nytimes.tdm <- TermDocumentMatrix(nytimes.corpus,control=list(wordLengths=c(1, Inf)))
nytimes.tdm [1:10,1:10] 

# Remove sparse terms
nytimes.tdm.rm <- removeSparseTerms(nytimes.tdm, sparse=0.99)
nytimes.tdm.rm[1:10,1:10]

#Transform TDM transform into a matrix. 
nytimes.m<- as.matrix(nytimes.tdm.rm)

#Transform the matrix into a Boolean matrix 
#Contains 1/0.It indicates 1 for existing values other than zero.

#Therefore, we are just modifying the matrix to indicate yes or no for the terms existing in a document.

#Convert to boolean matrix
nytimes.m[nytimes.m>=1] <- 1

#Adjacency matrix : 
#It will show how many 'connections' each term has.
#This will require the product of two matrices, using the '%*%' matrix operator. 
#Through the inner product of the terms, 
#we will arrive at the number of times each term appears in a document.
################################################################################################################################################### 
#------------------------------------------------------Adjacency  Matrix------------------------------------------------------------#
#%*% is product of 2 matrices

#Build a Term Adjacency matrix

nytimes.m2 <- nytimes.m %*% t(nytimes.m)


#How many times each word has occurred with the other words
#Let's choose a subset of nytimes.m2 a 20 x 20 adjacency matrix 
nytimes.test<-nytimes.m2[1:20,1:20]
nytimes.m2[1:20,1:20]
View(nytimes.test)



#--------------------------------------------Building an adjacency graph -----------------------------------------------------------------#

#w="TRUE"
#d="undirected"

#weighted = "TRUE", mode="undirected"
#weighted = NULL, mode="undirected"
#weighted = "TRUE", mode="directed"

nytimes.m3<-nytimes.m2[1:20,1:20]
nytimes.g <- graph.adjacency(nytimes.m3,weighted="TRUE",mode="undirected")

#Remove Loops
nytimes.g<-simplify(nytimes.g)

#Vertices
V(nytimes.g)

E(nytimes.g)
#Number of edges  & vertices
ecount(nytimes.g)
vcount(nytimes.g)

#List of connections
E(nytimes.g)$weight
E(nytimes.g)$weight <- runif(ecount(nytimes.g))

# Number of Connections for each person
degree(nytimes.g,mode="out",loops = TRUE)

# set labels and degrees of vertices
V(nytimes.g)$label <- V(nytimes.g)$name
V(nytimes.g)$degree <- degree(nytimes.g)

# plot layout fruchterman.reingold
layout1 <- layout.fruchterman.reingold(nytimes.g,dim=2)

plot(nytimes.g,layout=layout1, vertex.size=2, vertex.label.color="darkred")

plot(nytimes.g,layout=layout.kamada.kawai)

#Adding more modifications

V(nytimes.g)$label.cex <- 2 * V(nytimes.g)$degree/ max(V(nytimes.g)$degree)
V(nytimes.g)$label.color <- rgb(0, 0, .2, .8)
V(nytimes.g)$label.color <- "red"
V(nytimes.g)$frame.color <- NA

#Only if weights exist
edge_weight <-  (-log(E(nytimes.g)$weight)) / max(-log(E(nytimes.g)$weight))

E(nytimes.g)$color <- rgb(0.3,0.3, 0, edge_weight)
E(nytimes.g)$width <- edge_weight

#E(nytimes.g)$color<-"grey"

# plot the graph in layout1
plot(nytimes.g, layout=layout1, vertex.color="yellow")


