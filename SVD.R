
x <- matrix(c(1,0,0,0,0,0,0,4,0,3,0,0,0,0,0,0,2,0,0),byrow=FALSE,nrow=4,ncol=5)
# it is filled by columns, 5 columns 4 rows

# svd is split into 3 different matrices
# u,v are orthonormal (orthonormal is orthogonal that menas they are perpendicular and unit lenght or length =1)
svdObj <- svd(x)
svdObj$u
svdObj$d
svdObj$v

# on the concept of PCA we find eigen vectors and ?? - eigen value
# C - Covariance matrix, V - eigen vector, ?? - eigen value
# CV = ??V
# EIGRN VECTOR WILL NOT CHANGE THE DIRECTION IT REMAINS CONSTANT
# If direction is chnage that means it's not eigen vector


svdObj$u%*%diag(svdObj$d)%*%t(svdObj$v) # diag-diagonal, t-transpose
# if you multiply we will get same original size matrix
x ## compare the above output with x, It's same to original matrix

## The "Romeo and Juliet" example
# we taken TDM in matrix format
A <- matrix(c(1,0,1,0,0,
              1,1,0,0,0,
              0,1,0,0,0,
              0,1,1,0,0,
              0,0,0,1,0,
              0,0,1,1,0,
              0,0,0,1,0,
              0,0,0,1,1),byrow=TRUE,nrow=8,ncol=5)
colnames(A) <- c("d1","d2","d3","d4","d5")
rownames(A) <- c("romeo","juliet","happy","dagger","live","die","Free","New Hampshire")
A

svdObj <- svd(A)
U <- svdObj$u # Terms to topics matrix
U
S <- diag(svdObj$d) # diagonal matrix
S
V <- svdObj$v # topic to documents matrix
t(V)

## going with Rank-2 approximation
U2 <- U[,1:2] # first 2 columns of U
U2
S2 <- S[1:2,1:2] # first 2 columns and 2 rows of S
S2
VT2 <- t(V[,1:2]) #first two rows of V_Transpose 
VT2

#enumerating the words
terms <- U2 %*% S2 # when you do matrix multiplication use this symbol %*%
terms

#enumerating the documents
docs <- S2 %*% VT2
docs

# we are using search query here, Search Query:{dies, dagger}

query <- (terms[4,]+terms[6,])/2 # see in excel data, dagger is in 4th number and die is in 6th number.
query  #average of both the terms to get weightage

#ranks of documnets to the query
#creating ranks matrix
ranks <- matrix(nrow=5,ncol=1)
for(i in 1:5)ranks[i] <- sum(docs[,i]*query) / (sqrt(sum(docs[,i] * docs[,i])) * sqrt(sum(query * query))) #cosine based similarity equation
# find similarity between documents and query (dies & dagger) , Multiply documents with query this gives ranks
ranks
# by looking in this ranks what can you say -  USE EXCEL SHEET TO COMPARE & FIND THE RESULT
# d3 Romeo died by dagger. - is the highest value - 0.9844360
# d1 Romeo and Juliet - is the 2nd highest value - 0.7727965
# Romeo and Juliet is similar to dies and dagger so you can show this Romeo and Juliet in SEO
# Publist the document which is more similar
