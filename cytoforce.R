CytoForce <- function(nodes,links,cutoff=0.1){
  nNodes=length(nodes[,1])
  nLinks=length(links[,1])
  
  nodes$altName = c(1:nNodes)-1
  
  for (i in 1:nLinks){
    links$fromAltName[i] <- nodes$altName[as.character(nodes$nodeName) == as.character(links$fromNode[i])]
    links$toAltName[i] <- nodes$altName[as.character(nodes$nodeName) == as.character(links$toNode[i])]
  }
  
  Links = links[links$weight > cutoff,c(5,6,3)]
  names(Links) <- c("Source","Target","Value")
  nLinks2 <- length(Links[,1])
  
kon <- vector(length=nNodes)
  
  for (i in 1:nNodes){
    kon[i] <- 0
    kon[i] = sum(Links$Value[Links$Source == c(i-1)]) 
    kon[i] = kon[i] + sum(Links$Value[Links$Target == c(i-1)])
  }
kon = kon/max(kon)
  
  Nodes = nodes[,c(1,3)]
  Nodes = cbind(Nodes,kon)
  #Nodes = Nodes[,as.numeric(Nodes[,3]) > 0]
  names(Nodes) <- c("NodeID","Group","Nodesize")

  cat("Total number of nodes:",nNodes)
  cat("\nTotal Links Drawn:",nLinks2)
  
  forceNetwork(Links=Links,Nodes=Nodes,Source="Source",Target="Target",Value="Value",NodeID="NodeID",Group="Group",Nodesize = "Nodesize",radiusCalculation = "Math.pow(d.nodesize,2)*10+5",zoom=TRUE,opacityNoHover = T)
}

