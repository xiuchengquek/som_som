
  
                
              









somHelper <- R6Class("somHelper", 
                     public = list(      
                       somObj =  "kohonen",
                       initialize = function(somObj){
                         if(!missing(somObj)) self$somObj <- somObj;                    
                       },                   
                       set_som = function(somObj){
                         self$somObj <- somObj                     
                       },
                       
                       size = function(){         
                         somObj <- self$somObj
                         x <- as.data.frame(table(somObj$unit.classif));
                         colnames(x) <- c('cluster', 'Freq');
                         x;
                       },
                       
                       trendLine = function(long=TRUE){
                         df_code <- as.data.frame(self$somObj$codes);
                         ## convert to numeric to preserve numeric order
                         df_code$cluster <- as.factor(as.numeric(rownames(df_code)));                         
                         
                         if(isTRUE(long)){                      
                           df_code <- melt(df_code)                        
                           colnames(df_code) <- c("cluster", "x", "value")
                         }                      
                         df_code;
                       },
                       
                       clusterValues = function(long=TRUE){                     
                         somObj<- self$somObj;
                         df_cluster <- data.frame(x = as.factor(rownames(somObj$data)), cluster = as.factor(somObj$unit.classif));                              
                         df_data <- as.data.frame(somObj$data);
                         df_cluster<- merge(df_cluster, df_data, by.x = 'x', by.y = 0);
                         
                         if(isTRUE(long)){
                           df_cluster <- melt(df_cluster)
                           ##make sure cluster is sorted properly by cluster
                         }            
                         df_cluster;                                        
                       },
                       
                       getGeneByCluster = function(cluster, long=TRUE){ 
                         if(cluster){                     
                           somObj <- self$somObj;
                           clusterData <- somObj$data[somObj$unit.classif == cluster ,]                       
                           if (isTRUE(long)){
                             clusterData<- melt(clusterData)                         
                             colnames(clusterData) <- c("gene", "x", "y")
                           }
                           clusterData;                     
                         }
                       },
                       
                       getClusterByGene = function(gene) {
                         somObj <- self$somObj;
                         data <- somObj$data;
                         cluster <- somObj$unit.classif
                         cluster <- cluster[rownames(data) %in% gene]
                         data.frame(gene = gene, cluster= cluster )
                       },
                       
                       getGene = function(gene){
                         
                         data <- as.data.frame(self$somObj$data )
                         data[rownames(data) %in% gene,]
                      
                         
                       }
                       
                     ) 
)  




somPlotHelper <- R6Class("haystackMainPlot", 
                         public = list(   
                           samples = "lists",                                                             
                           initialize = function(samples){
                             if(!missing(samples)) self$samples <- samples;                    
                           }, 
                           
                           
                           plotAllCluster = function(){
                             
                             
                             for (x in names(self$samples)) {
                               
                               print(self$plotCluster(x))
                               
                             }
                             
                             
                           },
                           
                           plotCluster = function(sampleName){                       
                             obj <- self$samples[[sampleName]]
                             p <- ggplot(obj$trendLine(TRUE), aes(x=x, y=value, group=cluster, colour='red')) + geom_line()
                             p <- p + facet_wrap(~cluster) + theme_bw()  + theme(axis.text.x = element_text(angle=90, hjust =1)) + ggtitle(label = sampleName);
                             p
                           },
                           
                           plotHeatMap = function(sampleName, clusterNo){
                             obj <- self$samples[[sampleName]]
                             df <- obj$getGeneByCluster(clusterNo, long=F)
                             pheatmap(df, cluster_cols = F, main=sampleName)
                             
                             
                             
                             
                           },
                           
                           plotGenePlot = function(geneName, long=T){
                             
                             
                             
                             if(nchar(geneName)!= 0 ){
                               
                               samples <- self$samples 
                               geneExpression <- lapply(samples, function(x) x$getGene(geneName))  
                               geneExpression <- lapply(geneExpression, function(x) melt(x))
                               geneExpression <- lapply(geneExpression, function(x) {x$timepoint<- seq(1:length(1:nrow(x))); x })
                               
                               geneExpression <- do.call(rbind, geneExpression)
                               
                               cluster <- rownames(geneExpression)
                               cluster <- strsplit(cluster, '\\.')
                               cluster <- unlist(lapply(cluster, `[[`, 1))
                               
                               
                               geneExpression$cluster <- cluster 
                               geneExpression$gene <- geneName
                               
                               print(geneExpression)
                               
                               ggplot(geneExpression, aes(x=timepoint, y=value, group=cluster, colour=cluster)) + geom_line() + ggtitle(geneName)
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                               
                             }
                             
                             
                             
                             
                           },
                           
                           
                           getSamples = function(sampleName){                         
                             self$samples[[sampleName]]
                           },
                           excludeSamples = function(sampleName){
                             
                             samples <- self$samples ;
                             samples[names(samples) != sampleName]
                             
                             
                           }
                           
                           
                         )
)    

