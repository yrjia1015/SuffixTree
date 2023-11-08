library(R6)
library(hash)
library(igraph)

Node <- R6Class("Node",
        public=list(
          suffix_node = NULL,  
          initialize = function(suffix_node = -1){
            self$suffix_node <- suffix_node
              }

        )        
)

Edge <- R6Class("Edge",
                public=list(
                  first_char_index = NULL,
                  last_char_index = NULL,
                  source_node_index = NULL,
                  dest_node_index = NULL,
                     
                  initialize = function(first_char_index,
                                        last_char_index,
                                        source_node_index,
                                        dest_node_index){
                    
                    self$first_char_index <- first_char_index
                    self$last_char_index <- last_char_index
                    self$source_node_index <- source_node_index
                    self$dest_node_index <- dest_node_index
                  },
                  
                  length = function(){
                    return (self$last_char_index - self$first_char_index)  
                  }
                  
                )        
)

Suffix <- R6Class("Suffix",
                public=list(
                  source_node_index = NULL,
                  first_char_index = NULL,
                  last_char_index = NULL,
                  
                  
                  initialize = function(source_node_index,
                                        first_char_index,
                                        last_char_index){
                    
                    self$source_node_index <- source_node_index
                    self$first_char_index <- first_char_index
                    self$last_char_index <- last_char_index
                  },
                  
                  length = function(){
                    return (self$last_char_index - self$first_char_index)  
                  },
                  
                  explicit = function(){
                    return (self$first_char_index > self$last_char_index)
                  },
                  
                  implicit = function(){
                    return(self$last_char_index >= self$first_char_index)
                  }
                  
                )        
)


SuffixTree <- R6Class("SuffixTree",
              public=list(
                        string = NULL,
                        N = NULL,
                        nodes = NULL,
                        edges = NULL,
                        active = NULL,
                         
                        initialize = function(inputstr){
                          self$string <- paste(inputstr,"#",sep = "")
                          self$N <- nchar(self$string)
                          self$nodes = c(Node$new())
                          self$edges = hash()
                          #has.key(key,hash)
                          #unname(object)

                          
                          for (i in (1:nchar(self$string))){
                            #print(i)
                            #print(self$string)
                            #print(self$N)
                            #print(self$nodes)
                            #print(self$edges)
                            #print(self$active)
                            self$add_prefix(i)
                            
                          }
                        },
                        insert_edge = function(edge){
                          key = paste(edge$source_node_index,substr(self$string,edge$first_char_index,edge$first_char_index),sep = ",")
                          .set(self$edges,key,edge)
                        },
                        
                        remove_edge = function(edge){
                          del(paste(edge$source_node_index,substr(self$string,edge$first_char_index,edge$first_char_index),sep = ","),self$edges)
                        },
                        
                        split_edge = function(edge,suffix){
                          self$nodes <- append(self$nodes,Node$new())
                          #print("+++++++++++++++")
                          e = Edge$new(edge$first_char_index,(edge$first_char_index + suffix$length())
                                       ,suffix$source_node_index,length(self$nodes))   
                          self$remove_edge(edge)
                          self$insert_edge(e)
                          self$nodes[[e$dest_node_index]]$suffix_node = suffix$source_node_index  
                          #R没有+=
                          edge$first_char_index = edge$first_char_index + suffix$length() + 1        
                          edge$source_node_index = e$dest_node_index
                          self$insert_edge(edge)
                          return (e$dest_node_index)
                        },
                        
                        canonize_suffix = function(suffix){
                          #print("-------------------------")
                          if (!(suffix$explicit())){  #!!!!!!

                            #print("+++++++++++++++")
                            e = unname(values(self$edges,keys=paste(suffix$source_node_index,substr(self$string,suffix$first_char_index,suffix$first_char_index),sep = ",")))[[1]]
                            if (e$length() <= suffix$length()){   
                              suffix$first_char_index = suffix$first_char_index + e$length() + 1 #!!!!!!
                              suffix$source_node_index = e$dest_node_index
                              self$canonize_suffix(suffix)
                            }
                          }
                        },
                        
                        add_prefix = function(last_char_index){
                          last_parent_node = 0    
                          #print(self$edges)
                          while (TRUE){
                            parent_node = self$active$source_node_index
                            #print(self$edges)
                            if (self$active$explicit()){
                              #has.key()
                              #print(keys(self$edges))
                              #print(self$edges[1])
                              #print(paste(self$active$source_node_index,substr(self$string,last_char_index,last_char_index),sep = ","))
                              if (has.key(paste(self$active$source_node_index,substr(self$string,last_char_index,last_char_index),sep = ","),self$edges)){
                                #print("************")
                                break
                              }
                            }
                            else {
                              e = unname(values(self$edges,keys=paste(self$active$source_node_index,substr(self$string,self$active$first_char_index,self$active$first_char_index),sep = ",")))[[1]]
                              if (substr(self$string,(e$first_char_index + self$active$length() + 1 ),(e$first_char_index + self$active$length() + 1)) == substr(self$string,last_char_index,last_char_index)){
                                break
                              }
                              parent_node = self$split_edge(e,self$active)
                            }
                            
                            self$nodes <-append(self$nodes,Node$new())
                            e = Edge$new(last_char_index,self$N,parent_node,length(self$nodes))#length(self$nodes)不用-1，肯定   !!!!
                            self$insert_edge(e)
                            #print(self$edges)
                            
                            if (last_parent_node > 1){  
                              self$nodes[[last_parent_node]]$suffix_node = parent_node
                            } 
                            last_parent_node = parent_node
                            
                            #print(self$active$source_node_index)
                            if (self$active$source_node_index == 1){
                              #print(self$active$first_char_index)
                              self$active$first_char_index = self$active$first_char_index + 1
                              
                            }
                            else{
                              #print(self$active)
                              self$active$source_node_index = self$nodes[[self$active$source_node_index]]$suffix_node
                            }
                            self$canonize_suffix(self$active)
                          }
                          
                          if (last_parent_node > 1){  
                            self$nodes[[last_parent_node]]$suffix_node = parent_node
                          }
                          self$active$last_char_index = self$active$last_char_index + 1
                          self$canonize_suffix(self$active)
                          self$draw()
                        },
                        
                        creat_draw_dataframe = function(){
                          Start <- c()
                          End <- c()
                          Str <- c()
                          for (edge in values(self$edges)){
                            Start <- append(Start,edge$source_node_index)
                            End <- append(End,edge$dest_node_index)
                            
                            substr <- substr(self$string,edge$first_char_index,edge$last_char_index)
                            Str <- append(Str,substr)
                          }
                          data <- data.frame(Start,End,Str)
                          return(data)
                        },
                        
                        
                        draw = function(){
                          dataframe <- self$creat_draw_dataframe()
                          graph <- graph_from_data_frame(dataframe)
                          plot(graph,layout = layout.reingold.tilford,edge.label=E(graph)$Str
                               ,edge.label.color="black",edge.label.cex=1,edge.curved=0.005,
                               edge.width = 2,edge.arrow.width = 0.5,edge.arrow.size = 0.5,asp = 0.5)
                        }
                        
              )    
              
)


test = SuffixTree$new("agctggcc")
print(test$string)
#print(length(test$nodes))
#print(test$edges)
print(test$nodes)
#test$draw()
print(test$creat_draw_dataframe())
#print(length(test$edges))
