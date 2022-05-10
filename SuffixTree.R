library(R6)
library(hash)#����ʵ��python�е��ֵ���������
library(igraph)

Node <- R6Class("Node",
        public=list(
          suffix_node = NULL,
      
          #initialize�ǳ�ʼ��������()�п������ó�ʼֵ    
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
                  
                  #initialize�ǳ�ʼ��������()�п������ó�ʼֵ    
                  initialize = function(first_char_index,
                                        last_char_index,
                                        source_node_index,
                                        dest_node_index){
                    
                    self$first_char_index <- first_char_index
                    self$last_char_index <- last_char_index
                    self$source_node_index <- source_node_index
                    self$dest_node_index <- dest_node_index
                  },
                  
                  #����һ���ڲ�����������ڲ���������������һ���Ļ�����Ҫ�ں�����β��һ��,
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
                  
                  
                  #initialize�ǳ�ʼ��������()�п������ó�ʼֵ    
                  initialize = function(source_node_index,
                                        first_char_index,
                                        last_char_index){
                    
                    self$source_node_index <- source_node_index
                    self$first_char_index <- first_char_index
                    self$last_char_index <- last_char_index
                  },
                  
                  #����һ���ڲ�����������ڲ���������������һ���Ļ�����Ҫ�ں�����β��һ��,
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
                        
                        #initialize�ǳ�ʼ��������()�п������ó�ʼֵ    
                        initialize = function(inputstr){
                          self$string <- paste(inputstr,"#",sep = "")
                          #�ַ��������ȼ��㺯��length(),�䷵���ַ������ĳ��ȣ������ַ������ַ��ĳ��ȡ�Ҫ��nchar()
                          self$N <- nchar(self$string) #python����Ҫ-1����֪�������費��Ҫ��R��������1��ʼ
                          self$nodes = c(Node$new())
                          self$edges = hash()#��ʼ��һ����ֵ�� ��.set(self$edges,key=,values=)����ֵ
                          #has.key(key,hash)��������hash�������Ƿ�ָ��key�ļ�ֵ�ԣ����ز���ֵ
                          #unname(object)����ȥ��������Ԫ�ص����� ,��Ҫȡself$edges��
                          #ĳ���ض�����a��,��Ӧ��ֵʱ����unname(values(p,keys="a"))[[1]],����������׼ȷ��λ��Ҫ�ҵ��Ǹ�ʵ����������Ҫʲô�������þͺ���
                          
                          #���Suffix$new�ǲ��Ǹôӣ�1,1,-1���ĳ�(1��1��0)
                          self$active = Suffix$new(1,1,0)#��python�в������ΪR��������1��ʼ���ڶ���������first_char_index��
                          #R�����е�range()�������ص������ֵ����Сֵ
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
                          #.set������hash�����Ӽ�ֵ�ԣ�
                          #keyֵ�������ַ����������ַ������Ӻ���paste()���ַ�����ȡ����substr��source_node_index���Ӵ��á��������ӳ��ַ�����Ϊ��
                          #valueֵΪ�����edge
                          key = paste(edge$source_node_index,substr(self$string,edge$first_char_index,edge$first_char_index),sep = ",")
                          .set(self$edges,key,edge)
                        },
                        
                        remove_edge = function(edge){
                          #del(x,hash)��������hash������ɾ����Ϊx�ļ�ֵ��,paste()��������ָ���ļ�
                          del(paste(edge$source_node_index,substr(self$string,edge$first_char_index,edge$first_char_index),sep = ","),self$edges)
                        },
                        
                        split_edge = function(edge,suffix){
                          #ʵ���������ã�������$new()
                          self$nodes <- append(self$nodes,Node$new())
                          #print("+++++++++++++++")
                          e = Edge$new(edge$first_char_index,(edge$first_char_index + suffix$length())
                                       ,suffix$source_node_index,length(self$nodes))    #length(self$nodes)-1��֪�������費��Ҫ-1������û��һ��Ӧ�ò��ã���ΪR��1��ʼ������
                          #���ڵ����Լ����ڵķ�����ʱ����Ҫ�ڷ�������ǰ���self$
                          self$remove_edge(edge)
                          #���ڵ����Լ����ڵķ�����ʱ����Ҫ�ڷ�������ǰ���self$
                          self$insert_edge(e)
                          #���������е���ʱ������Ҫ����[[]]���ܷ��ʵõ����е�����
                          self$nodes[[e$dest_node_index]]$suffix_node = suffix$source_node_index  #��仰Ӧ��û����!!!  
                          #Rû��+=
                          edge$first_char_index = edge$first_char_index + suffix$length() + 1        #+++++++++++����ط���������һ��+1��
                          edge$source_node_index = e$dest_node_index
                          #���ڵ����Լ����ڵķ�����ʱ����Ҫ�ڷ�������ǰ���self$
                          self$insert_edge(edge)
                          return (e$dest_node_index)
                        },
                        
                        canonize_suffix = function(suffix){
                          #print("-------------------------")
                          if (!(suffix$explicit())){  #!!!!!!
                            #ȡedges���ض�����ֵ��ֵΪһ���ߣ���ֻ������
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
                          last_parent_node = 0    #���Ǹ����ɵ�,�Ҿ��ò�����-1������0
                          #print(self$edges)
                          while (TRUE){
                            parent_node = self$active$source_node_index
                            #print(self$edges)
                            if (self$active$explicit()){
                              #has.key()�����ж����ض��ַ���Ϊ���ļ�ֵ���ڲ���hash������
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
                              #��֪�������+1�Բ��� !!!!!!!  ++++++++++++++++++++���if��+1һ����Ҫ�ӵģ���Ϊ�������ƫ����
                              if (substr(self$string,(e$first_char_index + self$active$length() + 1 ),(e$first_char_index + self$active$length() + 1)) == substr(self$string,last_char_index,last_char_index)){
                                break
                              }
                              parent_node = self$split_edge(e,self$active)
                            }
                            
                            self$nodes <-append(self$nodes,Node$new())
                            e = Edge$new(last_char_index,self$N,parent_node,length(self$nodes))#length(self$nodes)����-1���϶�   !!!!
                            self$insert_edge(e)
                            #print(self$edges)
                            
                            if (last_parent_node > 1){   #+++++++++++++++++����ط��Ҿ���Ӧ����1������0
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
                          
                          if (last_parent_node > 1){  #+++++++++++++++++����ط��Ҿ���Ӧ����1������0
                            self$nodes[[last_parent_node]]$suffix_node = parent_node
                          }
                          self$active$last_char_index = self$active$last_char_index + 1
                          self$canonize_suffix(self$active)
                          #�������û����Ļ�����ע������ӿ��ӻ�
                          self$draw()
                        },
                        
                        creat_draw_dataframe = function(){
                          Start <- c()
                          End <- c()
                          Str <- c()
                          #��hash������ȡÿ������Ӧ��Edge�ࣨ��values()��������������
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