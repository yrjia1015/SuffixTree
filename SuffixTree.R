library(R6)
library(hash)#用于实现python中的字典数据类型
library(igraph)

Node <- R6Class("Node",
        public=list(
          suffix_node = NULL,
      
          #initialize是初始化函数，()中可以设置初始值    
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
                  
                  #initialize是初始化函数，()中可以设置初始值    
                  initialize = function(first_char_index,
                                        last_char_index,
                                        source_node_index,
                                        dest_node_index){
                    
                    self$first_char_index <- first_char_index
                    self$last_char_index <- last_char_index
                    self$source_node_index <- source_node_index
                    self$dest_node_index <- dest_node_index
                  },
                  
                  #定义一个内部函数，如果内部函数不是类的最后一个的话，需要在函数结尾加一个,
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
                  
                  
                  #initialize是初始化函数，()中可以设置初始值    
                  initialize = function(source_node_index,
                                        first_char_index,
                                        last_char_index){
                    
                    self$source_node_index <- source_node_index
                    self$first_char_index <- first_char_index
                    self$last_char_index <- last_char_index
                  },
                  
                  #定义一个内部函数，如果内部函数不是类的最后一个的话，需要在函数结尾加一个,
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
                        
                        #initialize是初始化函数，()中可以设置初始值    
                        initialize = function(inputstr){
                          self$string <- paste(inputstr,"#",sep = "")
                          #字符向量长度计算函数length(),其返回字符向量的长度，而非字符串中字符的长度。要用nchar()
                          self$N <- nchar(self$string) #python中需要-1，不知道这里需不需要，R的索引从1开始
                          self$nodes = c(Node$new())
                          self$edges = hash()#初始化一个键值对 用.set(self$edges,key=,values=)来赋值
                          #has.key(key,hash)用来查找hash对象中是否指定key的键值对，返回布尔值
                          #unname(object)可以去掉向量中元素的名称 ,需要取self$edges中
                          #某个特定键“a”,对应的值时，用unname(values(p,keys="a"))[[1]],这个命令可以准确定位到要找的那个实例化对象，需要什么属性再拿就好了
                          
                          #这个Suffix$new是不是该从（1,1,-1）改成(1，1，0)
                          self$active = Suffix$new(1,1,0)#与python有差别是因为R的索引从1开始，第二个参数是first_char_index，
                          #R语言中的range()函数返回的是最大值和最小值
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
                          #.set用于向hash表添加键值对，
                          #key值必须是字符串所以用字符串连接函数paste()和字符串截取函数substr将source_node_index与子串用“，”连接成字符串作为键
                          #value值为传入的edge
                          key = paste(edge$source_node_index,substr(self$string,edge$first_char_index,edge$first_char_index),sep = ",")
                          .set(self$edges,key,edge)
                        },
                        
                        remove_edge = function(edge){
                          #del(x,hash)可用来在hash对象中删除键为x的键值对,paste()函数中是指定的键
                          del(paste(edge$source_node_index,substr(self$string,edge$first_char_index,edge$first_char_index),sep = ","),self$edges)
                        },
                        
                        split_edge = function(edge,suffix){
                          #实例化对象用：对象名$new()
                          self$nodes <- append(self$nodes,Node$new())
                          #print("+++++++++++++++")
                          e = Edge$new(edge$first_char_index,(edge$first_char_index + suffix$length())
                                       ,suffix$source_node_index,length(self$nodes))    #length(self$nodes)-1不知道这里需不需要-1，现在没减一，应该不用，因为R以1开始索引嘛
                          #类内调用自己类内的方法的时候，需要在方法名称前面加self$
                          self$remove_edge(edge)
                          #类内调用自己类内的方法的时候，需要在方法名称前面加self$
                          self$insert_edge(e)
                          #访问向量中的类时索引外要加上[[]]才能访问得到类中的属性
                          self$nodes[[e$dest_node_index]]$suffix_node = suffix$source_node_index  #这句话应该没问题!!!  
                          #R没有+=
                          edge$first_char_index = edge$first_char_index + suffix$length() + 1        #+++++++++++这个地方本来是有一个+1的
                          edge$source_node_index = e$dest_node_index
                          #类内调用自己类内的方法的时候，需要在方法名称前面加self$
                          self$insert_edge(edge)
                          return (e$dest_node_index)
                        },
                        
                        canonize_suffix = function(suffix){
                          #print("-------------------------")
                          if (!(suffix$explicit())){  #!!!!!!
                            #取edges中特定键的值（值为一个边）就只能这样
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
                          last_parent_node = 0    #这是个可疑点,我觉得不该是-1，该是0
                          #print(self$edges)
                          while (TRUE){
                            parent_node = self$active$source_node_index
                            #print(self$edges)
                            if (self$active$explicit()){
                              #has.key()用于判断以特定字符串为键的键值对在不在hash对象中
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
                              #不知道这里的+1对不对 !!!!!!!  ++++++++++++++++++++这个if的+1一定是要加的，因为这是算的偏移量
                              if (substr(self$string,(e$first_char_index + self$active$length() + 1 ),(e$first_char_index + self$active$length() + 1)) == substr(self$string,last_char_index,last_char_index)){
                                break
                              }
                              parent_node = self$split_edge(e,self$active)
                            }
                            
                            self$nodes <-append(self$nodes,Node$new())
                            e = Edge$new(last_char_index,self$N,parent_node,length(self$nodes))#length(self$nodes)不用-1，肯定   !!!!
                            self$insert_edge(e)
                            #print(self$edges)
                            
                            if (last_parent_node > 1){   #+++++++++++++++++这个地方我觉得应该是1而不是0
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
                          
                          if (last_parent_node > 1){  #+++++++++++++++++这个地方我觉得应该是1而不是0
                            self$nodes[[last_parent_node]]$suffix_node = parent_node
                          }
                          self$active$last_char_index = self$active$last_char_index + 1
                          self$canonize_suffix(self$active)
                          #后面如果没问题的话再这注释下面加可视化
                          self$draw()
                        },
                        
                        creat_draw_dataframe = function(){
                          Start <- c()
                          End <- c()
                          Str <- c()
                          #从hash类中提取每个键对应的Edge类（用values()），放在向量中
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
