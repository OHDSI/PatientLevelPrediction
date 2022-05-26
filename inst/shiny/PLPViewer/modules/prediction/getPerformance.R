getResult <- function(con, tableName, performanceId, mySchema, targetDialect){
  sql <- "SELECT * FROM @my_schema.@table_name WHERE performance_id = @performance_id"
  sql <- SqlRender::render(sql = sql, 
                           my_schema = mySchema,
                           table_name = tableName,
                           performance_id = performanceId)
  sql <- SqlRender::translate(sql = sql, targetDialect =  targetDialect)
  
  result <- DatabaseConnector::dbGetQuery(conn =  con, statement = sql) 
  colnames(result) <- SqlRender::snakeCaseToCamelCase(colnames(result))
  
  if('evaluation' %in% colnames(result)){
    result$evaluation <- trimws(result$evaluation)
  }
  
  #print(tableName)
  #print(colnames(result))
  
  return(result)
}


