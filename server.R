library(shiny)
library(arules)
library(arulesViz)
library(DBI)
library(DT)
library(data.table)

sqlQuery <- function (query) {
  # Connection to db
  conn <- dbConnect(
    RMySQL::MySQL(),
    dbname = "super_data",
    host = "34.87.121.216",
    user = "root",
    password = "superdata123"
  )
  # Close db connection after function call exits
  on.exit(dbDisconnect(conn))
  
  rs <- dbGetQuery(conn, query)
  return(rs)
}

shinyServer(function(input, output) {
  # Reading tables orders and products from sql db
  orders <-
    sqlQuery(
      "SELECT TRANSACTIONID, orders.PDTID, PDTNAME, USERID, ORDERQTY, PMENTTYPE, NETPRICE, PCHASEDATE FROM orders JOIN product ON orders.PDTID = product.PDTID"
    )
  orders$PRODUCT <- paste(orders$PDTID, orders$PDTNAME)
  
  # Create transactions table for market basket using PDTID or any other specified columns
  trans <-
    as(split(orders[, "PRODUCT"], orders[, "TRANSACTIONID"]), "transactions")
  
  #==========================================Selectize List =============================================
  output$productSelector <- renderUI({
    selectizeInput(
      'select_product',
      "Search Product",
      choices = unique(orders$PRODUCT),
      multiple = TRUE,
      options = list(
        maxItems = 1,
        maxOptions = 3,
        icon = c("glyphicon-search")
      )
    )
    
  })
  
  #========================================= Output Table / Visualizations ============================================
  output$rules_table <- DT::renderDataTable({
    general_rules <- rules()
    
    rules_table <-
      data.table(lhs = labels(lhs(general_rules)),
                 rhs = (labels(rhs(general_rules))),
                 quality(general_rules))
    rules_table
  })
  
  output$graph_scattered <- renderPlot({
    general_rules <- rules()
    
    plot(general_rules, method = "scatterplot")
  }, height = 600, width = 600)
  
  output$graph_network <- renderPlot({
    general_rules <- rules()
    
    plot(general_rules, method = "graph")
  }, height = 600, width = 600)
  
  output$data <- DT::renderDataTable({
    orders
  })
  
  #========================================= Get Arules ============================================
  
  rules <- function() {
    if (!is.null(input$select_product)) {
      # Put single item on lhs Note: need adjust to much lower params
      general_rules <-
        apriori(
          trans,
          parameter = list(
            supp = input$supp,
            conf = input$conf,
            minlen = 2
          ),
          appearance = list(default = "rhs", lhs = input$select_product)
        )
    } else {
      # Generate rules for all products
      general_rules <-
        apriori(trans,
                parameter = list(
                  supp = input$supp,
                  conf = input$conf,
                  minlen = 2
                ))
    }
    
    if (input$rr == TRUE) {
      # Remove any redundant rules
      redundant_rules <- is.redundant(general_rules)
      general_rules <- general_rules[!redundant_rules]
    }
    
    return (general_rules)
  }
  
  #========================================= Refresh Button ============================================
  
  observeEvent(input$refreshButton, {
    if (is.null(input$select_product)) {
      #=========================== Generate recommendations for ALL PRODUCTS ===============================
      print ("All Products: Generating Top 12 Product Recommendations...")
      
      # Truncate table before inserting
      sqlQuery("TRUNCATE TABLE product_recommendation;")
      
      # Read all product id from orders
      productList <- as.data.frame(unique(orders$PRODUCT))
      
      # Connection to db
      conn <- dbConnect(RMySQL::MySQL(), 
                        dbname="super_data", 
                        host="34.87.121.216", 
                        user="root", 
                        password="superdata123")
      
      # Close db connection after function call exits
      on.exit(dbDisconnect(conn))
      
      withProgress(message = 'Generating: ', value = 0, {
        n <- nrow(productList)
        
        for (row in 1:n) {
          # Increment the progress bar
          incProgress(1 / n, detail = paste(row, "/", n))
          
          # Get the product id
          product <- as.character(productList[row, "unique(orders$PRODUCT)"])
          pdtid <- strsplit(product, ' ')[[1]][1]
          
          # Put single item on lhs to generate rules
          product_rules <-
            apriori(
              trans,
              parameter = list(
                supp = 0.00001,
                conf = 0.01,
                minlen = 2
              ),
              appearance = list(default = "rhs", lhs = product)
            )
          
          # Getting top 12 rules based on confidence -> to be used in product recommendation
          product_rules <-
            head(product_rules, n = 12, by = "confidence")
          
          # Transform top 12 rules as data frame
          top12 <- as(product_rules, "data.frame")
          top12_rhs <- ""
          
          if (dim(top12)[2] != 0) {
            # Process each row of top 12 rules
            for (row in 1:nrow(top12)) {
              rhs <- as.character(top12[row, "rules"])
              
              # Format rule as character, split with delim ' => ', remove first and last char
              rhs <- strsplit(rhs, " => ")[[1]][2]
              rhs <- substr(rhs, 2, nchar(rhs) - 1)
              rhs <- strsplit(rhs, ' ')[[1]][1]
              
              top12_rhs <- paste(top12_rhs, rhs, sep = " ")
            }
            
            # SQL statement to insert product recommendations
            sql <-
              "INSERT INTO product_recommendation(PDTID, RECOMMENDATIONS) VALUES('"
            sql <- paste(sql, pdtid, sep = "")
            sql <- paste(sql, "', '", sep = "")
            sql <- paste(sql, top12_rhs, sep = "")
            sql <- paste(sql, "');", sep = "")
            print (sql)
            
            dbGetQuery(conn, sql)
          }
          
        }
        # End of global rules
        
      })
      
      showModal(
        modalDialog(
          title = "Refresh Recommendations",
          paste0("Generated product recommendations for all products!"),
          easyClose = TRUE,
          footer = NULL
        )
      )
      
    } else {
      # ==================== Generate and publish recommendations for one product ========================
      print ("Single Product: Generating Top 12 Product Recommendations...")
      
      
      withProgress(message = 'Generating: ', value = 0, {
        # Increment the progress bar
        incProgress(1 / 15, detail = paste(1, "/", 15))
        
        # Get the product id
        pdtid <- strsplit(input$select_product, ' ')[[1]][1]
        
        # Delete from table before inserting
        sql <- "DELETE FROM product_recommendation WHERE PDTID='"
        sql <- paste(sql, pdtid, sep = "")
        sql <- paste(sql, "';", sep = "")
        sqlQuery(sql)
        
        # Put single item on lhs to generate rules
        product_rules <-
          apriori(
            trans,
            parameter = list(
              supp = 0.00001,
              conf = 0.01,
              minlen = 2
            ),
            appearance = list(default = "rhs", lhs = input$select_product)
          )
        
        # Getting top 12 rules based on confidence -> to be used in product recommendation
        product_rules <-
          head(product_rules, n = 12, by = "confidence")
        
        # Increment the progress bar
        incProgress(2 / 15, detail = paste(3, "/", 15))
        
        # Transform top 12 rules as data frame
        top12 <- as(product_rules, "data.frame")
        top12_rhs <- ""
        
        if (dim(top12)[2] != 0) {
          # Process each row of top 12 rules
          for (row in 1:nrow(top12)) {
            # Increment the progress bar
            incProgress(1 / 15, detail = paste(3+row, "/", 15))
            rhs <- as.character(top12[row, "rules"])
            
            # Format rule as character, split with delim ' => ', remove first and last char
            rhs <- strsplit(rhs, " => ")[[1]][2]
            rhs <- substr(rhs, 2, nchar(rhs) - 1)
            rhs <- strsplit(rhs, ' ')[[1]][1]
            
            top12_rhs <- paste(top12_rhs, rhs, sep = " ")
          }
          
          # SQL statement to insert product recommendations
          sql <-
            "INSERT INTO product_recommendation(PDTID, RECOMMENDATIONS) VALUES('"
          sql <- paste(sql, pdtid, sep = "")
          sql <- paste(sql, "', '", sep = "")
          sql <- paste(sql, top12_rhs, sep = "")
          sql <- paste(sql, "');", sep = "")
          print (sql)
          
          sqlQuery(sql)
        }
        
      })
      
      showModal(
        modalDialog(
          title = "Refresh Recommendations",
          paste0(
            "Generated product recommendations for ",
            input$select_product,
            "!"
          ),
          easyClose = TRUE,
          footer = NULL
        )
      )
      
    }
    
    
  })
})
