#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(dplyr)

options(shiny.legacy.datatable = FALSE)
options(shiny.maxRequestSize = 100*1024^2)
options(shiny.launch.browser = TRUE)

rm(list = ls())

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "E-Commerce Analytics Dashboard"),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Product Analysis", tabName = "products", icon = icon("box")),
      menuItem("Payment Analysis", tabName = "payments", icon = icon("credit-card")),
      menuItem("Customer Geography", tabName = "geography", icon = icon("globe")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    
    br(),
    h4("Filters", style = "padding-left: 20px; color: #2c3e50;"),
    
    dateRangeInput(
      "date_range",
      "Order Date Range:",
      start = "2017-01-01",
      end = "2018-08-31"
    ),
    
    selectInput(
      "status_filter",
      "Order Status:",
      choices = c("All", "delivered", "shipped", "canceled", "unavailable"),
      selected = "All"
    ),
    
    selectInput(
      "state_filter",
      "Customer State:",
      choices = NULL,
      multiple = TRUE,
      selected = NULL
    ),
    
    sliderInput(
      "price_filter",
      "Price Range (R$):",
      min = 0, max = 1000,
      value = c(0, 500)
    ),
    
    actionButton(
      "apply_filters",
      "Apply Filters",
      icon = icon("filter"),
      width = "90%",
      style = "background-color: #3498db; color: white; margin-left: 5%; margin-top: 20px;"
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box { border-radius: 10px; }
        .box { border-radius: 8px; }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_orders_box", width = 3),
          valueBoxOutput("total_revenue_box", width = 3),
          valueBoxOutput("avg_order_value_box", width = 3),
          valueBoxOutput("delivery_rate_box", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Revenue & Orders Trend",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("revenue_trend_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Top Product Categories",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("top_categories_plot")
          ),
          
          box(
            title = "Orders by State",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("state_orders_plot")
          )
        )
      ),
      
      tabItem(
        tabName = "products",
        fluidRow(
          box(
            title = "Product Price Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("price_distribution_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Category Performance",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("category_performance_plot")
          ),
          
          box(
            title = "Shipping Cost Analysis",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("shipping_analysis_plot")
          )
        )
      ),
      
      tabItem(
        tabName = "payments",
        fluidRow(
          box(
            title = "Payment Method Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("payment_distribution_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Installments Analysis",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("installments_plot")
          ),
          
          box(
            title = "Payment Value Trends",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("payment_trends_plot")
          )
        )
      ),
      
      tabItem(
        tabName = "geography",
        fluidRow(
          box(
            title = "Top Brazilian States Analysis",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("state_metrics_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Top Cities",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("top_cities_plot")
          ),
          
          box(
            title = "Regional Performance",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("regional_plot")
          )
        )
      ),
      
      tabItem(
        tabName = "data",
        box(
          title = "Data Explorer",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          dataTableOutput("data_explorer_table"),
          br(),
          downloadButton("download_data", "Download Data", 
                         style = "background-color: #3498db; color: white;")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  base_data <- reactive({
    showNotification("Loading data...", type = "message", duration = 3)
    
    Customers <- read.csv("data/df_Customers copy.csv")
    OrderItems <- read.csv("data/df_OrderItems copy.csv")
    Orders <- read.csv("data/df_Orders copy.csv")
    Payments <- read.csv("data/df_Payments copy.csv")
    Products <- read.csv("data/df_Products copy.csv")
    
    Orders <- Orders %>%
      mutate(
        order_date = as.Date(order_purchase_timestamp),
        order_year = year(order_date),
        order_month = month(order_date, label = TRUE)
      )
    
    Payments_agg <- Payments %>%
      group_by(order_id) %>%
      summarise(
        total_payment = sum(payment_value, na.rm = TRUE),
        payment_types = paste(unique(payment_type), collapse = ", "),
        avg_installments = mean(payment_installments, na.rm = TRUE)
      )
    
    merged_data <- Orders %>%
      left_join(Customers, by = "customer_id") %>%
      left_join(OrderItems, by = "order_id") %>%
      left_join(Payments_agg, by = "order_id") %>%
      left_join(Products, by = "product_id", relationship = "many-to-many") %>%
      distinct()
    
    return(merged_data)
  })
  
  observe({
    df <- base_data()
    states <- sort(unique(df$customer_state))
    updateSelectInput(session, "state_filter",
                      choices = c("All States", states),
                      selected = "All States")
  })
  
  filtered_data <- eventReactive(input$apply_filters, {
    req(base_data())
    req(input$date_range)
    
    df <- base_data() %>%
      filter(
        order_date >= input$date_range[1] &
          order_date <= input$date_range[2]
      )
    
    if ("price" %in% names(df)) {
      df <- df %>%
        filter(
          price >= input$price_filter[1] &
            price <= input$price_filter[2]
        )
    }
    
    if (input$status_filter != "All") {
      df <- df %>% filter(order_status == input$status_filter)
    }
    
    if (!is.null(input$state_filter) && !"All States" %in% input$state_filter) {
      df <- df %>% filter(customer_state %in% input$state_filter)
    }
    
    return(df)
  })
  
  output$total_orders_box <- renderValueBox({
    df <- filtered_data()
    req(df)
    valueBox(
      format(n_distinct(df$order_id), big.mark = ","),
      "Total Orders",
      icon = icon("shopping-cart"),
      color = "green"
    )
  })
  
  output$total_revenue_box <- renderValueBox({
    df <- filtered_data()
    req(df)
    revenue <- sum(df$total_payment, na.rm = TRUE)
    valueBox(
      paste0("R$", format(round(revenue), big.mark = ",")),
      "Total Revenue",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })
  
  output$avg_order_value_box <- renderValueBox({
    df <- filtered_data()
    req(df)
    avg_value <- df %>%
      distinct(order_id, total_payment) %>%
      summarise(avg = mean(total_payment, na.rm = TRUE)) %>%
      pull(avg)
    valueBox(
      paste0("R$", format(round(avg_value, 2), nsmall = 2)),
      "Avg Order Value",
      icon = icon("calculator"),
      color = "yellow"
    )
  })
  
  output$delivery_rate_box <- renderValueBox({
    df <- filtered_data()
    req(df)
    delivered <- sum(df$order_status == "delivered", na.rm = TRUE)
    total <- nrow(df)
    rate <- ifelse(total > 0, round(delivered/total * 100, 1), 0)
    valueBox(
      paste0(rate, "%"),
      "Delivery Rate",
      icon = icon("truck"),
      color = "orange"
    )
  })
  
  output$revenue_trend_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    daily_summary <- df %>%
      group_by(order_date) %>%
      summarise(
        revenue = sum(total_payment, na.rm = TRUE),
        orders = n_distinct(order_id)
      )
    
    plot_ly(daily_summary, x = ~order_date) %>%
      add_trace(y = ~revenue, name = "Revenue", 
                type = 'scatter', mode = 'lines',
                line = list(color = '#3498db', width = 3)) %>%
      add_trace(y = ~orders, name = "Orders", 
                type = 'bar', 
                marker = list(color = 'rgba(46, 204, 113, 0.3)'),
                yaxis = "y2") %>%
      layout(
        title = "Revenue & Orders Trend Over Time",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Revenue (R$)"),
        yaxis2 = list(title = "Orders", side = "right", overlaying = "y"),
        hovermode = 'x unified'
      )
  })
  
  output$top_categories_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    top_cats <- df %>%
      group_by(product_category_name) %>%
      summarise(revenue = sum(total_payment, na.rm = TRUE)) %>%
      filter(!is.na(product_category_name)) %>%
      arrange(desc(revenue)) %>%
      head(10)
    
    plot_ly(top_cats, 
            x = ~revenue, 
            y = ~reorder(product_category_name, revenue),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#3498db')) %>%
      layout(
        title = "Top Product Categories by Revenue",
        xaxis = list(title = "Revenue (R$)"),
        yaxis = list(title = "")
      )
  })
  
  output$state_orders_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    state_summary <- df %>%
      group_by(customer_state) %>%
      summarise(orders = n_distinct(order_id)) %>%
      arrange(desc(orders)) %>%
      head(10)
    
    plot_ly(state_summary,
            x = ~reorder(customer_state, orders),
            y = ~orders,
            type = 'bar',
            marker = list(color = '#2ecc71')) %>%
      layout(
        title = "Top States by Orders",
        xaxis = list(title = "State", tickangle = -45),
        yaxis = list(title = "Orders")
      )
  })
  
  output$price_distribution_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    df_filtered <- df %>%
      filter(price > 0 & price < 500)
    
    plot_ly(df_filtered,
            x = ~price,
            type = 'histogram',
            nbinsx = 50,
            marker = list(color = '#9b59b6')) %>%
      layout(
        title = "Product Price Distribution",
        xaxis = list(title = "Price (R$)"),
        yaxis = list(title = "Count")
      )
  })
  
  output$category_performance_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    cat_perf <- df %>%
      group_by(product_category_name) %>%
      summarise(
        avg_price = mean(price, na.rm = TRUE),
        total_orders = n_distinct(order_id)
      ) %>%
      filter(!is.na(product_category_name)) %>%
      arrange(desc(total_orders)) %>%
      head(10)
    
    plot_ly(cat_perf,
            x = ~reorder(product_category_name, total_orders),
            y = ~total_orders,
            type = 'bar',
            marker = list(color = '#3498db')) %>%
      layout(
        title = "Category Performance by Order Count",
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Number of Orders")
      )
  })
  
  output$shipping_analysis_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    if ("shipping_charges" %in% names(df)) {
      shipping_summary <- df %>%
        filter(shipping_charges > 0 & shipping_charges < 100) %>%
        group_by(product_category_name) %>%
        summarise(avg_shipping = mean(shipping_charges, na.rm = TRUE)) %>%
        filter(!is.na(product_category_name)) %>%
        arrange(desc(avg_shipping)) %>%
        head(10)
      
      plot_ly(shipping_summary,
              x = ~reorder(product_category_name, avg_shipping),
              y = ~avg_shipping,
              type = 'bar',
              marker = list(color = '#e74c3c')) %>%
        layout(
          title = "Shipping Cost Analysis by Category",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Avg Shipping Cost (R$)")
        )
    } else {
      plot_ly(type = 'scatter', mode = 'text') %>%
        add_text(x = 0.5, y = 0.5, text = "Shipping data not available") %>%
        layout(title = "Shipping Analysis")
    }
  })
  
  output$payment_distribution_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    payment_summary <- df %>%
      group_by(payment_types) %>%
      summarise(value = sum(total_payment, na.rm = TRUE)) %>%
      filter(!is.na(payment_types) & payment_types != "")
    
    plot_ly(payment_summary,
            labels = ~payment_types,
            values = ~value,
            type = 'pie',
            hole = 0.4,
            textinfo = 'label+percent',
            marker = list(colors = c('#3498db', '#2ecc71', '#e74c3c', '#f39c12'))) %>%
      layout(title = "Payment Method Distribution by Value")
  })
  
  output$installments_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    installments_data <- df %>%
      filter(!is.na(avg_installments) & avg_installments > 0) %>%
      group_by(installment_group = round(avg_installments)) %>%
      summarise(
        avg_payment = mean(total_payment, na.rm = TRUE),
        count = n()
      )
    
    plot_ly(installments_data,
            x = ~installment_group,
            y = ~avg_payment,
            type = 'bar',
            marker = list(color = '#f39c12'),
            text = ~paste("Orders:", count),
            hoverinfo = 'text') %>%
      layout(
        title = "Installments Analysis - Average Payment by Number of Installments",
        xaxis = list(title = "Number of Installments"),
        yaxis = list(title = "Average Payment (R$)")
      )
  })
  
  output$payment_trends_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    monthly_trends <- df %>%
      mutate(month = floor_date(order_date, "month")) %>%
      group_by(month) %>%
      summarise(
        total_payments = sum(total_payment, na.rm = TRUE),
        avg_payment = mean(total_payment, na.rm = TRUE)
      )
    
    plot_ly(monthly_trends, x = ~month) %>%
      add_trace(y = ~total_payments, name = "Total Payments",
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#3498db', width = 2)) %>%
      layout(
        title = "Payment Value Trends Over Time",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total Payments (R$)")
      )
  })
  
  output$state_metrics_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    state_metrics <- df %>%
      group_by(customer_state) %>%
      summarise(
        total_orders = n_distinct(order_id),
        total_revenue = sum(total_payment, na.rm = TRUE),
        avg_order_value = mean(total_payment, na.rm = TRUE),
        delivery_rate = mean(order_status == "delivered", na.rm = TRUE) * 100
      ) %>%
      filter(!is.na(customer_state)) %>%
      arrange(desc(total_orders)) %>%
      head(10)
    
    if (nrow(state_metrics) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No data available for selected filters"))
    }
    
    plot_ly(state_metrics, x = ~reorder(customer_state, total_orders)) %>%
      add_trace(y = ~total_orders, name = "Total Orders",
                type = 'bar', marker = list(color = '#3498db')) %>%
      add_trace(y = ~total_revenue/1000, name = "Revenue (R$/1000)",
                type = 'bar', marker = list(color = '#2ecc71'),
                yaxis = "y2") %>%
      add_trace(y = ~delivery_rate, name = "Delivery Rate %",
                type = 'scatter', mode = 'lines+markers',
                line = list(color = '#e74c3c', width = 2),
                marker = list(color = '#e74c3c', size = 8),
                yaxis = "y3") %>%
      layout(
        title = "State Performance Metrics - Top 10 Brazilian States",
        xaxis = list(title = "State", tickangle = -45),
        yaxis = list(title = "Total Orders", side = "left", position = 0, showgrid = TRUE),
        yaxis2 = list(title = "Revenue (R$/1000)", side = "right",
                      overlaying = "y", position = 1, showgrid = FALSE, automargin = TRUE),
        yaxis3 = list(title = "Delivery Rate %", side = "right",
                      overlaying = "y", position = 0.85, showgrid = FALSE, automargin = TRUE),
        barmode = 'group',
        legend = list(orientation = "h", x = 0, y = 1.1),
        margin = list(r = 150)
      )
  })
  
  output$top_cities_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    top_cities <- df %>%
      group_by(customer_city) %>%
      summarise(orders = n_distinct(order_id)) %>%
      arrange(desc(orders)) %>%
      head(10)
    
    plot_ly(top_cities,
            x = ~orders,
            y = ~reorder(customer_city, orders),
            type = 'bar',
            orientation = 'h',
            marker = list(color = '#1abc9c')) %>%
      layout(
        title = "Top 10 Cities by Orders",
        xaxis = list(title = "Number of Orders"),
        yaxis = list(title = "")
      )
  })
  
  output$regional_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    
    region_data <- df %>%
      mutate(
        region = case_when(
          customer_state %in% c("SP", "RJ", "MG", "ES") ~ "Southeast",
          customer_state %in% c("PR", "RS", "SC") ~ "South",
          customer_state %in% c("BA", "PE", "CE", "MA", "PB", "RN", "AL", "SE", "PI") ~ "Northeast",
          customer_state %in% c("DF", "GO", "MT", "MS") ~ "Central-West",
          TRUE ~ "North"
        )
      ) %>%
      group_by(region) %>%
      summarise(
        orders = n_distinct(order_id),
        revenue = sum(total_payment, na.rm = TRUE)
      )
    
    plot_ly(region_data,
            labels = ~region,
            values = ~orders,
            type = 'pie',
            hole = 0.3,
            textinfo = 'label+percent',
            marker = list(colors = c('#3498db', '#2ecc71', '#e74c3c', '#f39c12', '#9b59b6'))) %>%
      layout(title = "Regional Performance - Orders by Brazilian Region")
  })
  
  output$data_explorer_table <- renderDataTable({
    df <- filtered_data()
    req(df)
    
    df %>%
      select(order_id, order_date, customer_state, customer_city,
             product_category_name, price, total_payment, order_status) %>%
      head(100) %>%
      mutate(order_date = format(order_date, "%Y-%m-%d"))
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("ecommerce_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
