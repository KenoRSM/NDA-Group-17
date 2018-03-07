# Data Preparation
library(data.table)
library(igraph)
library(ggplot2)
# df3[, c("foo","bar"):=NULL] 
setwd("C:/Users/marci/Desktop/Studies/5_Network_Data_Analytics/Tm_Assignment_Shiny/FirstShiny/data")

dt.qvc.1 <- as.data.table(read.csv("QVC data 1.csv", sep = ";"))
dt.qvc.2 <- as.data.table(read.csv("QVC data 2.csv", sep = ","))
dt.qvc.3 <- as.data.table(read.csv("QVC data 3.csv", sep = ","))
dt.qvc.4 <- as.data.table(read.csv("QVC data 4.csv", sep = ","))
dt.qvc.5 <- as.data.table(read.csv("QVC data 5.csv", sep = ","))
dt.qvc.6 <- as.data.table(read.csv("QVC data 6.csv", sep = ","))

dt.qvc.1 <- dt.qvc.1[, c("X.Sales_Order_Nbr", "Sales_Order_Line_Nbr", "Order_Dt", "Party_Id",
                         "Total_Line_Amt", "Unit_Price_Amt", "Product_Id", "Shipped_Dt",
                         "Source_Ship_Warehouse_Nbr", "Assigned_Dc_Id", "Cancelled_Qty",
                         "Merchandise_Dept_Desc", "Shipment_Status_Dt", "Pickup_Dt",
                         "Scheduled_Delivery_Dt", "Rescheduled_Delivery_Dt", "Package_Scan_Dttm",
                         "Delivery_Confirmation_Dt", "SHIP_TO_STATE", "SHIP_TO_ZIP")]

dt.qvc.2 <- dt.qvc.2[, c("X.Sales_Order_Nbr", "Sales_Order_Line_Nbr", "Order_Dt", "Party_Id",
                         "Total_Line_Amt", "Unit_Price_Amt", "Product_Id", "Shipped_Dt",
                         "Source_Ship_Warehouse_Nbr", "Assigned_Dc_Id", "Cancelled_Qty",
                         "Merchandise_Dept_Desc", "Shipment_Status_Dt", "Pickup_Dt",
                         "Scheduled_Delivery_Dt", "Rescheduled_Delivery_Dt", "Package_Scan_Dttm",
                         "Delivery_Confirmation_Dt", "SHIP_TO_STATE", "SHIP_TO_ZIP")]

dt.qvc.3 <- dt.qvc.3[, c("X.Sales_Order_Nbr", "Sales_Order_Line_Nbr", "Order_Dt", "Party_Id",
                         "Total_Line_Amt", "Unit_Price_Amt", "Product_Id", "Shipped_Dt",
                         "Source_Ship_Warehouse_Nbr", "Assigned_Dc_Id", "Cancelled_Qty",
                         "Merchandise_Dept_Desc", "Shipment_Status_Dt", "Pickup_Dt",
                         "Scheduled_Delivery_Dt", "Rescheduled_Delivery_Dt", "Package_Scan_Dttm",
                         "Delivery_Confirmation_Dt", "SHIP_TO_STATE", "SHIP_TO_ZIP")]

dt.qvc.4 <- dt.qvc.4[, c("X.Sales_Order_Nbr", "Sales_Order_Line_Nbr", "Order_Dt", "Party_Id",
                         "Total_Line_Amt", "Unit_Price_Amt", "Product_Id", "Shipped_Dt",
                         "Source_Ship_Warehouse_Nbr", "Assigned_Dc_Id", "Cancelled_Qty",
                         "Merchandise_Dept_Desc", "Shipment_Status_Dt", "Pickup_Dt",
                         "Scheduled_Delivery_Dt", "Rescheduled_Delivery_Dt", "Package_Scan_Dttm",
                         "Delivery_Confirmation_Dt", "SHIP_TO_STATE", "SHIP_TO_ZIP")]

dt.qvc.5 <- dt.qvc.5[, c("X.Sales_Order_Nbr", "Sales_Order_Line_Nbr", "Order_Dt", "Party_Id",
                         "Total_Line_Amt", "Unit_Price_Amt", "Product_Id", "Shipped_Dt",
                         "Source_Ship_Warehouse_Nbr", "Assigned_Dc_Id", "Cancelled_Qty",
                         "Merchandise_Dept_Desc", "Shipment_Status_Dt", "Pickup_Dt",
                         "Scheduled_Delivery_Dt", "Rescheduled_Delivery_Dt", "Package_Scan_Dttm",
                         "Delivery_Confirmation_Dt", "SHIP_TO_STATE", "SHIP_TO_ZIP")]

dt.qvc.6 <- dt.qvc.6[, c("X.Sales_Order_Nbr", "Sales_Order_Line_Nbr", "Order_Dt", "Party_Id",
                         "Total_Line_Amt", "Unit_Price_Amt", "Product_Id", "Shipped_Dt",
                         "Source_Ship_Warehouse_Nbr", "Assigned_Dc_Id", "Cancelled_Qty",
                         "Merchandise_Dept_Desc", "Shipment_Status_Dt", "Pickup_Dt",
                         "Scheduled_Delivery_Dt", "Rescheduled_Delivery_Dt", "Package_Scan_Dttm",
                         "Delivery_Confirmation_Dt", "SHIP_TO_STATE", "SHIP_TO_ZIP")]


dt.qvc.all <- do.call("rbind", list(dt.qvc.1, dt.qvc.2, dt.qvc.3, dt.qvc.4, dt.qvc.5, dt.qvc.6))

rm(list = c("dt.qvc.1", "dt.qvc.2", "dt.qvc.3", "dt.qvc.4", "dt.qvc.5", "dt.qvc.6"))

dt.qvc.all <- dt.qvc.all[, c("X.Sales_Order_Nbr", "Sales_Order_Line_Nbr", "Party_Id",
                         "Total_Line_Amt", "Unit_Price_Amt", "Product_Id", "Shipped_Dt",
                         "Source_Ship_Warehouse_Nbr", "Assigned_Dc_Id", "Cancelled_Qty",
                         "Merchandise_Dept_Desc", "Pickup_Dt", "Scheduled_Delivery_Dt", 
                         "Rescheduled_Delivery_Dt", "Delivery_Confirmation_Dt", 
                         "SHIP_TO_STATE", "SHIP_TO_ZIP")]

dt.qvc.all <- dt.qvc.all[, n_orders := .N, by = "Product_Id"]


setnames(dt.qvc.all, "X.Sales_Order_Nbr", "Sales_Order_Nbr")
setnames(dt.qvc.all, c("SHIP_TO_STATE", "SHIP_TO_ZIP"), c("ship_to_state", "ship_to_zip"))

##### Oncea dt.qvc.all loaded

# dt.qvc.clean <- dt.qvc.all[!(Rescheduled_Delivery_Dt == "" )]
dt.qvc.all <- dt.qvc.all[, c("n_orders") := NULL]
dt.qvc.clean <- dt.qvc.all[!(Rescheduled_Delivery_Dt == "" & Scheduled_Delivery_Dt == "")]


##### OPERATION ON DATA CLEAN
# df3[, c("foo","bar"):=NULL] 
dt.qvc.clean <- dt.qvc.clean[, n_orders := .N, by = "Product_Id"]
dt.qvc.clean <- dt.qvc.clean[!(Delivery_Confirmation_Dt == "")]

# Merge disti information
dt.qvc.disti <- as.data.table(read.csv("QVCdist_ctr.csv", sep = ","))

dt.qvc.disti$Source_Ship_Warehouse_Nbr <- as.integer(dt.qvc.disti$Source_Ship_Warehouse_Nbr)

dt.qvc.clean$Source_Ship_Warehouse_Nbr <- as.integer(dt.qvc.clean$Source_Ship_Warehouse_Nbr)

dt.qvc.clean <- merge(dt.qvc.clean, dt.qvc.disti, by = "Source_Ship_Warehouse_Nbr")

# Load zip locations
dt.zip <- as.data.table(read.csv("dtzip.csv", sep = ","))
dt.qvc.clean$ship_to_zip <- as.integer(as.character(dt.qvc.clean$ship_to_zip))
dt.zip$zip <- as.integer(dt.zip$zip)

dt.qvc.clean.zip <- merge(dt.qvc.clean, dt.zip, by.x = "ship_to_zip", by.y = "zip")

# Cut by the number of ordered products
# dt.qvc.clean4 <- dt.qvc.clean3[n_orders >= 50]

setnames(dt.qvc.clean.zip, c("lat", "lng", "city", "state"),
         c("ship_to_lat", "ship_to_lng", "ship_to_city", "zip_ship_to_state")) 

dt.qvc.clean.zip$POSTL_CD <- as.integer(as.character(dt.qvc.clean.zip$POSTL_CD))

dt.qvc.clean.zip <- merge(dt.qvc.clean.zip, dt.zip, by.x = "POSTL_CD", by.y = "zip")

###########################################################

dt.qvc.clean.zip <- na.omit(dt.qvc.clean.zip, cols = 'POSTL_CD')
dt.qvc.clean.zip <- na.omit(dt.qvc.clean.zip, cols = 'ship_to_zip')

setnames(dt.qvc.clean.zip, c("lat", "lng", "city", "state"),
         c("disti_lat", "disti_lng", "disti_city", "disti_state"))

rm(dt.qvc.all)
rm(dt.qvc.clean)
rm(dt.qvc.disti)
rm(dt.zip)

dt.qvc.clean <- dt.qvc.clean.zip


###########################################################
# To do:

# dt.qvc.clean <- dt.qvc.clean[n_orders >= 50]

# change to dates:
# dt.qvc.clean2$Rescheduled_Delivery_Dt <- as.character(dt.qvc.clean2$Rescheduled_Delivery_Dt)
# dt.qvc.clean2$Scheduled_Delivery_Dt <- as.character(dt.qvc.clean2$Scheduled_Delivery_Dt)
