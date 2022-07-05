-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

CREATE DATABASE Kimball2
GO
USE Kimball2

CREATE TABLE
	dimLocation
	(
	Location_ID int PRIMARY KEY,
	County nvarchar(100),
	Country nvarchar(100)
	)
;

CREATE TABLE
	dimProduct
	(
	Product_ID int PRIMARY KEY,
	Product_Name nvarchar(500),
	Product_Category nvarchar(100),
	Product_Sub_Category nvarchar(100)
	)
;

CREATE TABLE
	dimTime
	(
	Date_ID int PRIMARY KEY,
	Date date,
	Day int,
	Week int,
	Month int,
	Year int
	)
;
GO

CREATE TABLE
	factOrders
	(
	Fact_ID int PRIMARY KEY,
	Location_ID int FOREIGN KEY REFERENCES dimLocation(Location_ID),
	Date_ID int FOREIGN KEY REFERENCES dimTime(Date_ID),
	Product_ID int FOREIGN KEY REFERENCES dimProduct(Product_ID),
	Ship_Method nvarchar(50),
	Profit decimal (9,2),
	Ship_Cost decimal (9,2),
	Order_Quantity int,
	No_Of_Orders int
	)
;
GO