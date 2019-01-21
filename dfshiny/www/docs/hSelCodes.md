---
title: "Select Code/s"
author: "Alex F. Bokov"
date: "January 21, 2019"
output: html_document
---

This is a special transformation: it requires you to choose which EMR codes to 
include. Click the blank text field and you will see a menu of all distinct EMR
codes that occur in this column. You have to pick at least one EMR code in order
to select this transformation, otherwise your click will be ignored. It's like
the `Last numeric value for each visit` except that it will only report last 
numeric values for observations whose EMR code is one of those which you select.

The purpose of this transformation is to split out adjacent but different 
lab results, vital signs, and medication dosages that your i2b2 query might have
combined into a single column.

You will be able to add multiple instances of this transformation as long as
the combination of EMR codes you select is unique (their order does not matter).
If you select combination of codes you have already added to the `Chosen...` 
list, your click will be ignored. DataFinisher automatically generates a unique
name for each new column you create in this manner. 
