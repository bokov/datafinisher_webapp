---
title: "Available Transformations"
author: "Alex F. Bokov"
date: "January 21, 2019"
output: html_document
---

This section shows which transformations are available for you to apply to this
main column in order to derive additional output columns from it. You can choose
one or more transformations by clicking `Add/Update` next to them. DataFinisher
will not create duplicate columns if you click on an already chosen 
transformation again-- it will silently update that transformation and keep 
going.

Not all columns have transformations available-- static columns (including 
`patient_num`, `start_date`, `birth_date`, `sex_cd`, `language_cd`, `race_cd`,
`age_at_visit_days`, and `age_at_death_days`) always get passed through to the
output unchanged.

If you do not choose a transformation for a dynamic column (e.g. this one) then
DataFinisher will use its default algorithm to select at least one 
transformation for you: columns that contain numeric values and only one type of
code will default to last numeric value per visit, columns that have no numeric
values but still only one type of code will default to TRUE/FALSE, and all 
others will default to a concatenated list of unique values. **The default 
transformation rules are under intense development and will become 'smarter' 
over time. You are encouraged to choose your own transformations either now or 
at a future time by uploading this file into DataFinisher again.**

Regardless of which transformations were chosen, the original value of each 
dynamic column is always copied unaltered into the output file, always after the 
transformed columns that were derived from it. This enables you to later come
back and choose a different set of transformations on the same data.
