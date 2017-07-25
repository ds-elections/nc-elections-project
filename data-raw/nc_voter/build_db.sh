#!/bin/bash
# run this shell script with argument user name for the db

psql -h ncelections.c7d0c8vzwwoc.us-west-2.rds.amazonaws.com --user $1 -f make_nc_db.sql

psql -h ncelections.c7d0c8vzwwoc.us-west-2.rds.amazonaws.com --user $1 -c "\copy nc_vhis FROM 'ncvhis_Statewide.txt' WITH csv DELIMITER E'\t' HEADER" -c "\copy nc_voter FROM 'ncvoter_Statewide.txt' WITH csv DELIMITER E'\t' HEADER ENCODING 'windows-1251'"
