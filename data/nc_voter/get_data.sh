#!/bin/bash

wget https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvhis_Statewide.zip
wget https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvoter_Statewide.zip
wget https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvhis_ncvoter_data_format.txt

7za x ncvhis_Statewide.zip
7za x ncvoter_Statewide.zip

rm *.zip
