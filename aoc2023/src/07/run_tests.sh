#!/usr/bin/env bash 

# Compile
gnatmake camel_cards-test.adb

# Run
./camel_cards-test

# Clean up
rm *.ali *.o
rm camel_cards-test