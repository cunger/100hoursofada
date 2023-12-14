#!/usr/bin/env bash 

# Compile
gnatmake camel_cards_test.adb

# Run
./camel_cards_test

# Clean up
rm *.ali *.o
rm camel_cards_test