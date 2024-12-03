#!/bin/sh

echo "Running all example programs and their results"

echo "Program 1"
./prog1
echo "wanted: 11 got: $(echo $?)"

echo ""

echo "Program 2"
./prog2
echo "wanted: 249 got: $(echo $?)"

echo ""

echo "Program 3"
./prog3
echo "wanted: 5 got: $(echo $?)"

echo ""

echo "Program 4"
./prog4
echo "wanted: 10 got: $(echo $?)"

echo ""

echo "Program 5"
./prog5
echo "wanted: 5 got: $(echo $?)"

echo ""

echo "Program 6"
./prog6
echo "wanted: 0 got: $(echo $?)"

echo ""

echo "Program 7"
./prog7
echo "wanted: 3 got: $(echo $?)"

echo ""

echo "Program 8"
./prog8
echo "wanted: 5 got: $(echo $?)"

echo ""

echo "Program 10"
./prog10
echo "wanted: 10 got: $(echo $?)"

echo ""

echo "Program 11"
echo "wanted: 4 got: $(./prog11)"

echo ""

echo "Program 12"
./prog12
echo "wanted: 0 got: $(echo $?)"

echo ""

echo "Program 13"
./prog13
echo "wanted: 0 got: $(echo $?)"

echo ""

echo "Program 14"
echo "wanted: 4 got: $(./prog14)"

echo ""

echo "Program 15"
./prog15
echo "wanted: 2 got: $(echo $?)"

echo ""

echo "Program 16"
./prog16
echo "wanted: 3 got: $(echo $?)"

echo ""