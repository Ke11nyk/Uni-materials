#include "pch.h"
#include "..\OOP Lab 2.3a\OOP Lab 2.3a.cpp"

TEST(MergeSortTest, SerialMergeSort) {
    std::vector<int> input = { 5, 2, 1, 8, 3, 7, 4, 6 };
    std::vector<int> expected = { 1, 2, 3, 4, 5, 6, 7, 8 };
    EXPECT_EQ(merge_sort_serial(input), expected);
}

TEST(MergeSortTest, ParallelMergeSort) {
    std::vector<int> input = { 5, 2, 1, 8, 3, 7, 4, 6 };
    std::vector<int> expected = { 1, 2, 3, 4, 5, 6, 7, 8 };
    EXPECT_EQ(merge_sort_parallel(input), expected);
}

TEST(MergeSortTest, RandomInput) {
    std::vector<int> input(10000);
    for (int i = 0; i < 10000; i++) {
        input[i] = rand() % 10000;
    }
    std::vector<int> serial_output = merge_sort_serial(input);
    std::vector<int> parallel_output = merge_sort_parallel(input);
    EXPECT_EQ(serial_output, parallel_output);
}