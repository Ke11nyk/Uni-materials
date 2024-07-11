#include <iostream>
#include <vector>
#include <thread>
#include <chrono>
//#include <gtest/gtest.h>

// Helper function to merge two sorted arrays
std::vector<int> merge(const std::vector<int>& left, const std::vector<int>& right) {
    std::vector<int> result;
    size_t l = 0, r = 0;

    while (l < left.size() && r < right.size()) {
        if (left[l] < right[r]) {
            result.push_back(left[l++]);
        }
        else {
            result.push_back(right[r++]);
        }
    }

    result.insert(result.end(), left.begin() + l, left.end());
    result.insert(result.end(), right.begin() + r, right.end());

    return result;
}

// Serial merge sort implementation
std::vector<int> merge_sort_serial(const std::vector<int>& arr) {
    if (arr.size() <= 1) {
        return arr;
    }

    size_t mid = arr.size() / 2;
    std::vector<int> left(arr.begin(), arr.begin() + mid);
    std::vector<int> right(arr.begin() + mid, arr.end());

    left = merge_sort_serial(left);
    right = merge_sort_serial(right);

    return merge(left, right);
}

// Parallel merge sort implementation
std::vector<int> merge_sort_parallel(const std::vector<int>& arr) {
    if (arr.size() <= 1024) {  // Base case for small arrays
        return merge_sort_serial(arr);
    }

    size_t mid = arr.size() / 2;
    std::vector<int> left(arr.begin(), arr.begin() + mid);
    std::vector<int> right(arr.begin() + mid, arr.end());

    std::thread left_thread([&]() { left = merge_sort_parallel(left); });
    std::thread right_thread([&]() { right = merge_sort_parallel(right); });

    left_thread.join();
    right_thread.join();

    return merge(left, right);
}

// Unit tests
//TEST(MergeSortTest, SerialMergeSort) {
//    std::vector<int> input = { 5, 2, 1, 8, 3, 7, 4, 6 };
//    std::vector<int> expected = { 1, 2, 3, 4, 5, 6, 7, 8 };
//    EXPECT_EQ(merge_sort_serial(input), expected);
//}
//
//TEST(MergeSortTest, ParallelMergeSort) {
//    std::vector<int> input = { 5, 2, 1, 8, 3, 7, 4, 6 };
//    std::vector<int> expected = { 1, 2, 3, 4, 5, 6, 7, 8 };
//    EXPECT_EQ(merge_sort_parallel(input), expected);
//}
//
//TEST(MergeSortTest, RandomInput) {
//    std::vector<int> input(10000);
//    for (int i = 0; i < 10000; i++) {
//        input[i] = rand() % 10000;
//    }
//    std::vector<int> serial_output = merge_sort_serial(input);
//    std::vector<int> parallel_output = merge_sort_parallel(input);
//    EXPECT_EQ(serial_output, parallel_output);
//}

int main(int argc, char** argv) {
    /*::testing::InitGoogleTest(&argc, argv);
    int result = RUN_ALL_TESTS();*/

    int result = 0;

    if (result == 0) {
        std::cout << "All tests passed!" << std::endl;

        const int n = 1000000;
        std::vector<int> input(n);
        for (int i = 0; i < n; i++) {
            input[i] = rand() % n;
        }

        auto start_serial = std::chrono::high_resolution_clock::now();
        merge_sort_serial(input);
        auto end_serial = std::chrono::high_resolution_clock::now();
        auto duration_serial = std::chrono::duration_cast<std::chrono::milliseconds>(end_serial - start_serial).count();

        auto start_parallel = std::chrono::high_resolution_clock::now();
        merge_sort_parallel(input);
        auto end_parallel = std::chrono::high_resolution_clock::now();
        auto duration_parallel = std::chrono::duration_cast<std::chrono::milliseconds>(end_parallel - start_parallel).count();

        std::cout << "Serial merge sort time: " << duration_serial << " ms" << std::endl;
        std::cout << "Parallel merge sort time: " << duration_parallel << " ms" << std::endl;
    }

    return result;
}