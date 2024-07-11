#include "HashTable.h"

int HashTable::hash(int k, int p, int m, std::string word)
{
    int len = word.length();
    int hash = 0;
    for (int i = 0; i < len; i++) {
        char t = word[i];
        hash += (t - '\0') * (k ^ i);
    }
    hash %= p;
    hash %= m;
    return hash;
}

void HashTable::hash_input(const std::string file_name)
{
    std::ifstream in_file(file_name);
    if (in_file.is_open()) {
        std::string word;
        while (getline(in_file, word)) {
            std::cout << word << '\n';
        }
        in_file.close();
    }
    else {
        std::cout << "\n ERROR: Unable to open file " << file_name << '\n';
    }
}
