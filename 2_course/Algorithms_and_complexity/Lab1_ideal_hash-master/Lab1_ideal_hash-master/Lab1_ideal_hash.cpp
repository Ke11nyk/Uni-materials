
#include <iostream>
#include "HashTable.h"
int main()
{
    HashTable hash_tab;
    const std::string src_file_name = "word_list.txt";
    hash_tab.hash_input(src_file_name);
}
