
#include <iostream>
#include "HashTable.h"
#include <time.h>
int main()
{
    srand(time(0));
    const int N = 50;
    const std::string src_file_name = "word_list.txt";

    //Створюємо таблицю
    HashTable hash_tab(N);
    hash_tab.hash_input(src_file_name);
    hash_tab.rehash_collisions();
    char input = '\0';

    //Main цикл для взаємодії користувача і програми
    while (input != '3') {
        std::cout << "\n 0 - print table;\n 1 - find word;\n 2 - delete word;\n 3 - exit; \n";
        std::cin >> input;
        switch (input)
        {
        case(48):
            hash_tab.print();
            break;
        case(49):
            hash_tab.find_str();
            break;
        case(50):
            hash_tab.delete_str();
            break;
        case(51):
            break;
        default:
            std::cout << "\n Invalid command \n";
            break;
        }
    }
}
