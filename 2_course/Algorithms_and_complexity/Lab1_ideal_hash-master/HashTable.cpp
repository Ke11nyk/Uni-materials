#include "HashTable.h"

int HashTable::hash(int k, int p, int m, std::string word)
{
    if (m == 0 || p == 0) {
        return 0;
    }
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
// Допоміжна функція для перевірки чи є val простим
bool is_prime(int val) {
    int root = sqrt(val);
    for (int i = 2; i <= root; i++) {
        if (val % i == 0) {
            return false;
        }
    }
    return true;
}
// Допоміжна функція для знаходження наступного після orig простого числа
int next_prime(int orig) {
    int next = orig + 1;
    while (!is_prime(next)) {
        next++;
    }
    return next;
}

void HashTable::rehash_collisions() {
    for (int i = 0; i < t_size; i++) {
        if (hash_table[i].size() > 1) {
            bool no_collisions = false;
            while (!no_collisions) {
                std::vector<std::string> new_vec(hash_table[i].size() * hash_table[i].size());
                keys[i] = rand() % t_size + 1;
                int p = next_prime(new_vec.size());
                for (int j = 0; j < hash_table[i].size(); j++) {
                    int ind_hash = hash(keys[i], p, new_vec.size(), hash_table[i][j]);
                    if (new_vec[ind_hash].empty()) {
                        new_vec[ind_hash] = hash_table[i][j];
                        no_collisions = true;
                    }
                    else {
                        no_collisions = false;
                        break;
                    }
                }
                if (no_collisions) {
                    hash_table[i] = new_vec;
                }
            }
            
        }
        else {
            keys[i] = 0;
        }
    }
}

void HashTable::hash_input(const std::string file_name)
{
    k = rand() % t_size;
    int p = next_prime(t_size);
    std::ifstream in_file(file_name);
    if (in_file.is_open()) {
        std::string word;
        while (getline(in_file, word)) {
            hash_table[hash(k, p, t_size, word)].push_back(word);
        }
        in_file.close();
    }
    else {
        std::cout << "\n ERROR: Unable to open file " << file_name << '\n';
    }
}

void HashTable::print()
{
    for (int i = 0; i < t_size; i++) {
        std::cout << i << ":   " ;
        for (int j = 0; j < hash_table[i].size(); j++) {
            if (!hash_table[i][j].empty()) {
                std::cout << j << " : " << hash_table[i][j] << ' ';
            }
        }
        std::cout << '\n';
    }
}

void HashTable::find_str()
{
    std::string str;
    std::cout << "\n Enter word: ";
    std::cin >> str;
    int t_pos = hash(k, next_prime(t_size), t_size, str);
    int t_vec_pos;
    if (hash_table[t_pos].size() != 1) {
        t_vec_pos = hash(keys[t_pos], next_prime(hash_table[t_pos].size()), hash_table[t_pos].size(), str);
    }
    else {
        t_vec_pos = 0;
    }
    if (str == hash_table[t_pos][t_vec_pos]) {
        std::cout << "\n ( " << t_pos << " , " << t_vec_pos << " ) " << hash_table[t_pos][t_vec_pos];
    }
    else {
        std::cout << "\n This word does not exist in this table\n";
    }
}

void HashTable::delete_str()
{
    std::string str;
    std::cout << "\n Enter word: ";
    std::cin >> str;
    int t_pos = hash(k, next_prime(t_size), t_size, str);
    int t_vec_pos;
    if (hash_table[t_pos].size() != 1) {
        t_vec_pos = hash(keys[t_pos], next_prime(hash_table[t_pos].size()), hash_table[t_pos].size(), str);
    }
    else {
        t_vec_pos = 0;
    }
    std::vector<std::string>::iterator it = hash_table[t_pos].begin();
    hash_table[t_pos].erase(it + t_vec_pos);
}
