#pragma once
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
class HashTable
{
public:
	// Хеш-функція
	int hash(int, int, int, std::string);
	// Функція для хешування елементів первинної таблиці
	void hash_input(const std::string);
	// Функція для усунення колізій
	void rehash_collisions();
	// Функція виводу таблиці в консоль
	void print();
	// Функція знаходження елемента за ключем
	void find_str();
	// Функція видалення елемента з таблиці за ключем
	void delete_str();
	//Конструктор
	HashTable(unsigned int size) {
		hash_table = new std::vector<std::string>[size];
		keys = new int[size];
		t_size = size;
	}
	//Деструктор
	~HashTable() {
		for (unsigned int i = 0; i < t_size; i++) {
			hash_table[i].clear();
		}
		delete[] hash_table;
	}
private:
	std::vector<std::string>* hash_table;
	int* keys;
	unsigned int t_size;
	int k;
};