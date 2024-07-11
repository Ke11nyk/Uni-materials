#pragma once
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
class HashTable
{
public:
	int hash(int, int, int, std::string);
	void hash_input(const std::string);
private:
	std::string hash_table[50];
};

