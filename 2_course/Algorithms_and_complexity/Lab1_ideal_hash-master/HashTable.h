#pragma once
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
class HashTable
{
public:
	// ���-�������
	int hash(int, int, int, std::string);
	// ������� ��� ��������� �������� �������� �������
	void hash_input(const std::string);
	// ������� ��� �������� �����
	void rehash_collisions();
	// ������� ������ ������� � �������
	void print();
	// ������� ����������� �������� �� ������
	void find_str();
	// ������� ��������� �������� � ������� �� ������
	void delete_str();
	//�����������
	HashTable(unsigned int size) {
		hash_table = new std::vector<std::string>[size];
		keys = new int[size];
		t_size = size;
	}
	//����������
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