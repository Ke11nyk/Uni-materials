﻿#include <iostream>
using namespace std;

struct list
{
    int num;
    list* right;
    list* left;
};

struct cList
{
    int counter;
    int num;
    cList* right;
    cList* left;
};

void addTestList(list** first)         // �������� ������
{
    list* newNum, * current;

    newNum = new list;
    newNum->num = 0;
    newNum->left = NULL;
    *first = newNum;
    current = newNum;

    newNum = new list;
    newNum->num = 0;
    newNum->left = current;
    current->right = newNum;
    current = newNum;

    newNum = new list;
    newNum->num = 1;
    newNum->left = current;
    current->right = newNum;
    current = newNum;

    newNum = new list;
    newNum->num = 0;
    newNum->left = current;
    current->right = newNum;
    current = newNum;

    newNum = new list;
    newNum->num = 3;
    newNum->left = current;
    current->right = newNum;
    current = newNum;

    newNum = new list;
    newNum->num = 0;
    newNum->left = current;
    current->right = newNum;
    current = newNum;

    newNum = new list;
    newNum->num = 0;
    newNum->left = current;
    current->right = newNum;
    current = newNum;

    newNum = new list;
    newNum->num = 4;
    newNum->left = current;
    current->right = newNum;
    current = newNum;

    newNum = new list;
    newNum->num = 0;
    newNum->left = current;
    newNum->right = NULL;
    current->right = newNum;
}

void showList(list* first)             // ��������� ���������� ������
{
    if (first == NULL)
        cout << "The list is empty!" << endl;
    else
    {
        list* current = first;
        int i = 1;
        while (current != NULL)
        {
            cout << i << ". " << current->num << endl;
            current = current->right;
            i++;
        }
    }
    cout << endl;
}

void showList(cList* first)           // ��������� �������� ���������� ������������� ������
{
    if (first == NULL)
        cout << "The list is empty!" << endl;
    else
    {
        cList* current = first;
        int i = 1;
        while (current != NULL)
        {
            cout << i << ". " << current->counter << "<-(counter|number)->" << current->num << endl;
            current = current->right;
            i++;
        }
    }
    cout << endl;
}

void transList(list** first, cList** cFirst)   // ������������ � ������ �������� �������������
{
    list* current = *first;
    list* next;
    cList* temp = new cList;
    int i = 1;

    while (current->right != NULL)
    {
        if(current->num != 0)
        {
            cList* newList = new cList;
            if (*cFirst == NULL)                 // ���� �� ������ ������� � ������� ���������� ������
            {
                newList->counter = i;
                newList->num = current->num;
                newList->left = newList->right = NULL;
                *cFirst = temp = newList;
            }
            else
            {
                newList->counter = i;
                newList->num = current->num;
                newList->left = temp;
                newList->right = NULL;
                temp->right = newList;
                temp = newList;
            }
        }
        
        i++;
        next = current->right;
        delete current;              // ������� ��������� ������ � ���������� ������������
        current = next;
    }

    *first = NULL;
}

void numNUll(cList* current)
{
    int i, j, nulls;

    cout << "Nums of nulls from i to j" << endl << "i: ";
    cin >> i;
    cout << endl << "j: ";
    cin >> j;
    cout << endl;

    nulls = j - i - 1;
    while (current)
    {
        if (current->counter > i && current->counter < j)   // ����� � �������� ���������� ������ ����� � ��������� �� i �� j
            nulls--;                                        // ���� ���� �, �� ������ ������� �� ������� ����
        current = current->right;
    }

    cout << "Num of nulls: " << nulls;
}

int main()
{
    list* first = NULL;
    cList* cFirst = NULL;
    addTestList(&first);
    showList(first);
    transList(&first, &cFirst);
    showList(cFirst);
    showList(first);
    numNUll(cFirst);

    return 0;
}

