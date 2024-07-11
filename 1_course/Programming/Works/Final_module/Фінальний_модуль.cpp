#include <iostream>
using namespace std;

/*1.7. Для стеку написати функцію, що впорядковує його наступним чином – спочатку
від’ємні числа, потім решта.*/

struct nodeStack 
{
    int num;
    nodeStack* next;
};

void task1(nodeStack** head) 
{
    nodeStack* temp = *head;

    while (temp)
    {
        if (((temp == *head) && (temp->num >= 0)) || (temp->next->num >= 0))
        {
            nodeStack* current; nodeStack* end;

            if ((temp == *head) && (temp->num >= 0))
            {
                current = end = temp;
                *head = temp = temp->next;
            }

            if (temp->next->num >= 0)
            {
                current = end = temp->next;
                temp->next = current->next;
            }

            while (end->next) end = end->next;
           
            end->next = current;
            current->next = NULL;
        }

        temp = temp->next;
    }
}


/*2.7. Для дерева степеня 3 знайти кількість всіх внутрішній вершин.*/

struct node
{
    int num;
    node* offspring[3];
};

void task2(node* nod, int numNodes)  // при виклику функції потрібно врахувати, що туди входитиме корінь, тому відняти 1
{
    if (!nod)
        return;

    if (nod->offspring[0])
    {
        numNodes++;

        for (int i = 0; i < 3; i++)
            task2(nod->offspring[i], numNodes);
    }
}


/*3.7. Перевірити неорієнтований граф, поданий структурою суміжності, на ейлеровість.*/

struct nodeM
{
    int index;
    nodeM* next;
};

struct matrix
{
    nodeM* arr[10];
};

bool task3(matrix list, int nodes)
{
    nodeM* temp;
    for (int i = 0; i < nodes; i++)
    {
        int sum = 0;
        temp = list.arr[i];

        while (temp)
        {
            sum++;
            temp = temp->next;
        }

        if ((sum % 2 != 0) or (sum == 0)) return false;
    }

    return true;
}


int main() 
{
    return 0;
}