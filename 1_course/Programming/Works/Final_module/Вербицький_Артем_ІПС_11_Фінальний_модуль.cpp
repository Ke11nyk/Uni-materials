#include <iostream>
using namespace std;

/*1.1. Зв’язний циклічний список зберігає послідовність цілих чисел. 
Написати функцію, що перебудовує список так щоб спочатку йшли від’ємні числа, потім решта.*/

struct list
{
    int num;
    list* right;
    list* left;
};

void showList(list* head)
{
    if (head == NULL)
        cout << "The list is empty!" << endl;
    else
    {
        list* current = head;
        int i = 1;
        do
        {
            cout << i << ". " << current->num << endl;
            current = current->right;
            i++;
        } 
        while (current != head);
    }
    cout << endl;
}

void addTestList(list** head, list** tail)
{
    list* temp, * current;

    temp = new list;
    temp->num = 1;

    temp->left = temp->right = temp;
    *head = *tail = temp;

    temp = new list;
    temp->num = -10;

    temp->left = temp->right = *head;
    (*head)->right = (*head)->left = temp;
    current = *tail = temp;

    for (int i = 0; i < 10; i++)
    {
        temp = new list;

        int sign = rand() % 2;
        if (sign == 0) temp->num = rand() % 100;
        else temp->num = -1 * rand() % 100;

        temp->left = current;
        temp->right = *head;
        (*head)->left = current->right = temp;
        current = *tail = temp;
    }
}

// TASK 1 \\

void task1Func(list** head, list** tail)
{
    if (*head != NULL)
    {
        list* current = *head;
        
        while (current != *tail)
        {
            if (current->num >= 0)
            {
                if (current == *head) *head = (*head)->right;
                else
                {
                    current->right->left = current->left;
                    current->left->right = current->right;

                    (*head)->left->right = current;
                    current->left = (*head)->left;
                    current->right = *head;
                    (*head)->left = current;
                }
            }
            current = current->right;
        } 
        

        *tail = (*head)->left;  // перекидую в кінці хвіст, так як раніше не перекидував, аби цикл зупинився в потрібному місці
    }
}

void task1()
{
    cout << "TASK 1" << endl;

    list* listh = new list;
    list* listt = new list;

    addTestList(&listh, &listt);
    showList(listh);
    task1Func(&listh, &listt);
    showList(listh);
}


/*2.1. За допомогою стека реалізувати обхід бінарного дерева в оберненому порядку.
Необхідний стек написати самостійно.*/

struct node
{
    int num;
    node* left;
    node* right;
};

struct nodeStack
{
    int num;
    nodeStack* next;
};

void addNode(node* nod, int data)
{
    node** current = new node*;
    current = (data < nod->num) ? &(nod->left) : &(nod->right);

    if (*current) addNode(*current, data);
    else
    {
        node* newNode = new node;
        newNode->num = data;
        newNode->left = newNode->right = NULL;
        *current = newNode;
    }
}

void createTree(node** root)
{
    int arr[12] = { 4,9,7,15,12,1,20,3,5,8,13,16 };
    (*root)->left = (*root)->right = NULL;
    (*root)->num = 10;

    for (int i = 0; i < 12; i++) addNode(*root, arr[i]);
}

// TASK 2 \\

void addStack(nodeStack** head, int num)
{
    nodeStack* temp = new nodeStack;

    temp->num = num;
    temp->next = *head;
    *head = temp;
}

void delStack(nodeStack** head)
{
    nodeStack* temp = *head;
    cout << temp->num;
    *head = (*head)->next;
    delete temp;
}

void showBinary(node* nod, nodeStack** head)
{
    if (nod)
    {
        addStack(head, nod->num);
        showBinary(nod->left, head);
        showBinary(nod->right, head);
        delStack(head);
        cout << "; ";
    }

}

void task2()
{
    cout << endl << "TASK 2" << endl;

    node* root = new node;
    nodeStack* head = NULL;

    createTree(&root);
    showBinary(root, &head);
    cout << endl;
}


/*3.1. Граф задано структурою суміжності. 
Знайти всі вершини, досяжні від заданої користувачем.*/

struct nodeM1
{
    int index;
    nodeM1* next;
};

struct matrix1
{
    nodeM1* arr[10];
};

matrix1 matrixintoList1(int arr[10][10], int nodes)
{
    matrix1 list;
    for (int i = 0; i < nodes; i++)
        list.arr[i] = NULL;

    for (int i = 0; i < nodes; i++)
    {
        for (int j = 0; j < nodes; j++)
        {
            if (arr[i][j] != 0)
            {
                for (int k = 0; k < arr[i][j]; k++)
                {
                    nodeM1* tempNew = new nodeM1;
                    tempNew->index = j + 1;
                    tempNew->next = NULL;

                    if (!list.arr[i]) list.arr[i] = tempNew;
                    else
                    {
                        nodeM1* temp = list.arr[i];
                        while (temp->next) temp = temp->next;
                        temp->next = tempNew;
                    }
                }
            }
        }
    }
    return list;
}

void showList(matrix1 list, int nodes) {
    nodeM1* temp;
    for (int i = 0; i < nodes; i++)
    {
        cout << i + 1 << " --> ";
        temp = list.arr[i];
        while (temp)
        {
            cout << temp->index << ";       ";
            temp = temp->next;
        }
        cout << endl;
    }
}

// TASK 3 \\

void task3Func(matrix1 list, int j, bool num[10], int amount) // пошук в глибину
{
    num[j - 1] = true;

    nodeM1* temp = list.arr[j - 1];

    while(temp)
    {
        if (!num[temp->index - 1])
        {
            cout << "node: " << temp->index << endl;
            task3Func(list, temp->index, num, amount);
        }
        temp = temp->next;
    }
} 

void task3()
{
    int arr[10][10] = {  {1,1,1,0,1,0,0,0,0},
                          {1,0,0,0,1,0,0,0,0},
                          {1,0,0,1,0,1,0,0,0},
                          {0,0,1,0,0,0,0,0,0},
                          {1,1,0,0,0,1,1,0,0},
                          {0,0,1,0,1,0,1,0,0},
                          {0,0,0,0,1,1,0,0,0},
                          {0,0,0,0,0,0,0,0,1},
                          {0,0,0,0,0,0,0,1,0}  };

    int j;
    cout << endl << "TASK 3" << endl << "Write number of node: ";
    cin >> j;
    cout << endl;

    matrix1 list;
    int nodes = 9;
    list = matrixintoList1(arr, nodes);

    bool num[10];
    for (int i = 0; i < nodes; i++) num[i] = false;

    task3Func(list, j, num, nodes);
}


/*4.3* Написати функцію додавання двох розріджених стислих матриць.*/

struct nodeM {
    int data;
    int index;
    nodeM* next;
};

struct matrix {
    nodeM* arr[4];
};

matrix matrixintoList(int arr[4][4])     // (int **arr, int size1, int size2)
{
    matrix list;
    for (int i = 0; i < 4; i++)
        list.arr[i] = NULL;

    for (int i = 0; i < 4; i++)
    {
        for (int j = 0; j < 4; j++)
        {
            if (arr[i][j] != 0)
            {
                nodeM* tempNew = new nodeM;
                tempNew->data = arr[i][j];
                tempNew->index = j;
                tempNew->next = NULL;
                if (!list.arr[i])
                    list.arr[i] = tempNew;
                else
                {
                    nodeM* temp = list.arr[i];
                    while (temp->next)
                        temp = temp->next;
                    temp->next = tempNew;
                }
            }
        }
    }
    return list;
}

void showList(matrix list) {
    nodeM* temp;
    for (int i = 0; i < 4; i++)
    {
        cout << "line: " << i << ":  ";
        //	cout << "i = " << i << ":   ";
        temp = list.arr[i];
        while (temp)
        {
            cout << temp->index << " <-(index|data)-> " << temp->data << ";       ";
            temp = temp->next;
        }
        cout << endl;
    }
    cout << endl;
}

void sumList(matrix* list1, matrix list2)   // сума матриць
{
    for (int i = 0; i < 4; i++)           // проходжу по коміркам масивів матриць
    {
        nodeM* temp1 = (*list1).arr[i];
        nodeM* temp2 = list2.arr[i];

        if (temp1 == NULL)                // якщо рядок у першій пустий, то просто записую рядок з другої
        {
            temp1 = new nodeM;
            (*list1).arr[i] = temp1;
            temp1->data = temp2->data;
            temp1->index = temp2->index;
            temp1->next = NULL;
            temp2 = temp2->next;
            while (temp2)
            {
                nodeM* tempNew = new nodeM;
                tempNew->data = temp2->data;
                tempNew->index = temp2->index;
                tempNew->next = NULL;
                temp1->next = tempNew;
                temp1 = temp1->next;
                temp2 = temp2->next;
            }
        }
        else
        {
            while (temp2)                 // проходжу по рядку другої матриці
            {
                bool search = false;      // вводжу булеву змінну аби фіксувати знайдену комірку з однаковим індексом 
                while (temp1)             // проходжу по рядку першої матриці і шукаю комірку з тим самим індексом, що у комірки другої
                {
                    if (temp2->index == temp1->index)   // якщо знаходжу, то додаю data і виходжу з циклу
                    {
                        temp1->data += temp2->data;
                        search = true;
                        break;
                    }
                    if (temp1->next != NULL) temp1 = temp1->next;   // ввів if аби в наступному if можна було записати нову комірку
                    else break;
                }

                if (!search)                           // якщо комірки з тим самим індексом нема то записую комірку з другої матриці до першої в кінець рядка
                {
                    nodeM* tempNew = new nodeM;
                    tempNew->data = temp2->data;
                    tempNew->index = temp2->index;
                    temp1->next = tempNew;
                    tempNew->next = NULL;
                }

                temp2 = temp2->next;                  // переходжу до наступної комірки рядка другої матриці
                temp1 = (*list1).arr[i];
            }
        }
    }

}

void task4()
{
    cout << endl << "TASK 4" << endl;

    int array1[4][4] = { 0,0,0,0,  0,2,3,0,  0,4,0,0, 10,0,4,0 };
    int array2[4][4] = { 1,0,5,0,  8,0,0,1,  0,2,0,0, 0,7,3,0 };

    matrix list1, list2;
    list1 = matrixintoList(array1);
    list2 = matrixintoList(array2);

    showList(list1);
    showList(list2);
    sumList(&list1, list2);
    showList(list1);
}


int main()
{
    task1();
    task2();
    task3();
    task4();

    return 0;
}
