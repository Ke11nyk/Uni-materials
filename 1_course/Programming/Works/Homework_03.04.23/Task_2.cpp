#include <iostream>
using namespace std;

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
	matrix list1;
	for (int i = 0; i < 4; i++)
		list1.arr[i] = NULL;

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
				if (!list1.arr[i])
					list1.arr[i] = tempNew;
				else
				{
					nodeM* temp = list1.arr[i];
					while (temp->next)
						temp = temp->next;
					temp->next = tempNew;
				}
			}
		}
	}
	return list1;
}

void showList1(matrix list1) {
	nodeM* temp;
	for (int i = 0; i < 4; i++)
	{
		cout << "line: " << i << ":  ";
		//	cout << "i = " << i << ":   ";
		temp = list1.arr[i];
		while (temp)
		{
			cout << temp->index << " <-(index|data)-> " << temp->data << ";       ";
			temp = temp->next;
		}
		cout << endl;
	}
	cout << endl;
}



void listintoMatrix(matrix list, int arr[4][4])      // перетворення у двовимірний масив
{
	for (int i = 0; i < 4; i++)
	{
		nodeM* temp = list.arr[i];
		while (temp)
		{
			arr[i][temp->index] = temp->data;
			temp = temp->next;
		}
	}
}

void showMatrix(int arr[4][4])            // виведення масиву в консоль
{
	for (int i = 0; i < 4; i++)
	{
		for (int j = 0; j < 4; j++) cout << arr[i][j] << "; ";
		cout << endl;
	}
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


int main()
{
	int array1[4][4] = { 0,0,0,0,  0,2,3,0,  0,4,0,0, 10,0,4,0 };
	int array2[4][4] = { 1,0,5,0,  8,0,0,1,  0,2,0,0, 0,7,3,0 };
	int array[4][4] =  { 0,0,0,0,  0,0,0,0,  0,0,0,0, 0,0,0,0 };
	matrix list1, list2;
	list1 = matrixintoList(array1);
	list2 = matrixintoList(array2);
	showList1(list1);
	showList1(list2);
	sumList(&list1, list2);
	showList1(list1);
	listintoMatrix(list1, array);
	showMatrix(array);

	return 0;
}