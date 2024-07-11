#include <iostream>
using namespace std;

struct nodeM
{
	int index;
	nodeM* next;
};
struct matrix
{
	nodeM* arr[10];
};

matrix matrixintoList(int arr[10][10], int nodes)     
{
	matrix list;
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
					nodeM* tempNew = new nodeM;
					tempNew->index = j + 1;
					tempNew->next = NULL;

					if (!list.arr[i]) list.arr[i] = tempNew;
					else
					{
						nodeM* temp = list.arr[i];
						while (temp->next) temp = temp->next;
						temp->next = tempNew;
					}
				}
			}
		}
	}
	return list;
}

void showList(matrix list, int nodes) {
	nodeM* temp;
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


void task1(int arr[10][10], int j, bool num[10], int amount)
{
	num[j - 1] = true; 

	for (int i = 0; i < amount; i++)
	{
		if (arr[j - 1][i] != 0 and !num[i])
		{
			cout << "node: " << i + 1 << endl;
			task1(arr, i + 1, num, amount);
		}
	}
}

void btask1(int arr[10][10], int j, int amount)
{
	bool num[10];
	for (int i = 0; i < amount; i++) num[i] = false;
	
	task1(arr, j, num, amount);

	for (int i = 0; i < amount; i++)
		if (!num[i])
		{
			cout << endl << "graph is not connected" << endl;
			return;
		}

	cout << endl << "graph is connected" << endl;
}


void task2(matrix* list, int nodes)
{
	nodeM* temp; nodeM** start;
	for (int i = 0; i < nodes; i++)
	{
		temp = (*list).arr[i];
		
		while (temp)
		{
			while ((temp == (*list).arr[i]) && (temp->index == i + 1)) // крайній випадок, коли петля знаходиться у масиві
			{
				nodeM* current = temp;
				temp = temp->next;
				if (temp->next == NULL) (*list).arr[i] = NULL; 
				else (*list).arr[i] = temp;
				delete current;
			}

			while ((temp->next) && ((temp->next->index == temp->index) || (temp->next->index == i + 1))) // всі інші випадки, коли треба видаляти
			{
				nodeM* current = temp->next;
				temp->next = current->next;
				delete current;
			}

			temp = temp->next;
		}
	}
}


matrix task3(matrix list, int nodes)
{
	nodeM* temp; matrix rlist;
	for (int i = 0; i < nodes; i++)
		rlist.arr[i] = NULL;

	for (int i = 0; i < nodes; i++)
	{
		temp = list.arr[i];
		while (temp)                  // проходжусь по початковій структурі і просто у комірки нової з номером індекс поточної - 1 записую індекс і + 1
		{
			nodeM* tempNew = new nodeM;
			tempNew->index = i + 1;
			tempNew->next = NULL;

			if (!rlist.arr[temp->index - 1]) rlist.arr[temp->index - 1] = tempNew;
			else
			{
				nodeM* ntemp = rlist.arr[temp->index - 1];
				while (ntemp->next) ntemp = ntemp->next;
				ntemp->next = tempNew;
			}
			temp = temp->next;
		}
	}

	return rlist;
}


int main()
{
	int arr1[10][10] = {  {1,1,1,0,1,0,0},
						  {1,0,0,0,1,0,0},
						  {1,0,0,1,0,1,0},
						  {0,0,1,0,0,0,0},
						  {1,1,0,0,0,1,1},
						  {0,0,1,0,1,0,1},
						  {0,0,0,0,1,1,0}  };

	int arr2[10][10] = {  {3,3,1,0,1,0,0},
						  {3,0,0,0,1,0,0},
						  {1,0,0,3,0,1,0},
						  {0,0,3,0,0,0,0},
						  {1,1,0,0,1,1,0},
						  {0,0,1,0,1,0,0},
						  {0,0,0,0,0,0,3}  };


	int j, amount;

	cout << "TASK 1" << endl << "Write number of node to start: ";
	cin >> j;
	cout << endl << "Write amount of nodes to start: ";
	cin >> amount;

	cout << "node: " << j << endl;
	btask1(arr1, j, amount);
	
	cout << endl;


	matrix list;
	int nodes = 7;
	list = matrixintoList(arr2, nodes);

	cout << "TASK 2" << endl;
	showList(list, nodes);
	cout << endl << endl;

	task2(&list, nodes);
	showList(list, nodes);
	cout << endl << endl;



	int arr3[10][10] = {  {0,1,1,0,1,0,0},
						  {0,0,0,0,1,0,0},
						  {0,0,0,1,0,1,0},
						  {0,0,0,0,0,0,0},
						  {0,0,0,0,0,1,1},
						  {0,0,0,0,0,0,1},
						  {0,0,0,0,0,0,0}  };

	matrix list1;
	list1 = matrixintoList(arr3, nodes);

	cout << "TASK 3" << endl;
	showList(list1, nodes);
	cout << endl << endl;

	matrix rlist;
	rlist = task3(list1, nodes);
	showList(rlist, nodes);

	return 0;
}