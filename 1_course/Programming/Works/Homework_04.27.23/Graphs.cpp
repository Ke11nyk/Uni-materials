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

matrix matrixintoList(int arr[10][10], int nodes)     // (int **arr, int size1, int size2)
{
	matrix list1;
	for (int i = 0; i < nodes; i++)
		list1.arr[i] = NULL;

	for (int i = 0; i < nodes; i++)
	{
		for (int j = 0; j < nodes; j++)
		{
			if (arr[i][j] != 0)
			{
				nodeM* tempNew = new nodeM;
				tempNew->index = j+1;
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


bool EulerMatrix(int arr[10][10], int nodes)
{
	for (int i = 0; i < nodes; i++)
	{
		int sum = 0;
		for (int j = 0; j < nodes; j++) if (arr[i][j] != 0) sum++;
		if ((sum % 2 != 0) or (sum == 0)) return false;
	}

	return true;
}

bool EulerList(matrix list, int nodes)
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


bool NeighborMatrix(int arr[10][10], int i, int j)
{
	if (arr[i - 1][j - 1] == 0) return false;
	return true;
}


bool NeighborList(matrix list, int i, int j)
{
	nodeM* temp;
	temp = list.arr[i-1]->next;
	while (temp)
	{
		if (temp->index == j) return true;
		temp = temp->next;
	}

	return false;
}

int main()
{
	int arr1[10][10] = {  {0,1,1,0,1,0,0,0},
						  {1,0,0,0,1,0,0,0},
						  {1,0,0,1,0,1,0,0},
						  {0,0,1,0,0,0,0,0},
						  {1,1,0,0,0,1,1,0}, 
						  {0,0,1,0,1,0,1,0}, 
						  {0,0,0,0,1,1,0,0}, 
						  {0,0,0,0,0,0,0,0} };

	matrix list1;
	int nodes1 = 8;
	list1 = matrixintoList(arr1, nodes1);
	showList(list1, nodes1);
	cout << endl << endl;

	cout << "EulerM1: ";
	if (EulerMatrix(arr1, nodes1)) cout << "YES";
	else cout << "NO";
	cout << endl << endl;

	cout << "EulerL1: ";
	if (EulerList(list1, nodes1)) cout << "YES";
	else cout << "NO";
	cout << endl << endl;

	int i, j;
	cout << "NeighborM1: " << endl << "i:";
	cin >> i; cout << "j:"; cin >> j;
	if (NeighborMatrix(arr1, i, j)) cout << "YES";
	else cout << "NO";
	cout << endl << endl;

	cout << "NeighborL1: " << endl << "i:";
	cin >> i; cout << "j:"; cin >> j;
	if (NeighborList(list1, i, j)) cout << "YES";
	else cout << "NO";
	cout << endl << endl;
	

	int arr2[10][10] = { {0,1,0,1,0},
						 {1,0,1,0,0},
						 {0,1,0,1,0},
						 {1,0,1,0,0},
						 {0,0,0,0,0} };

	matrix list2;
	int nodes2 = 5;
	list2 = matrixintoList(arr2, nodes2);
	showList(list2, nodes2);
	cout << endl << endl;

	cout << "EulerM2: ";
	if (EulerMatrix(arr2, nodes2)) cout << "YES";
	else cout << "NO";
	cout << endl << endl;

	cout << "EulerL2: ";
	if (EulerList(list2, nodes2)) cout << "YES";
	else cout << "NO";
	cout << endl << endl;


	matrix list3;
	int nodes3 = 4;
	list3 = matrixintoList(arr2, nodes3);
	showList(list3, nodes3);
	cout << endl << endl;

	cout << "EulerM3: ";
	if (EulerMatrix(arr2, nodes3)) cout << "YES";
	else cout << "NO";
	cout << endl << endl;

	cout << "EulerL3: ";
	if (EulerList(list3, nodes3)) cout << "YES";
	else cout << "NO";
	cout << endl << endl;

	return 0;
}