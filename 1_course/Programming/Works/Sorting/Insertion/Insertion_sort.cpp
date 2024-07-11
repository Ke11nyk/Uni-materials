#include <iostream>

using namespace std;

void showArr(int arr[], int max)
{
	for (int i = 0; i < max; i++)
		cout << arr[i] << "; ";
	cout << endl;
}

void insertSort(int arr[], int max)
{
	for (int i = 1; i < max; i++)
	{
		int j = i - 1;
		while (arr[j] > arr[i] and j >= 0) j--; 
		j++;

		int temp = arr[i];
		for (int k = i - 1; k >= j; k--)
			arr[k + 1] = arr[k];
		arr[j] = temp;
		showArr(arr, max);
	}
}

int main()
{
	int const max = 8;

	int arr[max] = { 9,3,5,8,7,6,4,2 };

	showArr(arr, max);
	insertSort(arr, max);

	return 0;
}


