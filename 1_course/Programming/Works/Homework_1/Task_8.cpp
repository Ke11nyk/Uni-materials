#include <iostream>
using namespace std;

int uniqueCheck(int arr[], int max)
{
	int counter = 0, k = 0, arr1[max];
	bool b[max];
	for (int i = 0; i < max; i++)
		b[i] = true;
	    
	for (int i = 0; i < max; i++)
	{
		if (b[i])
		    for (int j = i+1; j < max; j++)
				if (arr[i]==arr[j])
			        b[j] = false;
		if (b[i])
		{
			if (k == 0)
			    arr1[k] = arr[i];
			else if (arr[i] > arr1[k-1])
			    arr1[k] = arr[i];
			else if (arr[i] < arr1[k-1])
			{
				arr1[k] = arr1[k-1];
				arr1[k-1] = arr[i];
				int n = k-1, m = 0;
				while (arr1[n] < arr1[n-1])
				{
					m = arr1[n];
					arr1[n] = arr1[n-1];
					arr1[n-1] = m;
					n--;
				}
			}
			k++;
			counter++;
		}
	}	
	
	cout << "Your sorted massif: ";
	for (int i = 0; i < counter; i++)
	   cout << arr1[i] << "; ";  
	cout << "\n";
	
	return counter;        
}

int main(int argc, char** argv)
{
	cout << "Write the size of massif: ";
	int size;
	cin >> size; cout << "\n";
	int arr[size];
	for (int i = 0; i < size; i++)
	{
		cout << "Write number: ";
		int a;
		cin >> a;
		arr[i] = a;
	}
	
	cout << "Your massif: ";
	for (int i = 0; i < size; i++)
	   cout << arr[i] << "; ";  
	cout << "\n";
	
    cout << "\nUnique elements: " << uniqueCheck(arr, size);
	
	return 0;
}

