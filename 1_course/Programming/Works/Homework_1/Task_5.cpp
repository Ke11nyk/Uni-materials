#include <iostream>
using namespace std;

int masyv(int arr[], int first, int last, int num)
{
	if (arr[first] < arr[last])
	    return -1;
	
	if (num == arr[first])
	    return first + 1;
	else if (num == arr[last])
	    return last + 1;
	
	int mid = (first + last)/2;
		
	if (num == arr[mid])
		return mid + 1;
	else if (num > arr[mid])
		return masyv(arr, first + 1, mid - 1, num);
	else if (num < arr[mid])
		return masyv(arr, mid + 1, last - 1, num);
}

int main()
{
	int i, j, num;
	cout << "Enter the number of numbers in the array: ";
	cin >> i;
	int arr [i];
	cout << "Enter the number to search: ";
	cin >> num;
	
	while (j < i)
	{
		arr[j] = (i - j)*2;
		cout << arr[j] << "; ";
		j++;
	}

	if ((num > arr[0]) || (num < arr[j-1]) || ((masyv(arr, 0, j-1, num) == -1)))
	    cout << "\nYour number isn\'t here!";
	else
		cout << "\nYour number is on the " << masyv(arr, 0, j-1, num) << " position";
	
	return 0;
}
