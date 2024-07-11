#include <iostream>
using namespace std;
#include <string>
#include <math.h>

void numRead(int &num, string str)
{
	cout << "Input number: " << str << endl;
	
	for(int i = 0; i < str.length(); i++)
    	num += (str[i] - 48)*pow(10, str.length() - 1 - i);  
}

void toHex(int num, int arr[], int &i)
{
	for( ; num > 16; i++)
	{
		arr[i] = num%16;
		num /= 16;
	}
	arr[i] = num;
	
	cout << "Input number hex: ";
	for (int j = 0; j <= i; j++)
	    cout << hex << uppercase << arr[i - j];
	    cout << endl;
	i++;
}

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
	
	cout << "Unique figures: ";
	for (int i = 0; i < counter; i++)
	   cout << hex << uppercase << arr1[i] << "; ";  
	cout << "\n";
	
	return counter; 
}


int main(int argc, char** argv)
{
	string strNum;
	int num=0, max=0, arr[10];
	
	cout << "Enter your number: ";
	cin >> strNum;
	cout << endl;
	
	numRead(num, strNum);
	toHex(num, arr, max);
	
	cout << "\nUnique figures: " << uniqueCheck(arr, max);
	
	return 0;
}
