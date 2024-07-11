#include <iostream>
#include <math.h>
using namespace std;

int main()
{
	int arr[10], num;
	cout << "Write your number: ";
	cin >> num;
	
	int i = 0;
	int num1 = num;
	while (num1 > 0)
	{
		arr[i] = num1 % 10;
		num1 = num1/10;
		i++;
	}
	
    int sum = 0;
    int j = i - 1;
	while (j >= 0)
	{
		sum += pow(arr[j], i);
		j--;
	}
	
	if (sum == num)
	    cout << "Your number is narcissistic number";
	else if (sum != num)
	    cout << "Your number isn\'t narcissistic number";
	    
	return 0;
}
