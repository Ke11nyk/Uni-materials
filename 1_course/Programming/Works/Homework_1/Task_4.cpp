#include <iostream>
#include <math.h>
using namespace std;

int main()
{
	int point;
	cout << "Write the point of your number(points start from 0): ";
	cin >> point;
	
	if (point == 0)
		cout << "Your number is 0";
	else if (point == 1)
		cout << "Your number is 1";
	else
	{
		int num;
		num = (pow(2, point) - pow(-1, point))/3;
		cout << "Your number is " << num;
	}
	
	return 0;
}
