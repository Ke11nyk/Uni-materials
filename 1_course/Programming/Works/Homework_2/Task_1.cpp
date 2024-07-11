#include <iostream>

using namespace std;

int main()
{
	int n = 4, m = 6;
	int arrMax[n];
	int arrNumbers[][4] = {{1,  78,  3,  4},
		          		   {6,  10,  7,  9},
	              		   {12, 24, 72,  2},
	                       {19, 56, 68, 97},
	                       {90, 56, 87, 34},
						   {22, 46, 77, 95}};


	int max, min;
	for (int i = 0; i < n; i++)          
	{
		max = arrNumbers[0][i];
		for (int j=1; j < m; j++)
		{
			if (arrNumbers[j][i] > max)
				max = arrNumbers[j][i];   // визначаю максимальне значення у стовпчиках 
		}
		
		if((min > max) || (i == 0))       // надаю мінімальному значенню значення першого максимум, потім порівнюю з наступними
			min = max;
	}
		
	cout << "Your min number is " << min;	

	return 0;
}
