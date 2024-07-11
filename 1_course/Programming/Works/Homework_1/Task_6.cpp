#include <iostream>
#include <math.h>
using namespace std;

void showNumber(int num)
{
	cout << num << "   Number is Simple\n";
}	

bool simplicityCheck(int number)
{	
    for (int i = 3; i <= sqrt(number); i+=2)
    {
        if (number % i == 0)
    	  	return false;		      	  	     	  
	}	
	return true;	
}	

int main()
{
	int num, num1;
	cout << "Write the number of numbers: ";
	cin >> num;
	
	showNumber(2);	
	    
	for (int j = 3; j < num; j+=2)     
        if (simplicityCheck(j))		
            showNumber(j);
	
	return 0;
}
