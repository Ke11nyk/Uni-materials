#include <iostream>
using namespace std;

int GCD(int a, int b)  // a > b
{
	if (b == 0) 
	    return a; 
	return GCD(b, a % b);
}

void showNumber1(int num, int r)
{
	cout << "The number " << r << " is mutually prime with the number " << num << "\n";
}	

bool simplCheck(int n, int r)
{	
    if (GCD(n, r) != 1)
    	return false;	
	return true;	
}	

int main()
{
	int n, m, r;
	cout << "Enter the first number of your interval: ";
	cin >> n;
	cout << "Enter the last number of your interval: ";
	cin >> m;
	cout << "Enter the number R: ";
	cin >> r;
	
	for (int k = n; k < m; k++)     
        if (simplCheck(k, r))		
            showNumber1(k, r);
	
	return 0;
}




