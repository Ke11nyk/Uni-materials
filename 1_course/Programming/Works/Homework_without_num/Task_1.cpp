#include <iostream>
#include<windows.h>

using namespace std;

int main(int argc, char** argv) {

	int x, chyslo[100], i = 0, j = 0, d = 0, c = 0; 

	cout << "Enter number: ";
	cin >> x;

     while (x > 0)
    {
    	chyslo[i] = x % 10;
    	x /= 10;
    	i++;
	}
    
    while (j < i/2)
    {
    	d = d + chyslo[j];
    	j++;
	}
	
	if (i%2 != 0)
        i += 1;
	int k = i/2;
	
	while ((j = k) and (k < i))
    {
    	c = c + chyslo[j];
    	k++;
	}
	
	if (c > d)
	    cout << "The sum of the first half of the digits is greater" << endl;
	if (c < d)
	    cout << "The sum of the second half of the digits is greater" << endl;
	if (c == d)  
	    cout << "The sums are equal" << endl;

	
	cout << "The sum of the first half: " << c << endl << "The sum of the second half: " << d;

	return 0;
}
