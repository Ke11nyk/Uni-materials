#include <iostream>
#include <math.h>
using namespace std;

int storona(float n, float m, float r)
{
	if (n < m + r)
	{
		float p, s;
		cout << "The triangle exists\n\n";
		if (m*m + r*r - n*n > 0)
		    cout << "It is an acute-angled triangle\n\n";
		else if (m*m + r*r - n*n < 0)
		    cout << "It is an obtuse triangle\n\n";
		else if (m*m + r*r - n*n == 0)
		    cout << "It is a right triangle\n\n";   
		p = (n + m + r)/2;
		s =  sqrt(p * (p - n) * (p - m) * (p - r));
		cout << "Area of the triagle is " << s;
	}
	else
	    cout << "The triangle doesn\'t exists\n\n";
}

int main()
{
	float a, b, c;
	
	cout << "Write first side: "; 
	cin >> a;
	cout <<"Write second side: "; 
	cin >> b;
	cout << "Write third side: "; 
	cin >> c;
	cout << "\n";
	
	if ((a > b) && (a > c))
        storona(a, b, c);
    if ((b > a) && (b > c))
        storona(b, a, c);
    if ((c > a) && (c > b))
        storona(c, a, b);
        
    return 0;
}
