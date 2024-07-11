#include <iostream>
#include <math.h>
using namespace std;

char interface_a(float coef)
{
	if ((coef > 0) && (coef != 1))
	    cout << coef;
	else if ((coef < 0) && (coef != -1))
	    cout << "-" << -coef;
	else if (coef == -1)
	    cout << "-";
}

char interface_b(float coef)
{
	if ((coef > 0) && (coef != 1))
	    cout << " + " << coef;
	else if ((coef < 0) && (coef != -1))
	    cout << " - " << -coef;
	else if (coef == 1)
	    cout << " + ";
	else if (coef == -1)
	    cout << " - ";
}

char interface_c(float coef)
{
	if (coef > 0)
	    cout << " + " << coef;
	else if (coef < 0)
	    cout << " - " << -coef;
}

int main() 
{
	float a, b, c, d, x1, x2;
	
	cout << "Write coefficient a: "; 
	cin >> a;
	cout <<"Write coefficient b: "; 
	cin >> b;
	cout << "Write coefficient c: "; 
	cin >> c;
	cout << "\n" << "Your equation: \n"; interface_a(a); cout << "x^2"; interface_b(b); cout << "x"; interface_c(c); cout << " = 0\n\n";
	
	d = b*b - 4*a*c;
	cout << "Discriminant is " << d << "\n\n";
	if (d < 0)
	    cout << "Invalid";
	else if (d == 0)
	{
		x1 = -b/2*a;
		cout << "X is " << x1;
	}
	else if (d > 0) 
	{
		x1 = (-b + sqrt(d))/2*a;
		x2 = (-b - sqrt(d))/2*a;
		cout << "X1 is " << x1 << "\nX2 is " << x2;
	}
	
	return 0;
}
