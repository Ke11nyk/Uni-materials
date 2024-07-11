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

int task1() 
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
	    cout << "Invalid\n\n";
	else if (d == 0)
	{
		x1 = -b/2*a;
		cout << "X is " << x1 << "\n\n";
	}
	else if (d > 0) 
	{
		x1 = (-b + sqrt(d))/2*a;
		x2 = (-b - sqrt(d))/2*a;
		cout << "X1 is " << x1 << "\nX2 is " << x2 << "\n\n";
	}
	
	return 0;
}



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
		cout << "Area of the triagle is " << s << "\n\n";
	}
	else
	    cout << "The triangle doesn\'t exists\n\n";
}

int task2()
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



int task3()
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
	    cout << "Your number is narcissistic number\n\n";
	else if (sum != num)
	    cout << "Your number isn\'t narcissistic number\n\n";
	    
	return 0;
}



int task4()
{
	int point;
	cout << "Write the point of your number(points start from 0): ";
	cin >> point;
	
	if (point == 0)
		cout << "Your number is 0\n\n";
	else if (point == 1)
		cout << "Your number is 1\n\n";
	else
	{
		int num;
		num = (pow(2, point) - pow(-1, point))/3;
		cout << "Your number is " << num << "\n\n";
	}
	
	return 0;
}



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

int task5()
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
	    cout << "\nYour number isn\'t here!\n\n";
	else
		cout << "\nYour number is on the " << masyv(arr, 0, j-1, num) << " position\n\n";
	
	return 0;
}



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

int task6()
{
	int num, num1;
	cout << "Write the number of numbers: ";
	cin >> num;
	
	showNumber(2);	
	    
	for (int j = 3; j < num; j+=2)     
        if (simplicityCheck(j))		
            showNumber(j);
    cout << "\n";
	
	return 0;
}



int GCD(int a, int b)  
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

int task7()
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
    cout << "\n";
	
	return 0;
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
	
	cout << "Your sorted massif: ";
	for (int i = 0; i < counter; i++)
	   cout << arr1[i] << "; ";  
	cout << "\n";
	
	return counter;        
}

int task8()
{
	cout << "Write the size of massif: ";
	int size;
	cin >> size; cout << "\n";
	int arr[size];
	for (int i = 0; i < size; i++)
	{
		cout << "Write number: ";
		int a;
		cin >> a;
		arr[i] = a;
	}
	
	cout << "Your massif: ";
	for (int i = 0; i < size; i++)
	   cout << arr[i] << "; ";  
	cout << "\n";
	
    cout << "\nUnique elements: " << uniqueCheck(arr, size) << "\n\n";
	
	return 0;
}



int main(int argc, char** argv)
{
	int marker;
    do
    {
    	cout << ">>> make your choise!" <<endl;
    	cout << ">>> 1 for task 1\n" << ">>> 2 for task 2\n" << ">>> 3 for task 3\n" << ">>> 4 for task 4\n" << ">>> 5 for task 5\n" << ">>> 6 for task 6\n" << ">>> 7 for task 7\n" << ">>> 8 for task 8\n" << "0 to quit\n\n";
        cin >> marker;
        switch (marker)
	    {
	    case 1:
	    	cout << "\ntask 1" << endl;
	    	task1();
	    	break;
    	case 2:
         	cout << "\ntask 2" << endl;
         	task2();
		    break;
     	case 3:
	    	cout << "\ntask 3" << endl;
	    	task3();
	    	break;
    	case 4:
        	cout << "\ntask 4" << endl;
        	task4();
	    	break;
        case 5:
        	cout << "\ntask 5" << endl;
        	task5();
	    	break;
	    case 6:
        	cout << "\ntask 6" << endl;
        	task6();
	    	break;
	    case 7:
        	cout << "\ntask 7" << endl;
        	task7();
	    	break;
	    case 8:
        	cout << "\ntask 8" << endl;
        	task8();
	    	break;
	    case 0:
	    	break;
    	default:
    		cout << "Bad choise, try again!\n\n";
	    	break;
    	}
	}
    while (marker!=0);
	
	return 0;
}
