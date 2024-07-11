#include <iostream>
using namespace std;
#include <string>
/* run this program using the console pauser or add your own getch, system("pause") or input loop */

const int size = 100;

void longZero(int array[size]) 
{
	for (int i = 0; i<size; i++)
	    array[i] = 0;
}

void longRead(int array[size], string str)
{
	cout << "Input number: " << str << endl;
	array[0] = str.length();
	// string = char[255] = {'a', 'b', ... '*',..., '1', '2', } 
	
	for(int i = array[0] - 1; i >= 0; i--)
    	array[str.length() - i] = str[i] - 48;   // - '0'
}

void longWrite(int array[size])
{
	for (int i = array[0]; i > 0; i--)
	    cout << array[i];
	cout << endl;    
}

int compare(int arr1[], int arr2[])  //  -1 if first larger; 0 if equeal; 1 if second larger
{
    if (arr1[0] > arr2[0]) 
	    return -1;
    if (arr2[0] > arr1[0]) 
	    return 1;
	
	int i;	
	for (i = arr1[0]; i >=1 and arr1[i] == arr2[i]; i--) ;
	 
	if (i == 0)
	    return 0;
	if (arr1[i] > arr2[i]) 
	    return -1;
    if (arr2[i] > arr1[i]) 
	    return 1;
}

void longSum(int arr1[], int arr2[], int arr3[])
{
	int max;
	if (arr1[0] > arr2[0])
	    max = arr1[0];
    else 
        max = arr2[0];
    
    for (int i = 1; i <= max; i++)
    {
    	arr3[i] = arr1[i] + arr2[i] + arr3[i];
    	
    	if (arr3[i] > 9)
    	{
    		arr3[i+1]++;  
    		arr3[i] %= 10;
		}
	}
	
	if (arr3[max+1] != 0)
	    arr3[0] = max+1;
	else     
	    arr3[0] = max;   
}

void longRiz(int arr1[], int arr2[], int arr3[], int arr[])
{
	if (compare(arr1, arr2) == 0) // €кщо друге число б≥льше, то м≥н€ю числа м≥сц€ми
	{
		arr3[0] = 1;
        arr3[1] = 0;
	}
	else
	{
	    int max; 
	    if (compare(arr1, arr2) == 1) // €кщо друге число б≥льше, то м≥н€ю числа м≥сц€ми
	    {
		    arr = arr1;
		    arr1 = arr2;
		    arr2 = arr;
	    }
	
	    max = arr1[0];
    
        for (int i = 1; i <= max; i++)
        {
    	    if(arr1[i] > arr2[i])
		        arr3[i] = arr1[i] - arr2[i];
		    if(arr1[i] < arr2[i])
		    {
			    arr3[i] = arr1[i] + 10 - arr2[i];
			    arr1[i+1]--;
		    }
	    }
	
	    for ( ; arr3[max] == 0 && arr3[max-1] == 0; max--) ;    
	    arr3[0] = max;
	    if(arr3[max] == 0) // дл€ менших чисел чомусь виводивс€ нуль перед числом, тому роблю ще одну перев≥рку
	        arr3[0]--; 	
	}   
}

void longDob(int arr1[], int arr2[], int arr3[])
{   
    int k = 1, n; 
	/* коеф k перем≥щаЇ з кожним циклом початкову ком≥рку дл€ заповненн€ 
	(перем≥щаЇмо добуток цифри другого числа на перше число по дес€ткам); 
	коеф n, власне, номер ком≥рок п≥сл€ початковоњ */
	                 
    for(int i = 1; i <= arr2[0]; i++)
    {
    	n = k;
    	for(int j = 1; j <= arr1[0]; j++)
    	{
    		arr3[n] += arr1[j]*arr2[i];
		    n++;
		}
		k++;
	}
	
	if (arr3[n] == 0) // перв≥рка к≥лькост≥ цифр
	    n -= 1;  
	arr3[0] = n;
	
	for(int i = 1; i <= arr3[0]; i++)   // так €к в цикл≥ добутку € не сортував ком≥рки,аби там лишалось по одн≥й цифр≥, цей цикл виконуЇ саме цю функц≥ю
		if (arr3[i] > 9)
			{
    		    arr3[i+1] += arr3[i] / 10;   
    		    arr3[i] %= 10;
		    }
	
	if (arr3[n+1] != 0) // повторна перев≥рка к≥лькост≥ цифр
	    arr3[0] = n+1;
}

void longChas(int arr1[], int arr2[], int arr3[])
{   
    if (compare(arr1, arr2) == 1) 
	    exit(1);
	
	if (compare(arr1, arr2) == 0) 
	{
		arr3[0] = 1;
        arr3[1] = 1;
	}
	
	if (compare(arr1, arr2) == -1) 
	{
		
	}

}

int main(int argc, char** argv) {
	
	int longArr1[size], longArr2[size], longArr3[size], Arr[size];
	string Str1, Str2; char sign;
	
	longZero(longArr1);
	longZero(longArr2);
	longZero(longArr3);
	longZero(Arr);
	
	cout << "Enter your long math example:  "<< endl << ">>>   ";
	cin >> Str1 >> sign >> Str2;
	
	cout << endl;
	longRead(longArr1, Str1);
	cout << "Input sign: " << sign << endl;
    longRead(longArr2, Str2);
    cout << endl;
    
    if (compare(longArr1, longArr2)==0) 
        cout << "they are equal!"<< endl;
    else if (compare(longArr1, longArr2)==-1) 
         cout << "first is bigger!"<< endl;    
	     else
	         cout << "second is bigger!"<< endl;   
	
	
	cout << "Result: ";
	
	if(sign == '+')
	    longSum(longArr1, longArr2, longArr3);
	if(sign == '*')
	    longDob(longArr1, longArr2, longArr3);
	if(sign == '-')
	{
		if (compare(longArr1, longArr2) == 1)
	        cout << "-";
	    longRiz(longArr1, longArr2, longArr3, Arr);
	}
	    
	longWrite(longArr3);
	
	return 0;
}
