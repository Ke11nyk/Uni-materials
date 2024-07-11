#include <iostream>
#include <cstring>

using namespace std;

int task1()
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


void bigMaker(char str[], int i)
{	
	for( ; i < strlen(str); i++)
	{ 
		if((str[0] > 96) && (str[0] < 123))                          //якщо перший символ є малою буквою                                                                                      
				str[0] -= 32;
		
		if((str[i] == 32) || (str[i] == 44) || (str[i] == 46))       //якщо символ == ' ', або ',', або '.'
		{
			i++;
			if((str[i] > 96) && (str[i] < 123))                      //якщо наступний символ є малою буквою                                                                                      
				str[i] -= 32;                                        //робимо її відповідною великою
			if((str[i] == 32) || (str[i] == 44) || (str[i] == 46))   //якщо наступний символ == ' ', або ',', або '.'
				bigMaker(str, i);                                    //перезаходимо у функцію
		}
	}
}

int task2()
{	
	char str[50];
	
	cout << "Enter your string: "; 
	fflush(stdin);
	gets(str);
	
	bigMaker(str, 0);
	
	cout << endl << "Your string: " << str << endl;
	
	return 0;
}


bool palindrom(int num)
{
	int k = 0, polim[10];
	for( ; num != 0; k++)               // розкладаю число на цифри
		{
			polim[k] = num % 10;
			num /= 10;
		}
			
	for(int l=0, m=k-1; m>l; l++, m--)   // перевіряю чи поліндром
		if(polim[l] != polim[m])
			return false;
	
	return true;
}


int task3()
{	
	int max, qpals = 0;
	
	cout << "Enter your max number: "; 
	cin >> max;
	
	int nums[max];
	
	for(int i=1; i <= max; i++)
		if((palindrom(i) && palindrom(i*i)))
		{
			nums[qpals] = i;
			qpals++;
		}
	
	cout << endl << "Your numbers: " << endl;
	for(int i; i < qpals; i++)
		cout << nums[i] << "; ";
	
	return 0;
}


int main(int argc, char** argv)
{
	int marker;
    do
    {
    	cout << ">>> make your choise!" << endl;
      	cout << ">>> 1 for task 1\n" << ">>> 2 for task 2\n" << ">>> 3 for task 3\n"  << "0 to quit\n\n";
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
