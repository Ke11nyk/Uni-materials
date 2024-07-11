#include <iostream>
#include <cstring>

using namespace std;

int task1()
{
	int n = 4, r = n-1;
    float A[n][n] = {{ 0, 1.5,   2,  3},
                     { 4,   5,   6,  7},
                     { 8,   9,  10, 11},
                     {12,  13, -14, 15}};
    
    float B[2*n-1];

    for(int column = 0; column < n; column++)              // проходжу 0-ий рядок масиву А
    {
        float sum = A[0][column];                          // надаю сумі значення елемента 0-ого рядка 
        for(int j = column - 1, i = 1; j >= 0; j--, i++)   // переходжу до елемента наступного рядка з номером на 1 меншим за даний
            sum += A[i][j];                                // до суми додаю цей елемент
        
        B[column] = sum;                                   // заношу значення суми в масив В
    }
    
    for(int line = 1; line < n; line++)                    // проходжу (n-1)-ий стовпчик масиву А
    {
        float sum = A[line][r];                            // надаю сумі значення елемента (n-1)-ого стовпчика
        for(int j = r - 1, i = line + 1; i < n; i++, j--)  // переходжу до елемента наступного рядка з номером на 1 меншим за даний
            sum += A[i][j];                                // до суми додаю цей елемент
        
		B[line + r] = sum;                                 // заношу значення суми в масив В
    }

	for(int i=0; i<2*n-1; i++)
	{
		cout << B[i] << "; ";
	}	
	cout << endl << endl;

	return 0;
}


int task2()
{	
	char str[50];
	
	cout << "Enter your string: "; 
	fflush(stdin);
	gets(str);
	
	for(int i = 0; i < strlen(str); i++)                            // проходжу посимвольно рядок
	{
		if(str[i] == str[i-1])                                      // якщо символ такий самий як попередній
		{
			int count = 1;                                          // число повторів дорівнює 1
			for(int j = i + 1; str[j] == str[i]; j++, count++);     // рахую скільки ще повторів є
			
			cout << str[i] << ": " << count << " repeats" << endl;
			
			for(int g = i; g < strlen(str) - count; g++)                    
				str[g] = str[g + count];                            // замінюю символи після даного на ті, що йдуть після повторів
			str[strlen(str) - count] = '\0';                        // позначаю новий кінець рядку
		}
	}
	
	
	cout << endl << "Your string: " << str << endl << endl;
	
	return 0;
}


int main(int argc, char** argv)
{
	int marker;
    do
    {
    	cout << ">>> make your choise!" << endl;
      	cout << ">>> 1 for task 1\n" << ">>> 2 for task 2\n" << "0 to quit\n\n";
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
