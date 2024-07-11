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

    for(int column = 0; column < n; column++)              // �������� 0-�� ����� ������ �
    {
        float sum = A[0][column];                          // ����� ��� �������� �������� 0-��� ����� 
        for(int j = column - 1, i = 1; j >= 0; j--, i++)   // ��������� �� �������� ���������� ����� � ������� �� 1 ������ �� �����
            sum += A[i][j];                                // �� ���� ����� ��� �������
        
        B[column] = sum;                                   // ������ �������� ���� � ����� �
    }
    
    for(int line = 1; line < n; line++)                    // �������� (n-1)-�� �������� ������ �
    {
        float sum = A[line][r];                            // ����� ��� �������� �������� (n-1)-��� ���������
        for(int j = r - 1, i = line + 1; i < n; i++, j--)  // ��������� �� �������� ���������� ����� � ������� �� 1 ������ �� �����
            sum += A[i][j];                                // �� ���� ����� ��� �������
        
		B[line + r] = sum;                                 // ������ �������� ���� � ����� �
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
	
	for(int i = 0; i < strlen(str); i++)                            // �������� ����������� �����
	{
		if(str[i] == str[i-1])                                      // ���� ������ ����� ����� �� ���������
		{
			int count = 1;                                          // ����� ������� ������� 1
			for(int j = i + 1; str[j] == str[i]; j++, count++);     // ����� ������ �� ������� �
			
			cout << str[i] << ": " << count << " repeats" << endl;
			
			for(int g = i; g < strlen(str) - count; g++)                    
				str[g] = str[g + count];                            // ������ ������� ���� ������ �� �, �� ����� ���� �������
			str[strlen(str) - count] = '\0';                        // �������� ����� ����� �����
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
