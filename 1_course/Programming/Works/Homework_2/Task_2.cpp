#include <iostream>
#include <cstring>

using namespace std;

void bigMaker(char str[], int i)
{	
	for( ; i < strlen(str); i++)
	{ 
		if((str[0] > 96) && (str[0] < 123))                          //���� ������ ������ � ����� ������                                                                                      
				str[0] -= 32;
		
		if((str[i] == 32) || (str[i] == 44) || (str[i] == 46))       //���� ������ == ' ', ��� ',', ��� '.'
		{
			i++;
			if((str[i] > 96) && (str[i] < 123))                      //���� ��������� ������ � ����� ������                                                                                      
				str[i] -= 32;                                        //������ �� ��������� �������
			if((str[i] == 32) || (str[i] == 44) || (str[i] == 46))   //���� ��������� ������ == ' ', ��� ',', ��� '.'
				bigMaker(str, i);                                    //������������ � �������
		}
	}
}


int main(int argc, char** argv)
{	
	char str[50];
	
	cout << "Enter your string: "; 
	gets(str);
	
	bigMaker(str, 0);
	
	cout << endl << "Your string: " << str << endl;
	
	return 0;
}
