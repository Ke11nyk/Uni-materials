#include <iostream>
#include <cstring>

using namespace std;

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


int main(int argc, char** argv)
{	
	char str[50];
	
	cout << "Enter your string: "; 
	gets(str);
	
	bigMaker(str, 0);
	
	cout << endl << "Your string: " << str << endl;
	
	return 0;
}
