#include <iostream>
#include <cstring>

using namespace std;
 
int main()
{
	char str[50];
	
	cout << "Enter your string: "; 
	gets(str);
	
	for(int i = 0; i < strlen(str); i++)
	{
		if(str[i] == str[i-1])
		{
			int count = 1;
			for(int j = i + 1; str[j] == str[i]; j++, count++);
			
			cout << str[i] << ": " << count << " repeats" << endl;
			
			for(int g = i; g < strlen(str); g++)
				str[g] = str[g + count];
		}
	}
	
	
	cout << endl << "Your string: " << str << endl;

    return 0;
}
