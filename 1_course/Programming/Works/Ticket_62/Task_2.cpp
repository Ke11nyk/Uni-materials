#include <iostream>
#include <windows.h>
#include <cstring>

using namespace std;

int main()
{
    SetConsoleCP(1251);
    SetConsoleOutputCP(1251);

    char str[] = "D:/path/to/Task_2.cpp";
    char str1[20], str2[20];

    int i = 0, point, line;
    for (i; str[i] != ':'; i++)
        str1[i] = str[i];

    

    for (int j = strlen(str) - 1; j != '3'; j--)
    {
        if (str[j] == '.')
            point = str[j];
        if (str[j] == '3')
            line = str[j];
    }

    for (int j = line + 1; j < point; i++)
        str1[i] = str[j];

    str1[i] = '\0';

    cout << str1;
}
