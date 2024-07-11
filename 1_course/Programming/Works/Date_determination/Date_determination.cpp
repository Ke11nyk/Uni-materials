#include <iostream>

using namespace std;

int main()
{
    int datenum;

    do
    {
        cout << "Enter your number of your day: ";
        cin >> datenum;
        if (datenum > 365)
            cout << "Try again" << endl;
    } while (datenum > 365);
    
    cout << "Enter your number: " << datenum;


    return 0;
}