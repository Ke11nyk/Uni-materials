#include <iostream>

using namespace std;

int main(int argc, char const *argv[])
{
    float a, b, c, d, e, f;
    
    cout << "Enter your first equal: ";
    cin >> a >> b >> e;
    cout << "Enter your first equal: ";
    cin >> d >> c >> f;

    float x, y;

    x = (f*b-e)/(b*d-a);
    y = (e - a*x)/b;

    cout << "x: " << x << endl;
    cout << "y: " << y << endl;

    return 0;
}
