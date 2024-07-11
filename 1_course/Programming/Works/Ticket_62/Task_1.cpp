#include <iostream>

using namespace std;

int main()
{
    int const m = 4, n = 3;
    int counter = 0;

    int arr[m][n] = {{1, 2, 4},
                     {1, 2, 3},
                     {2, 5, 8},
                     {1, 1, 1}
                    };

    for (int i = 0; i < m; i++)
    {
        int max = arr[i][0], sum = arr[i][0];
        
        for (int j = 1; j < n; j++)
        {
            sum += arr[i][j];
            if (arr[i][j] > max)
                max = arr[i][j];
        }

        if (max > sum - max)
            counter++;
    }

    cout << "Counter: " << counter;
}