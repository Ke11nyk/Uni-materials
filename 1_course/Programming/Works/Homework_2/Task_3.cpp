#include <iostream>

using namespace std;

bool palindrom(int num)
{
	int k = 0, polim[10];
	for( ; num != 0; k++)               // розкладаю число на цифри
		{
			polim[k] = num % 10;
			num /= 10;
		}
			
	for(int l=0, m=k-1; m>l; l++, m--)   // перев≥р€ю чи пол≥ндром
		if(polim[l] != polim[m])
			return false;
	
	return true;
}


int main(int argc, char** argv)
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
