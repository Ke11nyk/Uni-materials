#include <iostream>
#include <windows.h>
#include <cstring>

using namespace std;

void sort(char num[], char nums[][50], int& koef, int& j)
{	
	int i=0;
	while(num[koef] != ' ')
	    {
	    	if(num[koef] == '\0')
	    	    break;
		    else
		    {
		    	nums[j][i] = num[koef];
		        koef++;
		        i++;
			}	
	    }
	nums[j][i] = '\0';
	
	koef++;
	j++;
}

void numMaker(char nums[][50], char numsNum[][14], int& n, int& number, int roz, int koef)
{
	if(strlen(nums[n])>2)
		for(int i=0; i<roz; i++)
		{
			if(strlen(numsNum[i]) == strlen(nums[n]))
			{
				bool k = true;
				for(int j=0; j<strlen(nums[n]); j++)
					if(numsNum[i][j] != nums[n][j])
						k = false;
				if(k)
				{
					number += i*koef;
					n++;
				}
			}	
		}
}

int returnNumber(char nums[][50], int max)
{
	char numsNum1[][14] = {"нуль", "один", "два", "три", "чотири", "п\'ять", "шість", "сім", "вісім", "дев\'ять",
	                     "десять", "одинадцять", "дванадцять", "тринадцять", "чотирнадцять", "п\'ятнадцять", "шістнадцять",
						 "сімнадцять", "вісімнадцять", "дев\'ятнадцять"};
	char numsNum2[][14] = {"0", "0", "двадцять", "тридцять", "сорок", "п\'ятдесят", "шістдесят", "сімдесят", "вісімдесят", "дев\'яносто"};
	char numsNum3[][14] = {"0", "сто", "двісті", "триста", "чотириста", "п\'ятсот", "шістсот", "сімсот", "вісімсот", "дев\'ятсот"};
	
    int number=0, n=0;
    bool k = false;
    char minus[] = "мінус";
    
	if(strlen(nums[0]) == strlen(minus))
	{
		for(int j=0; j<strlen(nums[0]); j++)
			if(minus[j] != nums[0][j])
				break;
		k = true;
		n++;
	}

	numMaker(nums, numsNum3, n, number, 10, 100);	
	if(n < max)	
		numMaker(nums, numsNum2, n, number, 10, 10);
	if(n < max)
	    numMaker(nums, numsNum1, n, number, 20, 1);
	
	if(k == true)
    	number*=-1;
    
	if(n == max)
		return number;

	return 99;
}

int main(int argc, char** argv)
{
	SetConsoleCP(1251);
    SetConsoleOutputCP(1251);
	
	char num[50], nums[10][50];
	int koef=0, maxNums=0, number=0;
	cout << "Enter your number: "; 
	gets(num);
	cout << endl << "Your number: " << num << endl;
	
    while(koef <= strlen(num))
	    sort(num, nums, koef, maxNums);
    
	cout << endl << "Your number: " << num << endl;
	cout << endl << "Your numbers: "; 
	
	for(int k=0; k < maxNums; k++)
	    cout << nums[k] << ";";
	cout << endl;	
	
	cout << "Your number gg: "<< returnNumber(nums, maxNums) << ";";

	    
	return 0;
}