#include <iostream> 
using namespace std;

int main() 
{ 
 int n = 100, x = 0, y = 0;
 while((n > 0)) 
 {
    if ((n%10 != 0) || (n == 100)) 
	{
       cout << "-";
 	   n -= 1;
	}
 	else if ((n%10 == 0) && (n != 100)) 
	{
       cout << '\n' << "-";
 	   n -= 1;
	}
 }
 
 cout << "\n" << "Your coordinate x: ";
 cin >> x;
 cout << "\n" << "Your coordinate y: ";
 cin >> y;
 
 int v = 100;
 while((v > 0)) {
    if ((v%10 != 0) || (v == 100)) {
    	if (v != x + y*10 - 10) {
	       cout << "-";  
	       v -= 1;
        }    
        else if (v == x + y*10 - 10) {
	       cout << "*"; 
		   v -= 1; 
        }    
	}
 	else if ((v%10 == 0) && (v != 100)) {
       cout << '\n' << "-";
 	   v -= 1;
	   } 
}
  

 cout << "\n";
	
 system("pause"); 
 return 0; 
} 

