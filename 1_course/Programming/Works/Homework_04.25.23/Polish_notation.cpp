#include <iostream>
#include <math.h>
using namespace std;

struct sign 
{
	char ch;
	sign* next;
};

void signAdd(sign** head, char ch) 
{

	sign* temp = new sign;
	temp->ch = ch;
	temp->next = *head;
	*head = temp;
}

char signGet(sign** head) 
{
	{
		char ch = (*head)->ch;
		sign* temp = *head;
		*head = (*head)->next;
		delete temp;
		return ch;
	}
}

void signShow(sign *head)
{
	cout << "sign: ";
	while (head)
	{
		cout << head->ch;
		head = head->next;
	}
	cout << endl;
}

void makePOLIZ(char in[100], char out[100]) 
{
	sign* head = NULL;
	int i = 0, j = 0;

	while (in[i])
	{
		signShow(head); // додав, аби бачити які знаки в стекові знаходяться
		
		while (in[i] and in[i] > 47 and in[i] < 58)
		{
			out[j] = in[i];  
			i++; j++;
		}
		out[j] = ' ';
		j++;
			
		if (in[i] == ')')
		{
			while (head and head->ch != '(')
			{
				out[j] = signGet(&head);
				j++;
			}

			signGet(&head); // щоб видалити '(' зі стеку
			i++;
		}

		
		while (head and !((head->ch == '(') or (in[i] == '(') or (in[i] == '!') or (in[i] == 's') or (in[i] == 'c') or (in[i] == 't')
			or (((head->ch == '+') or (head->ch == '-')) and ((in[i] == '*') or (in[i] == '/')))))
			// у нас всього 4 випадка коли не виймаємо знак зі стеку, тому зробив таку умову
			// також додались умови щоб зупинялось коли в стекові дойшло до дужки, або коли в нас в масиві in розглядається дужка
			// + факторіал/тригонометричні функції
		{
			out[j] = signGet(&head);
			j++;
		}

		if (in[i] != '\0') signAdd(&head, in[i]);

		if ((in[i] == 's') or (in[i] == 'c') or (in[i] == 't')) i += 2; // щоб не заморочуватись з наступними буквами 
																		// хотів ще котангенс, але передумав через те, що перша буква співпадає з косинусом :)
		i++;
	}

	while (head)
	{
		out[j] = signGet(&head);
		j++;
	}
}


struct number 
{
	float num;
	number* next;
};

void numAdd(number** head, float num) 
{
	number* temp = new number;
	temp->num = num;
	temp->next = *head;
	*head = temp;
}

float numGet(number** head) 
{
	float num = (*head)->num;
	number* temp = *head;
	*head = (*head)->next;
	delete temp;
	return num;
}

float readNum(char in[100], int* i)
{
	float num = 0;
	while (in[*i] > 47 and in[*i] < 58)
	{
		num = num * 10 + (in[*i] - 48);
		(*i)++;
	}
	return num;
}

float factorial(int num)
{
	float factorial = 1;

	for (int i = 1; i <= num; ++i)
	{
		factorial *= i; 
	}
	return factorial; 
}

float countPOLIZ(char in[100]) 
{

	cout << "In count: " << in << endl;
	number* head = NULL;
	for (int i = 0; in[i]; i++)
	{
		if (in[i] != ' ')
			if (in[i] > 47 and in[i] < 58)    
				numAdd(&head, readNum(in, &i));
			else
			{
				float result;
				if (in[i] == '+')
					result = numGet(&head) + numGet(&head);
				else if (in[i] == '-')
					result = - numGet(&head) + numGet(&head);     // так як по числам йдемо в зворотньому порядку, то відповідно мінус перед першим 
				else if (in[i] == '*')
					result = numGet(&head) * numGet(&head);   
				else if (in[i] == '/')
					result = 1 / numGet(&head) * numGet(&head);   // з тої ж причини ділю 1 на перше число і множу на друге
				else if (in[i] == '!')
					result = factorial(numGet(&head));
				else if (in[i] == 's')
					result = sin(numGet(&head));
				else if (in[i] == 'c')
					result = cos(numGet(&head));
				else if (in[i] == 't')
					result = tan(numGet(&head));

				numAdd(&head, result);
			}
	}

	return numGet(&head);
}


int main() 
{

	char in[100] = "21/(2!+3*1+4/2+sin0)+66+3!";
	char out[100] = "";

	cout << "input: " << endl;
	for (int i = 0; in[i]; i++)
		cout << in[i];
	cout << endl;

	makePOLIZ(in, out);

	cout << "OUTput: " << out << endl;
	cout << "result: " << countPOLIZ(out);

	return 0;
}
