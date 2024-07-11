#include <iostream>
#include <string.h>     
using namespace std;

struct node 
{
	int num;
	node* left;
	node* right;
};

struct strNode 
{
	string str;
	strNode* left;
	strNode* right;
};

struct nodeStack
{
	int num;
	nodeStack* next;
};

void addNodeS(node* nod, int data)
{
	node** current = new node*;
	current = (data < nod->num) ? &(nod->left) : &(nod->right);  

	if (*current) addNodeS(*current, data);
	else
	{
		node* newNode = new node;
		newNode->num = data;
		newNode->left = newNode->right = NULL;
		*current = newNode;
	}
}

void addNodeNS(node* nod, int data, int side) 
{
	cout << "side: " << side << "  data: " << data << endl; // виводжу side i data, аби намалювати створене дерево

	node** current = new node*;
	current = (side == 0) ? &(nod->left) : &(nod->right);

	if (*current) addNodeNS(*current, data, rand() % 2);
	else
	{
		node* newNode = new node;
		newNode->num = data;
		newNode->left = newNode->right = NULL;
		*current = newNode;
	}
}

void showBinary(node* nod) 
{
	if (nod)
	{
		showBinary(nod->left);
		cout << nod->num << "; ";
		showBinary(nod->right);
	}
}

void createTree(node** root)
{
	int arr[12] = { 4,9,7,15,12,1,20,3,5,8,13,16 };
	(*root)->left = (*root)->right = NULL;
	(*root)->num = 10;

	for (int i = 0; i < 12; i++) addNodeS(*root, arr[i]);
}

///////////////TASK1///////////////

void task1S(node* nod, int min, int max, int* count, int* sum)
{
	if (nod)
	{
		if (nod->num >= max)  // якщо число більше-рівне за проміжок, то йду лише наліво
		{
			if (nod->num == max)
			{
				(*count)++;
				*sum += nod->num;
			}
			task1S(nod->left, min, max, count, sum);
		}
		else if (nod->num <= min)   // якщо число менше-рівне за проміжок, то йду лише направо
		{
			if (nod->num == min) 
			{
				(*count)++;
				*sum += nod->num;
			}
			task1S(nod->right, min, max, count, sum);
		}
		else   // інакше йду і наліво, і направо
		{
			(*count)++;
			*sum += nod->num;
			task1S(nod->left, min, max, count, sum);
			task1S(nod->right, min, max, count, sum);
		}
	}
}

void task1NS(node* nod, int min, int max, int* count, int* sum)
{
	if (nod)   // просто проходжу по дереву і знаходжу числа в межах
	{
		if (nod->num >= min && nod->num <= max)
		{
			(*count)++;
			*sum += nod->num;
		}
		task1NS(nod->left, min, max, count, sum);
		task1NS(nod->right, min, max, count, sum);
	}
}

void task1()
{
	int arr[12] = { 4,9,7,15,12,1,20,3,5,8,13,16 };

	node* rootS = new node;
	createTree(&rootS);

	node* rootNS = new node;
	rootNS->left = rootNS->right = NULL;
	rootNS->num = 10;
	for (int i = 0; i < 12; i++) addNodeNS(rootNS, arr[i], rand() % 2);

	cout << "--------------------------------------" << endl << endl << "Task 1" << endl << endl << "Sorted:" << endl;
	showBinary(rootS);
	cout << endl;

	int count = 0, sum = 0;
	task1S(rootS, 1, 5, &count, &sum);
	cout << "S count in range 1 to 5: " << count << endl << "S sum: " << sum;

	cout << endl << endl << "Unsorted:" << endl;
	showBinary(rootNS);
	cout << endl;

	count = sum = 0;
	task1NS(rootNS, 1, 5, &count, &sum);
	cout << "NS count in range 1 to 5: " << count << endl << "S sum: " << sum << endl << endl << "--------------------------------------" << endl << endl;
}

///////////////TASK2///////////////

void addStrNode(strNode* nod, string data) 
{
	strNode** current = new strNode*;
	current = (data[0] < nod->str[0]) ? &(nod->left) : &(nod->right); // сортую по юнікод кодам перших букв

	if (*current) addStrNode(*current, data);
	else
	{
		strNode* newNode = new strNode;
		newNode->str = data;
		newNode->left = newNode->right = NULL;
		*current = newNode;
	}
}

void showStrBinary(strNode* nod) 
{
	if (nod)
	{
		showStrBinary(nod->left);
		cout << nod->str << "; ";
		showStrBinary(nod->right);
	}
}

void task2()
{
	string daysOfWeek[] = { "Sunday", "Monday", "Tuesday",
	"Wednesday", "Thirsday", "Friday", "Saturday" };

	strNode* root = new strNode;
	root->left = root->right = NULL;
	root->str = "Day";

	for (int i = 0; i < 7; i++) addStrNode(root, daysOfWeek[i]);

	cout << "Task 2" << endl << endl;
	showStrBinary(root);
	cout << endl << endl << "--------------------------------------" << endl << endl;
}

///////////////TASK3///////////////

void lessV(node* nod, int v, int* max)
{
	if (nod)
	{
		if (nod->num >= v) lessV(nod->left, v, max); // якщо число більше-рівне за v, то йду наліво
		else  // якщо число менше за v, то присвоюю max число і йду направо
		{
			*max = nod->num;
			lessV(nod->right, v, max);
		}
	}
}

void task3()
{
	node* root = new node;
	createTree(&root);

	cout << "Task 3" << endl << endl;
	showBinary(root);
	cout << endl;

	int max = -1;
	lessV(root, 14, &max);
	cout << "Max min for 14: " << max << endl << endl << "--------------------------------------" << endl << endl;
}

///////////////TASK4///////////////

void addStack(nodeStack** head, int value)
{
	nodeStack* temp;
	temp = new nodeStack;

	temp->num = value;
	temp->next = *head;
	*head = temp;
}

void showStack(nodeStack* head)
{
	if (head)
	{
		cout << "Your stack is: " << endl;
		while (head)
		{
			cout << head->num << endl;
			head = head->next;
		}
	}
	else cout << "No way" << endl << endl;
}

void findV(node* nod, int v, bool* find, nodeStack** head)
{
	if (nod)
	{
		if (nod->num > v) findV(nod->left, v, find, head);  
		if (nod->num < v) findV(nod->right, v, find, head);
		if (nod->num == v)
		{
			*find = true;   //ввів булеву змінну, аби створювати стек лише якщо знайдено число рівне v
			addStack(head, nod->num);
			return;
		}

		if (*find) addStack(head, nod->num);
	}
}

void task4()
{
	nodeStack* head = new nodeStack;
	head = NULL;
	bool find = false;

	node* root = new node;
	createTree(&root);

	cout << "Task 4" << endl << endl;
	showBinary(root);
	cout << endl;

	findV(root, 8, &find, &head);
	showStack(head);
}


int main() 
{
	task1();
	task2();
	task3();
	task4();

	return 0;
}