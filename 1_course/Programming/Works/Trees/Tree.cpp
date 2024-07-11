#include <iostream>
using namespace std;

struct node 
{
	int num;
	node* offspring[3];
};

void addNode(node** nod, int number)
{
	*nod = new node;
	for (int i = 0; i < 3; i++) (*nod)->offspring[i] = NULL;
	(*nod)->num = number;
}

void createSimpleTree(node* root)
{
	addNode(&(root->offspring[0]), 2);
	addNode(&(root->offspring[0]->offspring[0]), 5);

	addNode(&(root->offspring[1]), 3);
	addNode(&(root->offspring[1]->offspring[0]), 6);
	addNode(&(root->offspring[1]->offspring[1]), 7);

	addNode(&(root->offspring[2]), 4);
	addNode(&(root->offspring[2]->offspring[0]), 8);
	addNode(&(root->offspring[2]->offspring[0]->offspring[0]), 9);
	addNode(&(root->offspring[2]->offspring[0]->offspring[1]), 10);
	addNode(&(root->offspring[2]->offspring[0]->offspring[2]), 11);
}

void show(node* nod, int level)
{
	if (!nod)
		return;

	cout << level << ", " << nod->num << ";  ";

	for (int i = 0; i < 3; i++)
		show(nod->offspring[i], level + 1);
}

void findHeight(node* nod, int current, int* max)
{
	if (!nod)
		return;

	if (current > *max)
		*max = current;

	for (int i = 0; i < 3; i++) findHeight(nod->offspring[i], current + 1, max);
}

int main() 
{
	node* root;
	root = new node;
	root->num = 1;
	root->offspring[0] = root->offspring[1] = root->offspring[2] = NULL;

	createSimpleTree(root);

	cout << "(";
	show(root, 1);
	cout << ")" << endl;

	int max = 0;
	findHeight(root, 0, &max);
	cout << "height: " << max << endl << endl;

	return 0;
}