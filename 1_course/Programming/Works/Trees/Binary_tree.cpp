#include <iostream>
using namespace std;

struct node 
{
	int num;
	node* left;
	node* right;
};

void addNode(node* nod, int data) 
{
	node** current = new node*;
	current = (data < nod->num) ? &(nod->left) : &(nod->right);   //зробив умову лише для визначення правого чи 
																  //лівого посилання вузла, бо код для обидвох був ідентичним
	if (*current) addNode(*current, data);
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

void findHeight(node* nod, int current, int* max)
{
	if (!nod)
		return;

	if (current > *max)
		*max = current;

	findHeight(nod->left, current + 1, max);
	findHeight(nod->right, current + 1, max);

}

void showByBrakets(node* nod, bool sibling)
{
	cout << nod->num;

	if (nod->right || nod->left)
	{
		cout << " (";
		if (!nod->right) showByBrakets(nod->left, false);
		else
		{
			if (nod->left) showByBrakets(nod->left, true);
			showByBrakets(nod->right, false);
		}
		cout << ")";
	}

	if (sibling)
		cout << ", ";
}

void showByLevels(node* nod, int level)
{
	if (!nod)
		return;

	cout << level << ", " << nod->num << ";  ";

	showByLevels(nod->left, level + 1);
	showByLevels(nod->right, level + 1);

}

int main() 
{
	int arr[12] = { 4,8,7,15,12,1,20,3,5,8,13,16 };
	node* root = new node;
	root->left = root->right = NULL;
	root->num = 10;

	for (int i = 0; i < 12; i++)
	{
		addNode(root, arr[i]);
	}

	cout << "Sorted:" << endl;
	showBinary(root);

	int max = 0;
	findHeight(root, 0, &max);
	cout << "height: " << max << endl << endl;

	cout << "By brackets:" << endl;
	showByBrakets(root, false);

	cout << endl << endl << "By levels:" << endl;

	cout << "(";
	showByLevels(root, 1);
	cout << ")" << endl;

	return 0;
}