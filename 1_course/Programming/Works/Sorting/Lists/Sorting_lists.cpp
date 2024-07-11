#include <iostream>
#include <string.h>
#include <stdio.h>      
#include <stdlib.h>     
#include <time.h>
using namespace std;

struct node 
{
	int data;
	node* prev, * next;
};


void showList(node* head, node* tail)
{
	node* temp = head;
	int i = 1;
	while (temp)
	{
		cout << i << ". " << temp->data << endl;
		if (temp->prev != head)
			temp = temp->prev;
		else temp = NULL;
		i++;
	}
	cout << endl;
}

void addListLast(node** head, node** tail, int data)
{
	node* temp;
	if (!*head)
	{
		temp = new node;
		temp->data = data;
		temp->next = temp->prev = temp;
		*head = *tail = temp;
	}
	else
	{
		temp = new node;
		temp->data = data;
		temp->next = *tail;
		(*tail)->prev = temp;
		temp->prev = *head;
		(*head)->next = temp;
		*tail = temp;
	}
}


void randArr(int arr[], int max)
{
	for (int i = 0; i < max; i++)
		arr[i] = rand() % 10000;
}


void bubbleSort1(node** head, node** tail)
{
	bool change = true;
	node* temp = *head;
	
	while (change)
	{
		node* temp = *head;
		change = false;
		while (temp->prev != *head)
		{
			if (temp->data > temp->prev->data)
			{
				node* ntemp = temp->prev;

				temp->prev = ntemp->prev;
				ntemp->prev->next = temp;

				ntemp->next = temp->next;
				temp->next->prev = ntemp;

				temp->next = ntemp;
				ntemp->prev = temp;

				if (*head == temp) *head = ntemp;
				if (*tail == ntemp) *tail = temp;

				change = true;
			}

			temp = temp->prev;
		}
	}
}

void bubbleSort2(node** head, node** tail)
{
	bool change = true;
	node* temp = *head;

	while (change)
	{
		node* temp = *head;
		node* end = *head;
		change = false;
		while (temp->prev != end)
		{
			if (temp->data > temp->prev->data)
			{
				node* ntemp = temp->prev;

				temp->prev = ntemp->prev;
				ntemp->prev->next = temp;

				ntemp->next = temp->next;
				temp->next->prev = ntemp;

				temp->next = ntemp;
				ntemp->prev = temp;

				if (*head == temp)
				{
					if (end == *head) end = ntemp;
					*head = ntemp;
				}
				if (*tail == ntemp) *tail = temp;

				change = true;
			}

			temp = temp->prev;
		}

		end = end->next;
	}
}

void insertSort(node** head, node** tail)
{
	node* temp = (*head)->prev;
	while (temp != *head)
	{
		node* ntemp = temp->next;
		if (temp->data < (*head)->data) ntemp = *tail;
		else
		{
			while (ntemp->data > temp->data and ntemp != *tail) ntemp = ntemp->next;
			ntemp->prev;
		}
		
		if (temp == ntemp)
		{
			*tail = (*tail)->next;
			*head = (*head)->next;
		}
		else
		{
			temp->next->prev = temp->prev;
			temp->prev->next = temp->next;

			temp->prev = ntemp->prev;
			ntemp->prev->next = temp;

			temp->next = ntemp;
			ntemp->prev = temp;

			if (ntemp == *tail) *head = temp;
		}
		showList(*head, *tail);
		temp = temp->prev;
	}
	
}

void selectSort(int arr[], int max)
{
	for (int i = 0; i < max - 1; i++)
	{
		int min = i;
		for (int j = i + 1; j < max; j++)
			if (arr[j] < arr[min])
				min = j;

		if (i != min)
		{
			int temp = arr[i];
			arr[i] = arr[min];
			arr[min] = temp;
		}
		//	showArr(arr, max);
	}
}

int main(int argc, char** argv) 
{
	int const max = 50;
	int begin, end;

	node* head, * tail;
	head = tail = NULL;

	addListLast(&head, &tail, 9);
	addListLast(&head, &tail, 3);
	addListLast(&head, &tail, 5);
	addListLast(&head, &tail, 8);
	addListLast(&head, &tail, 7);
	addListLast(&head, &tail, 6);
	addListLast(&head, &tail, 4);
	addListLast(&head, &tail, 11);

	node* headb1, * tailb1;
	headb1 = tailb1 = NULL;

	node* headb2, * tailb2;
	headb2 = tailb2 = NULL;

	node* headin, * tailin;
	headin = tailin = NULL;

	int arr[8] = { 9,3,5,8,7,6,4,2 };
	
	for (int i = 0; i < max; i++)
		addListLast(&headb1, &tailb1, rand() % 10000);

	for (int i = 0; i < max; i++)
		addListLast(&headb2, &tailb2, rand() % 10000);

	for (int i = 0; i < max; i++)
		addListLast(&headin, &tailin, rand() % 10000);

	
	begin = clock();
	bubbleSort1(&headb1, &tailb1);
	end = clock();
	cout << "exe time Bubble Sort: " << (end - begin) << endl;

	begin = clock();
	bubbleSort2(&headb2, &tailb2);
	end = clock();
	cout << "exe time Bubble Sort Modified: " << (end - begin) << endl;

	
	showList(head, tail);
	begin = clock();
	insertSort(&headin, &tailin);
	end = clock();
	showList(head, tail);
	
	cout << "exe time Insert Sort: " << (end - begin) << endl;

	return 0;
}