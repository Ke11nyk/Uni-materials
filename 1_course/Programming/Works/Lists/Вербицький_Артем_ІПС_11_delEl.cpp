#include <iostream>
#include <math.h>
using namespace std;

struct node
{
	int data;
	node* prev, *next;
};

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

void addList(node** head, node** tail, int data, int pos)
{
	node *search, *temp;
	temp = new node;
	temp->data = data;
	
	if (!*head)
	{
	
		temp->next = temp->prev = temp;
		*head = *tail = temp;
	}
	else
	{	
		search = *head;
		int i = 1;
	
		while (i < pos and search != *tail)
		{
			search = search->prev;
			i++;
		}
	
		if (pos == 1)
		    *head = temp;
		if (i < pos) 
		{    
		    search = search->prev;
		    *tail = temp;
		}
		
		search->next->prev = temp;
		temp->next = search->next;
		search->next = temp;
		temp->prev = search;
	}
}



void showList(node *head, node *tail)
{
	node* temp = head;
    int i = 1;
    while (temp)
	{
    	cout << i <<". " << temp->data <<endl;
    	if (temp->prev != head) 
		    temp = temp->prev;
		else temp = NULL;    
    	i++;
	}
}


// Ô Ó Í Ê Ö ² ß  Â È Ä À Ë Å Í Í ß
void delEl(node** head, node** tail)
{
    if (*head == NULL)
        cout << "The list is empty!" << endl << endl;
    else
    {
        int num;

        cout << "Enter place to delete: " << endl << ">>> ";
        cin >> num;
        cout << endl;

        if (num == 1)
        {
            node* headNew = (*head)->prev;
            headNew->next = *tail;
            (*tail)->prev = headNew;
            delete *head;
            *head = headNew;
        }
        else
        {
        	node* current = *head;
        	int i = 1;
            do
            {
                current = current->prev;
                i++;
            }
            while (i < num && current->prev != *head);

            if (current == *tail)
            {
                node* tailNew = (*tail)->next;
            	tailNew->prev = *head;
            	(*head)->next = tailNew;
            	delete *tail;
            	*tail = tailNew;
            }
            else
            {
                current->next->prev = current->prev;
                current->prev->next = current->next;
                delete current;
            }
        }
    }

    cout << "Done!" << endl << endl;
}




int main(){
	
	node* head, *tail;
	head = tail = NULL;
	
//	addListLast(&head, &tail, 1);
//	addListLast(&head, &tail, 2);
//	addListLast(&head, &tail, 3);
	addList(&head, &tail, 900, 110);

	for (int i = 0; i < 10; i++)
	    addListLast(&head, &tail, i);
	showList(head, tail);
	
	cout << "new list: "<<endl;
	addList(&head, &tail, 100, 110);
	showList(head, tail);
	delEl(&head, &tail);
	showList(head, tail);

//	showList(head, tail);

/*	node* temp = new node;
	temp->data = 1;
	temp->next = temp->prev = temp;
	head = tail = temp;
	
	temp = new node;
	temp->data = 2;
	temp->next = tail;
	tail->prev = temp;
	temp->prev = head;
	head->next = temp;
	tail = temp;
	
	temp = new node;
	temp->data = 3;
	temp->next = tail;
	tail->prev = temp;
	temp->prev = head;
	head->next = temp;
	tail = temp;
*/	
//	cout << "head : " << head->data << ";  " << head->prev->data << ";  next: " << head->next->data<<endl;

//    temp = head;
//    int i = 1;
    // infinite cycle!
/*    while (temp)
    {
    	cout << i <<". " << temp->data <<endl;
    	temp = temp->prev;
    	i++;
	}  */
	
/*	while (temp != tail)
	{
    	cout << i <<". " << temp->data <<endl;
    	temp = temp->prev;
    	i++;
	}
	cout << i <<". " << tail->data <<endl; 
*/	
/*    while (temp)
	{
    	cout << i <<". " << temp->data <<endl;
    	if (temp->prev != head) 
		    temp = temp->prev;
		else temp = NULL;    
    	i++;
	}   */
	
	return 0;
};
