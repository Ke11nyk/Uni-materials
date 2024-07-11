#include <iostream>
using namespace std;

struct nodeStack
{
    float num;
    nodeStack* next;
};

void showStack(nodeStack* head)
{
    cout << "Your stack is: " << endl;
    while (head)
    {
        cout << head->num << endl;
        head = head->next;
    }
}

void showElStack(nodeStack* head)
{
    int numEl;
    nodeStack* temp = head;
    
    cout << "Write whick element you want show: " << endl;
    cin >> numEl;

    for (int i = 0; i < numEl; i++)
    {
        temp = temp->next;
    }

    cout << temp->num << endl;
}

void addStack(nodeStack** head)
{       
    int numEl, value;
    
    cout << "Write how many elements you want to add: ";
    cin >> numEl;
    cout << "Write what you want to add: ";
    cin >> value;

    for (int i = 0; i < numEl; i++)
    {
        nodeStack* temp;
        temp = new nodeStack;

        temp->num = value;
        temp->next = *head;
        *head = temp;
    }
}

void delStack(nodeStack** head)
{
    nodeStack* temp;

    while (*head)
    {
        temp = *head;
        *head = (*head)->next;
        delete temp;
    }
}

void delElStack(nodeStack** head)
{
    nodeStack* temp;
    int numEl;

    cout << "Write how many elements you want to delete: ";
    cin >> numEl;

    for(int i = 0; i < numEl; i++)
    {
        temp = *head;
        *head = (*head)->next;
        delete temp;
    }
    cout << "Done!" << endl;
}

void newStack(nodeStack** head)
{
    delStack(head);
    
    for (int i = 0; i < 3; i++)
    {
        nodeStack* temp = new nodeStack;
        temp->num = i;
        temp->next = *head;
        *head = temp;
    }
}

float aMean(nodeStack* head)
{
    float aMean = 0, num = 0;
    
    while (head)
    {
        aMean += head->num;
        num++;
        head = head->next;
    }

    return aMean/num;
}

void menu()
{
    nodeStack* head = NULL;
    
    char ch;
    do
    {
        cout << endl;
        cout << "Make your choise!" << endl;
        cout << ">> N to create stack with 3 elements" << endl;
        cout << ">> A to add new" << endl;
        cout << ">> S to show all" << endl;
        cout << ">> I to show i-th element" << endl;
        cout << ">> D to delete all" << endl;
        cout << ">> K to delete some elements" << endl;
        cout << ">> M to calculate the arithmetic mean of stack" << endl;
        cout << ">> Q to quit" << endl;
        cin >> ch;

        switch (ch)
        {
        case 'N':
            cout << "New:" << endl;
            newStack(&head);
            showStack(head);
            break;
        case 'A':
            addStack(&head);
            break;
        case 'S':
            showStack(head);
            break;
        case 'I':
            showElStack(head);
            break;
        case 'D':
            cout << "Delete" << endl;
            delStack(&head);
            cout << "Stack is empty!" << endl;
            break;
        case 'K':
            delElStack(&head);
            break;
        case 'M':
            cout << "Arithmetic mean of stack is: " << endl << aMean(head) << endl;
            break;
        case 'Q':
            cout << "Quit" << endl;
            break;
        default:
            cout << "Try again!";
            break;
        }

    } while (ch != 'Q');
}

int main()
{
    menu();
    return 0;
}