#include <iostream>
using namespace std;

struct chessInfo 
{
    string firstName;
    string lastName;
    string state;
    int elo;
};

struct list
{
    chessInfo info;
    list* right;
    list* left;
};

void showList(list* first)
{
    if (first == NULL)      
        cout << "The list is empty!" << endl;
    else
    {
        list* current = first;
        int i = 1;
        while (current != NULL)
        {
            cout << i << ". " << current->info.firstName << " " << current->info.lastName << endl;
            current = current->right;
            i++;
        }
    }
    cout << endl;
}

void showEl(list* first)
{
    if (first == NULL)
    {
        cout << "The list is empty!" << endl << endl;
    }
    else
    {
        list* current = first;
        int i = 1, num;

        cout << "Enter which element you want to see: " << endl << ">>> ";
        cin >> num;

        while (i < num && current->right != NULL)
        {
            current = current->right;
            i++;
        }

        cout << i << ". " << current->info.firstName << " " << current->info.lastName << endl << endl;
    }
}

void addEl(list** first)
{
    list* newMaster = new list;


    if (*first == NULL)
        *first = newMaster;
    else 
    {
        list* current = *first;
        int i = 1, num;
        
        cout << "Enter place to put info: " << endl << ">>> ";
        cin >> num;
        cout << endl;

        if (num == 1)
        {
            (*first)->left = newMaster;
            newMaster->right = *first;
            *first = newMaster;
        }
        else
        {
            while (i < num - 1 && current->right != NULL)
            {
                current = current->right;
                i++;
            }

            if (current->right == NULL)
            {
                current->right = newMaster;
                newMaster->left = current;
                newMaster->right = NULL;
            }
            else
            {
                newMaster->left = current;
                newMaster->right = current->right;
                current->right->left = newMaster;
                current->right = newMaster;
            }
        }
    }

    cout << "Master was added!" << endl << endl;
}

void addTestList(list** first)
{
    list* newMasters, * current, * firstNew;

    firstNew = new list;
    firstNew->info.firstName = "Kawasaki";
    firstNew->info.lastName = "Kawasaki";
    firstNew->info.state = "The USA";
    firstNew->info.elo = 2080;

    if (*first == NULL)
    {
        firstNew->left = NULL;
        *first = firstNew;
    }
    else
    {
        current = *first;
        while (current->right != NULL) 
            current = current->right;
        current->right = firstNew;
        firstNew->left = current;
    }
    
    newMasters = new list;
    newMasters->info.firstName = "Kaw";
    newMasters->info.lastName = "Kaw";
    newMasters->info.state = "The UA";
    newMasters->info.elo = 2070;

    newMasters->left = firstNew;
    firstNew->right = newMasters;
    current = newMasters;

    newMasters = new list;
    newMasters->info.firstName = "law";
    newMasters->info.lastName = "law";
    newMasters->info.state = "The A";
    newMasters->info.elo = 2060;

    newMasters->left = current;
    newMasters->right = NULL;
    current->right = newMasters;
    current = newMasters;
}

void delList(list** first)
{
    list* forDel;
    while (*first != NULL)
    {
        forDel = *first;
        *first = (*first)->right;
        delete forDel;
    }

    cout << "Done!" << endl << endl;
}

void delEl(list** first)
{
    if (*first == NULL)
        cout << "The list is empty!" << endl << endl;
    else
    {
        list* current = *first;
        int i = 1, num;

        cout << "Enter place to delete: " << endl << ">>> ";
        cin >> num;
        cout << endl;

        if (num == 1)
        {
            list* firstCopy = (*first)->right;
            delete *first;
            *first = firstCopy;
        }
        else
        {
            while (i < num && current->right != NULL)
            {
                current = current->right;
                i++;
            }

            if (current->right == NULL)
            {
                current->left->right = NULL;
                delete current;
            }
            else
            {
                current->left->right = current->right;
                current->right->left = current->left;
                delete current;
            }
        }
    }

    cout << "Done!" << endl << endl;
}

void mainMenu()                 
{
    list* first = NULL;        

    char ch = ' ';                
    while (ch != 'Q')          
    {
        cout << "Please make your choise:" << endl << endl <<              
            "> A  to add a new chessmaster" << endl <<
            "> T  to add a test list" << endl <<
            "> S  to show all chessmasters in the list" << endl <<
            "> F  to show chessmaster" << endl <<
            "> D  to delete chessmaster from the list" << endl <<
            "> U  to delete the list" << endl <<
            "> Q  to quit the program" << endl << endl <<
            ">>> ";
        cin >> ch;                       
        cout << endl;
        switch (ch)                    
        {
        case 'A':
            cout << ">>> Add <<<" << endl << endl;            
            addEl(&first);                             
            break;
        case 'T':
            cout << ">>> Test List <<<" << endl << endl;
            addTestList(&first);
            break;
        case 'S':
            cout << ">>> Show all <<<" << endl << endl;
            showList(first);
            break;
        case 'F':
            cout << ">>> Find <<<" << endl << endl;
            showEl(first);
            break;
        case 'D':
            cout << ">>> Delete <<<" << endl << endl;
            delEl(&first);
            break;
        case 'U':
            cout << ">>> Delete all <<<" << endl << endl;
            delList(&first);
            break;
        case 'Q':
            cout << ">>> Quit <<<" << endl << endl;
            break;
        default:
            cout << ">>> Incorrect input, try again <<<" << endl << endl;           
            break;
        }
    }
}

int main()
{
    mainMenu();
    return 0;
}