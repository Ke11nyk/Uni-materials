#include <iostream>
#include <stack>
#include <string>
#include <cctype>
#include <cmath>
#include <unordered_map>

using namespace std;

class Node {
public:
    char op;
    double value;
    Node* left;
    Node* right;

    Node(char c) : op(c), value(0), left(nullptr), right(nullptr) {}
    Node(double val) : op('\0'), value(val), left(nullptr), right(nullptr) {}
};

class ExpressionTree {
private:
    Node* root;
    unordered_map<string, double> variables;

    int precedence(char op) {
        if (op == '+' || op == '-')
            return 1;
        else if (op == '*' || op == '/')
            return 2;
        else if (op == '^')
            return 3;
        else
            return -1;
    }

    Node* createTree(string& expr) {
        stack<Node*> operandStack;
        stack<char> operatorStack;

        for (size_t i = 0; i < expr.length(); i++) { // i is declared and initialized here
            char& c = expr[i];
            if (isdigit(c)) {
                double val = static_cast<double>(c - '0');
                string numStr;
                numStr += c;

                size_t j = 1;
                while (expr.length() > i + j && isdigit(expr[i + j])) {
                    numStr += expr[i + j];
                    j++;
                }

                val = stod(numStr);
                operandStack.push(new Node(val));
                i += j - 1;
            }
            else if (isalpha(c)) {
                string varName;
                varName += c;

                size_t j = 1;
                while (expr.length() > i + j && isalpha(expr[i + j])) {
                    varName += expr[i + j];
                    j++;
                }

                if (variables.count(varName) == 0) {
                    cout << "Enter the value of " << varName << ": ";
                    double val;
                    cin >> val;
                    variables[varName] = val;
                }

                operandStack.push(new Node(variables[varName]));
                i += j - 1;
            }
            else if (c == '(') {
                operatorStack.push(c);
            }
            else if (c == ')') {
                while (!operatorStack.empty() && operatorStack.top() != '(') {
                    char op = operatorStack.top();
                    operatorStack.pop();
                    Node* right = operandStack.top();
                    operandStack.pop();
                    Node* left = operandStack.top();
                    operandStack.pop();
                    Node* newNode = new Node(op);
                    newNode->left = left;
                    newNode->right = right;
                    operandStack.push(newNode);
                }
                if (!operatorStack.empty())
                    operatorStack.pop();
            }
            else {
                while (!operatorStack.empty() && precedence(operatorStack.top()) >= precedence(c)) {
                    char op = operatorStack.top();
                    operatorStack.pop();
                    Node* right = operandStack.top();
                    operandStack.pop();
                    Node* left = operandStack.top();
                    operandStack.pop();
                    Node* newNode = new Node(op);
                    newNode->left = left;
                    newNode->right = right;
                    operandStack.push(newNode);
                }
                operatorStack.push(c);
            }
        }

        while (!operatorStack.empty()) {
            char op = operatorStack.top();
            operatorStack.pop();
            Node* right = operandStack.top();
            operandStack.pop();
            Node* left = operandStack.top();
            operandStack.pop();
            Node* newNode = new Node(op);
            newNode->left = left;
            newNode->right = right;
            operandStack.push(newNode);
        }

        return operandStack.top();
    }

    double evaluate(Node* node) {
        if (!node)
            return 0;

        if (node->op == '\0')
            return node->value;

        double left = evaluate(node->left);
        double right = evaluate(node->right);

        switch (node->op) {
        case '+':
            return left + right;
        case '-':
            return left - right;
        case '*':
            return left * right;
        case '/':
            return left / right;
        case '^':
            return pow(left, right);
        default:
            return 0;
        }
    }

public:
    ExpressionTree(string expr) {
        root = createTree(expr);
    }

    double calculate() {
        return evaluate(root);
    }
};

int main() {
    string expr;
    cout << "Enter the expression: ";
    getline(cin, expr);

    ExpressionTree tree(expr);
    double result = tree.calculate();

    cout << "Result: " << result << endl;

    return 0;
}