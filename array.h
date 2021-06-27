#pragma once
struct Int
{
	int* mass_of_ints;
	int size_of_array;
};
struct Node {
	char* Name_of_Variable;
	char* Polynom;
	struct Node* next;
};
extern int semantic;
extern int counter;
extern char string_var[10];
struct Node* add(struct Node *head, char* name , char* polynom);
void Show(struct Node* head);
struct Node* Find(struct Node *head, char* name);
struct Node* Delete(struct Node* head, struct Node* removable);









