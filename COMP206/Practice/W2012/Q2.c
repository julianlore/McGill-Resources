#include<stdlib.h>

struct linked_list
{
  int data;
  linked_list *next;
};

struct linked_list reverse(struct linked_list head){
  struct linked_list *current = head;
  struct linked_list *reverse;
  // Start with head's data
  reverse = (struct linked_list*)malloc(sizeof(struct linked_list));
  reverse->data = current->data; // Tail is head's data
  // Go backwards
  while(current->next!=NULL){// Loop until end of list
    current = current->next;
    struct linked_list *new;
    new = (struct linked_list*)malloc(sizeof(struct linked_list));
    new->next = reverse; // Link to latest node
    new->data = curren->data;
    reverse = new; // Set head of reverse to latest
  }
  return reverse;
}
