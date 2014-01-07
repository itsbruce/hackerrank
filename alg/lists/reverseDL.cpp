Node *Reverse(Node *head) {
    Node *current = NULL;
    while (head) {
        current = head;
        head = head->next;
        current->next = current->prev;
        current->prev = head;
    }
    if (!head) head = current;
    return head;
}
