Node* Reverse(Node *head) {
    if (head) {
        Node *current;
        while (head) {
            current = head;
            head = head->next;
            current->next = current->prev;
            current->prev = head;
        }
        head = current;
    }
    return head;
}
