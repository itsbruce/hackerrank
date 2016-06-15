Node *SortedInsert(Node *head, int data) {
    Node *insert = new Node();
    insert->data = data;
    // Is list empty?
    if (!head) {
        insert->next = NULL;
        insert->prev = NULL;
        return insert;
    }
    Node *cursor = head;
    head = (head->data < data) ? head : insert;
    Node *prev = NULL;
    while (cursor && cursor->data < data) {
        prev = cursor;
        cursor = cursor->next;
    }
    insert->next = cursor;
    insert->prev = prev;
    if (cursor) cursor->prev = insert;
    if (prev) prev->next = insert;
    return head;
}
