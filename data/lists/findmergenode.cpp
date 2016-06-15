int FindMergeNode(Node *headA, Node *headB) {
    int countA = 1;
    int countB = 1;
    Node *cursor = headA;
    while (cursor->next) {
        cursor = cursor->next;
        ++countA;
    }
    cursor = headB;
    while (cursor->next) {
        cursor = cursor->next;
        ++countB;
    }
    Node *longer = (countA > countB) ? headA : headB;
    Node *shorter = (countA > countB) ? headB : headA;
    for (int i=0; i < abs(countA - countB); i++) {
        longer = longer->next;
    }
    while (longer->data != shorter->data) {
        longer = longer->next;
        shorter = shorter->next;
    }
    return longer->data;
}
