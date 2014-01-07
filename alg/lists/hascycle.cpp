int HasCycle(Node *head) {
    Node *slow = head;
    Node *fast = head;
    while (slow && fast && fast->next && fast->next->next) {
        if ((fast->next == slow) || (fast->next->next == slow)) {
            return 1;
        } else {
            slow = slow->next;
            fast = fast->next->next;
        }
    }
    return 0;
}
