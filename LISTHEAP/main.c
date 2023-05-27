#include <stdio.h>
#include <stdlib.h>

struct list {
    int value;
    struct list* next;
};

typedef struct list List;

List* new(int value,List* next) {
    List *res=malloc(sizeof(List));
    res->value=value;
    res->next=next;
    return res;
}

List* match(int value,List* schLst) {
    if (schLst==NULL || schLst->value==value) {
        return schLst;
    }
    return match(value,schLst->next);
}

List* add(int value,List* next) {
    return new(value,next);
}

List* delete_occ(int value,List* schLst) {
    if (schLst==NULL) {
        return schLst;
    } if (schLst->value==value) {
        List* cpy=schLst->next;
        free(schLst);
        return delete_occ(value,cpy);
    }
    schLst->next=delete_occ(value,schLst->next);
    return schLst;
}

List* reverse_list(List* schLst) {
    List* res=NULL,*ptr=schLst;
    while (ptr!=NULL) {
        res=add(ptr->value,res);
        ptr=ptr->next;
    }
    return res;
}

List* insert(int value,List* lst) {
    List* head=malloc(sizeof(List));
    head->value=value;
    head->next=lst;
    return head;
}

void print_list(List* lst) {
    if (lst==NULL) {
        printf("|\n");
        return;
    }
    printf("|%d\t",lst->value);
    print_list(lst->next);
}

int main(int argc,char** argv) {
    List* lst=add(1,add(2,add(3,add(4,add(5,NULL)))));
    for (int i=0;i<42*42;++i) {
        lst=insert(42*42-i,lst);
    }
    List* rlst=reverse_list(lst);
    print_list(lst);
    print_list(rlst);
    free(lst);
    free(rlst);
}

