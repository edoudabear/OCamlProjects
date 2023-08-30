#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

const int maxlen = 1000;

//-----------------------------------
// Node structure and allocation

struct node {
    struct node* u;
    struct node* d;
    struct node* l;
    struct node* r;
    struct node* c;
    char* name;
    int nb;
};

typedef struct node Node;

Node* new_node(void) {
    Node* n = (Node*)malloc(sizeof(Node));
    // If allocation fail
    if (n==NULL) { fprintf(stderr, "Node alloction failed\n"); exit(-1); }
    // Initialisation
    n->u = n; n->d = n;
    n->l = n; n->r = n;
    n->c = NULL; n->name = NULL;
    n->nb = 0;
    return n;
}

//-----------------------------------
// File analysis

char* strdup(char* s) {
    int len = strlen(s);
    char* result = (char*)malloc((len+1)*sizeof(char));
    if (result == NULL) { return NULL; }
    memcpy(result, s, len+1);
    return result;
}

Node* load_file(char* filename) {
    // Open file
    FILE* f = fopen(filename, "r");
    if (f==NULL) { fprintf(stderr, "Can't open file <%s>\n", filename); exit(-1); }

    // First node
    Node* h = new_node();

    // Buffer to read data
    char* buffer = (char*)malloc(maxlen*sizeof(char));

    // First line (list of objects)
    fgets(buffer, maxlen, f);
    char* str_token = strtok(buffer, " \r\n");
    Node* ptr=h;
    while (str_token != NULL) {
        // Another object!
        h->nb++;
        // Create a new header node
        Node* n = new_node();
        n->name = strdup(str_token);
        // Attach node to existing ones

        // new code
        n->c=NULL;
        n->nb=0;
        n->u=n;
        n->d=n;
        n->l=ptr;
        n->r=h;
        ptr->r=n;
        ptr=n;
        h->l=n;
        // end of added code

        // Get next name
        str_token = strtok(NULL, " \r\n");
    }

    // Read each offers (one per line)
    int line_counter = 0;
    bool* provides = (bool*)malloc(h->nb * sizeof(bool));
    while (!feof(f)) {
        line_counter++;
        // Read line
        if (fgets(buffer, maxlen, f)) {
            // Reset elements
            for (int i=0; i<h->nb; ++i) { provides[i] = false; }
            // Read elements and fill "provides" array
            str_token = strtok(buffer, " \r\n");
            while (str_token != NULL) {
                Node* n = h->r;
                int i = 0;
                while (n != h && strcmp(n->name, str_token)) { n = n->r; i++; }
                if (n == h) { fprintf(stderr, "Unknown token <%s> line <%d>\n", str_token, line_counter); exit(-1); }
                if (provides[i]) { fprintf(stderr, "Duplicate token <%s> line <%d>\n", str_token, line_counter); exit(-1); }
                provides[i] = true;
                str_token = strtok(NULL, " \r\n");
            }
            // Create nodes
            Node *ptr=h,*bkpPtr=NULL,*fstPtr=NULL;
            for (int i=0;i<h->nb;++i) {
                ptr=ptr->r;
                if (provides[i]) {
                    Node *ptr2=ptr->u;
                    Node *n=new_node();
                    n->u=ptr2;
                    n->d=ptr;
                    // horizontal linking
                    if (fstPtr==NULL) {
                        fstPtr=n;
                        bkpPtr=n;
                    } else {
                        n->l=bkpPtr;
                        n->r=fstPtr;
                        bkpPtr->r=n;
                        fstPtr->l=n;
                        bkpPtr=n;
                    }
                    ptr->u=n;
                    ptr2->d=n;
                    n->c=ptr;
                    n->name=ptr->name;
                }
            }
        }
    }
    // Close file
    fclose(f);
    return h;
}

//-----------------------------------
// Display functions

void print_line(Node* n) {
    // hand made
    Node* ptr=n;
    do {
        printf("%s ",ptr->name);
        ptr=ptr->r;
    } while (ptr!=n);
    printf("\n");
}

void print_solution(Node** output, int k) {
    // used to keep track of solution number
    static int counter=0;
    counter++;

    // Display solution
    printf("Solution #%d:\n", counter);
    for (int i=0; i<k; ++i) {
        printf("  ");
        print_line(output[i]);
    }
}

//-----------------------------------
// Linked list functions

void remove_h(Node* n) {
    Node *right=n->r,*left=n->l;
    (*left).r=right;
    (*right).l=left;
}

void replace_h(Node* n) {
    Node *right=n->r,*left=n->l;
    (*left).r=n;
    (*right).l=n;
}

void remove_v(Node* n) {
    Node *up=n->u,*down=n->d;
    (*down).u=up;
    (*up).d=down;
}

void replace_v(Node* n) {
    Node *up=n->u,*down=n->d;
    (*down).u=n;
    (*up).d=n;
}

//-----------------------------------
// Covering functions

void cover(Node* c) {
    Node *ptri=c->d;
    do {
        Node *ptrj=ptri;
        do {
            ptrj=ptrj->r;
            if (ptrj!=ptri)
                remove_v(ptrj);
        } while (ptrj!=ptri);
        ptri=ptri->d;
    } while (ptri!=c);
    remove_h(c);
}

void uncover(Node* c) {
    Node *ptri=c->u;
    replace_h(c);
    do {
        Node *ptrj=ptri;
        do {
            if (ptrj!=ptri)
                remove_v(ptrj);
            ptrj=ptrj->l;
        } while (ptrj!=ptri);
        ptri=ptri->u;
    } while (ptri!=c);
}

//-----------------------------------
// Backtracking resolution

Node* choose_column(Node* h) {
    return h->r;
}

void search(int k, Node* h, Node** output) {
    if (h->r == h) { print_solution(output, k); return; }

    // Choose column
    Node* c = choose_column(h);

    // Backtracking

    // ***** TODO *****
}

void solve(Node* h) {
    Node** output = (Node**)malloc(h->nb*sizeof(Node*));
    if (output==NULL) { fprintf(stderr, "Allocation of output failed\n"); exit(-1); }
    search(0, h, output);
}

//-----------------------------------
// Main program

int main(void) {
    Node* h = load_file("exemple.dlx");
    Node* bkp=h->r;

    // Tests d'affichage de lignes
    print_line(h->r->d);
    //print_line(h->l->u);
    cover(bkp);
    print_line(h->r->d);
    uncover(bkp);
    print_line(h->r->d);
    //print_line(h->l->u);
    //printf("%s\n",h->r->d->r->name);
    //printf("%p \n%p\n %p",h->r->d->r,h->r->d->r->r,h->r->d);
    //printf("%s",(h->r->r->r==h->r->d->r->u) ? "true\n" : "false\n");

    // Résolution du problème
    //solve(h);
    free(h);
    return 0;
}
