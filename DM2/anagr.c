#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

struct { int n; char** s; } words;

void read_data() {
    char buffer[21];
    FILE* f=fopen("dict.dat", "r");
    // Lecture du nombre total de mots
    fscanf(f, "%d\n", &words.n);
    // Allocation du tableau pour recueillir les chaînes
    words.s = (char**)malloc(words.n * sizeof(char*));
    if (words.s == NULL) { words.n = 0; return; } // Echec d'allocation
    // Remplissage du tableau
    for (int i=0; i<words.n; ++i) {
        fscanf(f, "%20s\n", buffer);
        words.s[i] = strdup(buffer);
    }
    fclose(f);
}

void find(char* req) {
    // Affiche les mots que l'on peut construire à partir des lettres de req
    for (int i=0; i<words.n; ++i) {
        if (strcmp(req, words.s[i])==0) { printf("%s\n", words.s[i]); }
    }
}

int main(void) {
    read_data();
    while (true) {
        char req[20];
        printf("Lettres : ");
        scanf("%19s", req);
        if (strcmp(req,"!")==0) { return 0; }
        find(req);
    }
}
