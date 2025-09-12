/*
Create a function similar to Python’s input function: response = input(“Enter name”) 

It should show the prompt, return the input
Use this function in main
You may notice you get a warning with what you return. This is because of how memory is allocated with it being on the stack. 
Once you have that completed create another similar Python function called toInt() (similar to int() in python) that returns the integer value of any string passed to it 

*/
#include <stdio.h>
#include <stdlib.h>

//mimic python input 
char *inputImplement(const char *prompt) {
    static char buffer[100];  // buffer for message
    printf("%s", prompt);     // take prompt passed to us 
    if (fgets(buffer, sizeof(buffer), stdin) != NULL) {
        // remove newline if present
        size_t len = 0;
        while (buffer[len] != '\0') {
            if (buffer[len] == '\n') {
                buffer[len] = '\0';
                break;
            }
            len++;
        }
        return buffer;
    }
    return NULL;  // if input fails
}

int toInt(char *intString) {
    return atoi(intString); 
}

int main(void) {
    char *response = inputImplement("Enter name: ");
    if (response != NULL) {
        printf("You entered: %s\n", response);
    }
    return 0;
}
