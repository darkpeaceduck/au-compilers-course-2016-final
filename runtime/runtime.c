#include <stdio.h>

extern int read(){
  int d;
  printf("> ");
  scanf("%d", &d);
  return d;
}

extern int write(int x){
  return printf("%d\n", x);
}
