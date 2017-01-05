#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIN(a,b) ((a) < (b) ? (a) : (b))

extern int Lread() {
  int d;
  printf("> ");
  scanf("%d", &d);
  return d;
}

extern int Lwrite(int x) {
  return printf("%d\n", x);
}

typedef struct {
  int len;
  char buf[];
} string_t;
typedef string_t *string;

static string stralloc(int n) {
  string s = (string) malloc(sizeof(int) + n * sizeof(char));
  s->len = n;
  return s;
}


extern string Lstrmake(int n, int c) {
  string s = stralloc(n);
  for (int i = 0; i < n; i++) {
    s->buf[i] = c;
  }
  return s;
}

extern string Lstrset(string s, int i, int c) {
  s->buf[i] = c;
  return s;
}

extern int Lstrget(string s, int i) {
  return s->buf[i];
}

extern string Lstrdup(string s) {
  string d = stralloc(s->len);
  strncpy(d->buf, s->buf, s->len);
  return d;
}

extern string Lstrcat(string s1, string s2) {
  string r = stralloc(s1->len + s2->len);
  strncpy(r->buf, s1->buf, s1->len);
  strncpy(r->buf + s1->len, s2->buf, s2->len);
  return r;
}

extern int Lstrcmp(string s1, string s2) {
  int res = strncmp(s1->buf, s2->buf, MIN(s1->len, s2->len));
  if (res == 0) res = s1->len - s2->len;
  if (res < 0) {
    return -1;
  } else if (res == 0) {
    return 0;
  } else {
    return 1;
  }
}

extern int Lstrlen(string s) {
  return s->len;
}

extern string Lstrsub(string s, int i, int l) {
  string r = stralloc(l);
  strncpy(r->buf, s->buf + i, l);
  return r;
}

typedef struct {
  int len;
  void* buf[];
} array_t;
typedef array_t *array;

static array arralloc(int n) {
  array a = (array) malloc(sizeof(int) + n * sizeof(void*));
  a->len = n;
  return a;
}

extern int Larrlen(array a) {
  return a->len;
}

extern array Larrmake(int n, int v) {
  array a = arralloc(n);
  for (int i = 0; i < n; i++) {
    a->buf[i] = (void*)v;
  }
  return a;
}

extern array LArrmake(int n, void* ptr) {
  array a = arralloc(n);
  for (int i = 0; i < n; i++) {
    a->buf[i] = ptr;
  }
  return a;
}
