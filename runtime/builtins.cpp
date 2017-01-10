#include <map>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
using namespace std;

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MARK_PRIMITIVE __asm__("movl $0, %ecx\n\t")
#define MARK_PTR __asm__("movl $1, %ecx\n\t")

/**
 * mem type map
 * 1 - for str
 * 2 - for unboxed arr
 * 3 - for boxed arr
 */
static map<void*, int> mem_type;

// IO

extern "C" {
  
  extern int Lread() {
    int d;
    printf("> ");
    scanf("%d", &d);
    MARK_PRIMITIVE;
    return d;
  }
  
  extern int Lwrite(int x) {
    MARK_PRIMITIVE;
    return printf("%d\n", x);
  }
  
}

// STRING

extern void* gc_malloc(size_t size);

extern "C" {
extern void Tgc_inc_ref(int t, void* p);
extern void Tgc_dec_ref(int t, void* p);
extern void Tgc_ref(void* a, int vt, void* v, int nt, void* n);
extern void Tgc_collect();
}


static void builtin_mark(void *ptr) {
	Tgc_inc_ref(1, ptr);
	Tgc_dec_ref(1, ptr);
}
    
typedef struct {
  int len;
  char buf[];
} string_t;
typedef string_t *mstring;

static mstring stralloc(int n) {
  void* ptr = gc_malloc(sizeof(int) + n * sizeof(char));
  mem_type[ptr] = 1;
  mstring s = (mstring) ptr;
  s->len = n;
  return s;
}

extern "C" {

extern mstring Lstrmake(int n, int c) {
  mstring s = stralloc(n);
  for (int i = 0; i < n; i++) {
    s->buf[i] = c;
  }
  MARK_PTR;
  return s;
}

extern mstring Lstrset(mstring s, int i, int c) {
  s->buf[i] = c;
  MARK_PTR;
  return s;
}

extern int Lstrget(mstring s, int i) {
  int ret = s->buf[i];
  builtin_mark((void*)s);
  Tgc_collect();
  MARK_PRIMITIVE;
  return ret;
}

extern mstring Lstrdup(mstring s) {
  mstring d = stralloc(s->len);
  strncpy(d->buf, s->buf, s->len);
  builtin_mark((void*)s);
  Tgc_collect();
  MARK_PTR;
  return d;
}

extern mstring Lstrcat(mstring s1, mstring s2) {
  mstring r = stralloc(s1->len + s2->len);
  strncpy(r->buf, s1->buf, s1->len);
  strncpy(r->buf + s1->len, s2->buf, s2->len);
  builtin_mark((void*)s1);
  builtin_mark((void*)s2);
  Tgc_collect();
  MARK_PTR;
  return r;
}

extern int Lstrcmp(mstring s1, mstring s2) {
  int res = strncmp(s1->buf, s2->buf, MIN(s1->len, s2->len));
  if (res == 0) res = s1->len - s2->len;
  builtin_mark((void*)s1);
	builtin_mark((void*)s2);
	Tgc_collect();
  MARK_PRIMITIVE;
  if (res < 0) {
    return -1;
  } else if (res == 0) {
    return 0;
  } else {
    return 1;
  }
}

extern int Lstrlen(mstring s) {
  int ret = s->len;
  builtin_mark((void*)s);
  Tgc_collect();
  MARK_PRIMITIVE;
  return ret;
}

extern mstring Lstrsub(mstring s, int i, int l) {
  mstring r = stralloc(l);
  strncpy(r->buf, s->buf + i, l);
  builtin_mark((void*)s);
  	Tgc_collect();
  MARK_PTR;
  return r;
}

}

// ARRAY

typedef struct {
  int len;
  void* buf[];
} array_t;
typedef array_t *marray;

static marray arralloc(int n) {
  marray a = (marray) gc_malloc(sizeof(int) + n * sizeof(void*));
  a->len = n;
  return a;
}


extern "C" {

extern int Larrlen(marray a) {
  int ret = a->len;
  builtin_mark((void*)a);
  Tgc_collect();
  MARK_PRIMITIVE;
  return ret;
}

extern marray Larrmake(int n, int v) {
  marray a = arralloc(n);
  mem_type[(void*)a] = 2;
  for (int i = 0; i < n; i++) {
    a->buf[i] = (void*)v;
  }
  MARK_PTR;
  return a;
}

extern marray LArrmake(int n, void* ptr) {
  marray a = arralloc(n);
  mem_type[(void*)a] = 3;
  for (int i = 0; i < n; i++) {
    a->buf[i] = ptr;
    Tgc_ref((void*)a, 0, NULL, 1, ptr);
  }

  builtin_mark(ptr);
  Tgc_collect();

  MARK_PTR;
  return a;
}

extern void* Larrget(marray a, int n) {
  void * ret = a->buf[n];
  int memt = mem_type[a];
  builtin_mark((void*)a);
  Tgc_collect();
  if (memt == 2) {
    MARK_PRIMITIVE;
  } else {
    MARK_PTR;
  }
  return ret;
}

}
