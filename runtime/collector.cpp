#include <map>
#include <memory>
#include <vector>
#include <stdint.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
using namespace std;

static vector<void*> free_q;

class RegisterItem{
  int refs;
  void* protect;
  vector<RegisterItem*> sub_objects;
public:
  RegisterItem() {}
  RegisterItem(void * ptr) : protect(ptr), refs(0), sub_objects() {}
  void inc_ref() {
    this->refs++;
  }
  void dec_ref() {
    this->refs--;
    if (this->refs == 0) {
      for(auto item : this->sub_objects) {
	item->dec_ref();
      }
      free_q.push_back(this->protect);
    }
  }
  void depency(RegisterItem *obj) {
    sub_objects.push_back(obj);
  }
  int refs_cnt() {
    return this->refs;
  }
  void print_info() {
    printf("%d %d \n", refs, (int) protect);
    for(auto item : this->sub_objects) {
      item->print_info();
    }
  }
};

static map<void*, RegisterItem> registry;

extern void* gc_malloc(size_t size) {
  void* ptr = malloc(size);
  registry[ptr] = RegisterItem(ptr);
  return ptr;
}

static int is_valid(int t, void* p) {
  if (t != 0 && registry.find(p) != registry.end()) {
    return 1;
  } else {
    return 0;
  }
}

extern "C" {

  /**
   * 
   */
  extern void Lgc_info(void* p) {
    registry[p].print_info();
  }
  
  /**
   * assign
   * t = 0 for primitive, 1 otherwise (means array or string)
   */
  extern void Tgc_inc_ref(int t, void* p) {
    // printf("* JUST %d %d * \n", t, (int) ptr);
    if (is_valid(t, p)) {
      // printf("* INC %d %d %d * \n", t, (int) ptr, registry[ptr].refs_cnt());
      registry[p].inc_ref();
    }
  }

  extern void Tgc_dec_ref(int t, void* p);

  /**
   * arr assign
   * a for array, v for prev value, n for new value
   */
  extern void Tgc_ref(void* a, int vt, void* v, int nt, void* n) {
    Tgc_dec_ref(vt, v);
    Tgc_inc_ref(nt, n);
    if (is_valid(nt, n)) {
      registry[a].depency(&registry[n]);
    }
  }

  /**
   * before Tgc_collect
   */
  extern void Tgc_dec_ref(int t, void* p) {
    if (is_valid(t, p)) {
      // printf("* DEC %d %d %d * \n", t, (int) ptr, registry[ptr].refs_cnt());
      registry[p].dec_ref();
    }
  }

  /**
   * ...
   */
  extern void Tgc_clear_q() {
    free_q.clear();
  }

  /**
   * before ret
   */
  extern void Tgc_collect() {
    //printf("%d\n", free_q.size());
    for(auto iter : free_q) {
      void * ptr = iter;
      // printf("* DEL %d *\n", (int) ptr);
      // if (t == 0 || (int) ptr != (int) dp) {
      free(ptr);
      // }
    }
    Tgc_clear_q();
  }

}
