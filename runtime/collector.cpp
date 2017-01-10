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
  void * protect;
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
};

static map<void *, RegisterItem> registry;

extern void * gc_malloc(size_t size) {
  void * ptr = malloc(size);
  registry[ptr] = RegisterItem(ptr);
  return ptr;
}

extern "C" {
  
  /**
   * assign
   * t = 0 for primitive, 1 otherwise
   */
  extern void Tgc_inc_ref(int t, void * ptr) {
    // printf("* JUST %d %d * \n", t, (int) ptr);
    if (t != 0 && registry.find(ptr) != registry.end()) {
      // printf("* INC %d %d %d * \n", t, (int) ptr, registry[ptr].refs_cnt());
      registry[ptr].inc_ref();
    }
  }

  /**
   * arr assign
   */
  extern void Tgc_ref(int at, void * a, int bt, void * b) {
    Tgc_inc_ref(bt, b);
    if (bt != 0 && registry.find(b) != registry.end()) {
      registry[a].depency(&registry[b]);
    }
  }

  /**
   * before Tgc_collect
   */
  extern void Tgc_dec_ref(int t, void * ptr) {
    if (t != 0 && registry.find(ptr) != registry.end()) {
      // printf("* DEC %d %d %d * \n", t, (int) ptr, registry[ptr].refs_cnt());
      registry[ptr].dec_ref();
    }
  }

  /**
   * before ret
   */
  extern void Tgc_collect() {
    for(auto iter : free_q) {
      void * ptr = iter;
      // printf("* DEL %d *\n", (int) ptr);
      free(ptr);
    }
    free_q.clear();
  }

}
