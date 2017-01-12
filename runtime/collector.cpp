#include <map>
#include <memory>
#include <set>
#include <vector>
#include <stdint.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
using namespace std;

static set<void*> free_q;

class RegisterItem{
  int refs;
  void* protect;
  multiset<RegisterItem*> sub_objects;
public:
  RegisterItem() {}
  RegisterItem(void * ptr) : protect(ptr), refs(0), sub_objects() {}
  void inc_ref() {
	/* anonymous fun res case */
	if (this->refs == 0) {
		if (free_q.count(this->protect))
			free_q.erase(this->protect);
		for (auto item : this->sub_objects)
			item->inc_ref();
	}
    this->refs++;
  }
  void dec_ref() {
    this->refs--;
    if (this->refs == 0) {
      for(auto item : this->sub_objects) {
    	  item->dec_ref();
      }
      free_q.insert(this->protect);
    }
  }
  void depency(RegisterItem *obj) {
    sub_objects.insert(obj);
  }
  void remove_depency(RegisterItem * ptr) {
	  auto itr = sub_objects.find(ptr);
	  if(itr != sub_objects.end()){
		  sub_objects.erase(itr);
	  }
  }
  int refs_cnt() {
    return this->refs;
  }
  void print_info() {
    for(auto item : this->sub_objects) {
      item->print_info();
    }
  }
};

static map<void*, RegisterItem *> registry;

extern void* gc_malloc(size_t size) {
  void* ptr = malloc(size);
  registry[ptr] = new RegisterItem(ptr);
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
    registry[p]->print_info();
  }

  /**
   * assign
   * t = 0 for primitive, 1 otherwise (means array or string)
   */
  extern void Tgc_inc_ref(int t, void* p) {
    if (is_valid(t, p)) {
      registry[p]->inc_ref();
    }
  }

  extern void Tgc_dec_ref(int t, void* p);

  /**
   * arr assign
   * a for array, v for prev value, n for new value
   */
  extern void Tgc_ref(void* a, int vt, void* v, int nt, void* n) {
	if (v != n) {
		Tgc_dec_ref(vt, v);
		if (is_valid(vt, v))
		if (registry.count(v))
			registry[a]->remove_depency(registry[v]);
	}
    Tgc_inc_ref(nt, n);
    if (is_valid(nt, n)) {
      registry[a]->depency(registry[n]);
    }
    registry[a]->print_info();
  }

  /**
   * before Tgc_collect
   */
  extern void Tgc_dec_ref(int t, void* p) {
    if (is_valid(t, p)) {
      registry[p]->dec_ref();
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
    for(auto iter : free_q) {
      void * ptr = iter;
      auto it = registry.find(ptr);
      delete it->second;
      registry.erase(it);
      free(ptr);
    }
    Tgc_clear_q();
  }

}
