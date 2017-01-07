#include <map>
#include <memory>
#include <vector>
#include <stdint.h>
#include <malloc.h>
using namespace std;

static vector<void*> free_q;

class RegisterItem{
	uint64_t refs;
	void * protect;
	vector<RegisterItem*> sub_objects;
public:
	RegisterItem() {}
	RegisterItem(void * ptr) : protect(ptr), refs(1), sub_objects() {}
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
};

static map<void *, RegisterItem> registry;

extern void * gc_malloc(size_t size) {
	void * ptr = malloc(size);
	registry[ptr] = RegisterItem();
	return ptr;
}

extern "C" {
extern void Tgc_inc_ref(void * ptr) {
	registry[ptr].inc_ref();
}

extern void Tgc_ref(void * a, void * b) {
	Tgc_inc_ref(b);
	registry[a].depency(&registry[b]);
}

extern void Tgc_dec_ref(void * ptr) {
	registry[ptr].dec_ref();
}

extern void Tgc_collect(void * ptr) {
	for(auto iter : free_q) {
		void * ptr = iter;
		free(ptr);
	}
}

}
