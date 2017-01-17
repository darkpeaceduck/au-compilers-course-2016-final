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

class RegisterItem {
	void* protect;
	enum RegisterItemType{
		GLOBAL, LOCAL
	} type;
	int global_cnt = 0;
	multiset<RegisterItem*> sub_objects;
	typedef multiset<RegisterItem*>::iterator RegisterItemIterator;
public:
	RegisterItem() {
	}
	RegisterItem(void * ptr) :
			protect(ptr), type(LOCAL), sub_objects() {
	}
	void mark_global() {
		type = GLOBAL;
		global_cnt++;
	}
	void mark_local() {
		global_cnt--;
		if (global_cnt == 0)
			type = LOCAL;
	}
	bool is_global() {
		return type == GLOBAL;
	}
	void add_depency(RegisterItem *obj) {
		sub_objects.insert(obj);
	}
	void remove_depency(RegisterItem * ptr) {
		auto itr = sub_objects.find(ptr);
		if (itr != sub_objects.end()) {
			sub_objects.erase(itr);
		}
	}
	RegisterItemIterator depency_begin() {
		return sub_objects.begin();
	}
	RegisterItemIterator depency_end() {
		return sub_objects.end();
	}
};

static map<void*, RegisterItem *> registry;
static set<RegisterItem *> reachable;
static int scope_ptr = 0;

static bool in_main_scope() {
	return scope_ptr == 0;
}

static bool is_valid(int t, void* p) {
	return t && registry.count(p);
}

static void clean_ptr(void * ptr) {
	auto it = registry.find(ptr);
	if (it != registry.end()) {
		delete it->second;
		registry.erase(it);
		free(ptr);
	}
}

void collect_dfs(RegisterItem * root) {
	if (reachable.count(root))
		return;
	reachable.insert(root);
	for(auto item = root->depency_begin(); item != root->depency_end(); item++) {
		 RegisterItem * reg_item = *item;
		 collect_dfs(reg_item);
	}
}


extern void* gc_malloc(size_t size) {
	void* ptr = malloc(size);
	RegisterItem * item = new RegisterItem(ptr);
	registry[ptr] = item;
	if (in_main_scope()) {
		item->mark_global();
	}
	return ptr;
}


extern "C" {

extern void Tgc_clear_q() { }

extern void Tgc_inc_ref(int t, void* p) {
	scope_ptr++;
	if (is_valid(t, p)) {
		registry[p]->mark_global();
	}
}

extern void Tgc_dec_ref(int t, void* p);

extern void Tgc_ref(void* a, int vt, void* v, int nt, void* n) {
	if (is_valid(nt, n)) {
		registry[a]->add_depency(registry[n]);
	}
}

extern void Tgc_dec_ref(int t, void* p) {
	scope_ptr--;
	if (is_valid(t, p)) {
		registry[p]->mark_local();
	}
}

extern void Tgc_collect() {
	vector<void *> clean;
	reachable.clear();
	if (!in_main_scope()) {
		for(auto it : registry) {
			RegisterItem * item = it.second;
			if (item->is_global()) {
				collect_dfs(item);
			}
		}
	}
	for(auto it : registry) {
		RegisterItem * item = it.second;
		if (!reachable.count(item)) {
			clean.push_back(it.first);
		}
	}
	for(auto ptr : clean) {
		clean_ptr(ptr);
	}
}


}
