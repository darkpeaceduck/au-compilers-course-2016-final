#include "allocator.h"
#include <vector>
#include <cassert>
#include <memory.h>
#include <iostream>

const size_t check_sz[] = { 1, 2, 3, 4, 5, 10, 15, 30, 40, 100, 500, 1000, 5000,
		10000 };

const size_t max_sz = 1LL<<15;
const size_t numpages = 2;
const size_t maxpools = 2;

void check_mem(char * mem, size_t sz) {
	memset(mem, 0, sz);
	for(size_t i = 0; i < sz; i++)
		mem[i] = i % 109;
	for(size_t i = 0; i < sz; i++)
		assert(mem[i] == i % 109);
}
void check(CachedAllocator &alloc, size_t sz, size_t count) {
	std::cerr << "Checking " << sz << " " << count << std::endl;
	std::vector<void*> mems;
	for(size_t i = 0; i < count; i++) {
		char * mem = (char*)alloc.allocate(sz);
		check_mem(mem, sz);
		mems.push_back(mem);
	}
	for(auto mem : mems)
		alloc.deallocate(mem);
}

int main() {
	CachedAllocator alloc(max_sz, maxpools, numpages);
	for(auto item : check_sz) {
		check(alloc, item, max_sz / item);
	}
}
