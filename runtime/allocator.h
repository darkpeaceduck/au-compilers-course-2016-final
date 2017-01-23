#pragma once
#include <memory>
#include <unistd.h>

const long PAGE_SIZE = sysconf(_SC_PAGESIZE);


class CachedAllocatorPriv;
class CachedAllocator {
	std::shared_ptr<CachedAllocatorPriv> priv;
	size_t last_memory_counter = 0;
public:
	CachedAllocator(const size_t max_sz, const size_t maxpools, const size_t numpages);
	void * allocate(size_t sz);
	void deallocate(void *ptr);
	size_t last_memory_allocated();
	void clear_last_memory_counter();
};
