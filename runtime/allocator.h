#pragma once
#include <memory>
class CachedAllocatorPriv;
class CachedAllocator {
	std::shared_ptr<CachedAllocatorPriv> priv;
public:
	CachedAllocator(const size_t max_sz, const size_t maxpools, const size_t numpages);
	void * allocate(size_t sz);
	void deallocate(void *ptr);
};
