#include <memory>
#include <vector>
#include <map>
#include <set>
#include <unistd.h>
#include <sys/types.h>
#include <stddef.h>
#include <stdint.h>
#include "allocator.h"

#define container_of(ptr, type, member) ({            \
 (type *)( (char *)ptr - offsetof(type,member) );})

const long PAGE_SIZE = sysconf(_SC_PAGESIZE);

class PoolAllocator {
	std::vector<void *> pool;
	void * mem;
	const size_t obj_total;
	const size_t pool_total;
	const size_t obj_size;
	void allocate_mem() {
		pool.reserve(obj_total);
		mem = malloc(pool_total);
		if (mem == NULL)
			throw std::overflow_error("can't allocate mem for pool");
		char * ptr = (char *) mem;
		for (size_t i = 0; i < obj_total; i++, ptr += obj_size) {
			pool.push_back(ptr);
		}
	}
	void deallocate_mem() {
		free(mem);
	}
public:
	PoolAllocator(const size_t obj_sz, const size_t numpages) :
			pool(), mem(NULL), pool_total(PAGE_SIZE * numpages), obj_size(
					obj_sz), obj_total(pool_total / obj_size) {
		allocate_mem();
	}
	~PoolAllocator() {
		if (mem != NULL)
			deallocate_mem();
	}
	bool empty() {
		return pool.empty();
	}
	bool full() {
		return pool.size() == obj_total;
	}
	void* allocate() {
		if (pool.empty())
			throw std::overflow_error("can't allocate obj - pool is empty");
		void * ret = pool.back();
		pool.pop_back();
		return ret;
	}
	void deallocate(void *ptr) {
		pool.push_back(ptr);
	}
};

class FixedSizeAllocator {
	struct StorageItem {
		size_t pool_index;
		char data[0];
	};
	enum {
		UNDEFINED_POOL
	};
	std::map<size_t, std::shared_ptr<PoolAllocator>> pools;
	ptrdiff_t current = UNDEFINED_POOL;
	size_t pcnt = 0;
	std::set<size_t> full_pools;
	const size_t maxpools;
	const size_t obj_sz;
	const size_t numpages;
	void allocate_pool() {
		pools[pcnt] = std::make_shared<PoolAllocator>(obj_sz, numpages);
		full_pools.insert(pcnt);
		pcnt++;
	}
	void choise_pool() {
		if (current == UNDEFINED_POOL) {
			if (full_pools.empty()) {
				allocate_pool();
			}
			current = *full_pools.begin();
		}
	}
	void try_free_redudant_pools() {
		if (full_pools.size() * 2 > pools.size()) {
			ptrdiff_t bound = full_pools.size() / 2;
			for (auto iter = full_pools.begin(); bound > 0; iter++, bound--) {
				pools.erase(*iter);
				iter = full_pools.erase(iter);
			}
		}
	}
public:
	FixedSizeAllocator(const size_t obj_sz, const size_t numpages,
			const size_t maxpools) :
			obj_sz(obj_sz + sizeof(StorageItem)), numpages(numpages), maxpools(
					maxpools), pools(), full_pools() {
	}
	void * allocate() {
		if (current == UNDEFINED_POOL && pools.size() == maxpools)
			throw std::overflow_error(
					"can't allocate obj - all pools are full");
		if (current == UNDEFINED_POOL || pools[current]->empty()) {
			current = UNDEFINED_POOL;
			choise_pool();
		}
		void * ret = pools[current]->allocate();
		/* can't failed here */
		auto iter = full_pools.find(current);
		if (iter != full_pools.end())
			full_pools.erase(iter);
		StorageItem * item = (StorageItem *) ret;
		item->pool_index = current;
		return item->data;
	}

	void deallocate(void *ptr) {
		StorageItem * item = container_of(ptr, StorageItem, data);
		size_t index = item->pool_index;
		pools[index]->deallocate(item);
		if (pools[index]->full())
			full_pools.insert(index);
		try_free_redudant_pools();
	}
};

class CachedAllocatorPriv {
	struct StorageItem {
		size_t pool_index;
		char data[0];
	};
	const size_t maxpools;
	const size_t numpages;
	const size_t max_sz;
	std::vector<std::shared_ptr<FixedSizeAllocator>> pools;
	static size_t order_index(size_t sz) {
		if (sz == 0)
			return 0;
		return (sizeof(sz) * 8) - __builtin_clzll(sz);
	}
public:
	CachedAllocatorPriv(const size_t max_sz, const size_t maxpools,
			const size_t numpages) :
			max_sz(max_sz), maxpools(maxpools), numpages(numpages) {
		for (size_t sz = 1; sz <= max_sz; sz *= 2) {
			pools.push_back(
					std::make_shared<FixedSizeAllocator>(sz, numpages,
							maxpools));
		}
	}
	void * allocate(size_t sz) {
		sz += sizeof(StorageItem);
		size_t index = order_index(sz);
		if (index >= pools.size()) {
			throw std::runtime_error("too big allocating size");
		}
		StorageItem * item = (StorageItem *) pools[index]->allocate();
		item->pool_index = index;
		return item->data;
	}
	void deallocate(void *ptr) {
		StorageItem * item = container_of(ptr, StorageItem, data);
		pools[item->pool_index]->deallocate(item);
	}
};

CachedAllocator::CachedAllocator(const size_t max_sz, const size_t maxpools,
		const size_t numpages) {
	priv = std::make_shared<CachedAllocatorPriv>(max_sz, maxpools, numpages);
}

void *CachedAllocator::allocate(size_t sz) {
	return priv->allocate(sz);
}

void CachedAllocator::deallocate(void *ptr) {
	return priv->deallocate(ptr);
}

