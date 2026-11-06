#include "stdafx.h"
#include <utility>


using namespace std;

namespace
{
	bool smem_less(const smem_value* Value, const std::pair<u32, u32>& Key)
	{
		if (Value->dwCRC < Key.first)	return true;
		if (Value->dwCRC > Key.first)	return false;
		return Value->dwLength < Key.second;
	}
}

XRCORE_API	smem_container*	g_pSharedMemoryContainer	= nullptr;

smem_value*			smem_container::find_locked		(u32 dwCRC, u32 dwLength, const void* ptr) const
{
	auto it = std::lower_bound(container.begin(), container.end(), std::make_pair(dwCRC, dwLength), smem_less);

	for (; it != container.end(); ++it)
	{
		if ((*it)->dwCRC	!= dwCRC)		break;
		if ((*it)->dwLength != dwLength)	break;
		if (0 == memcmp((*it)->value, ptr, dwLength))
			return *it;
	}

	return nullptr;
}

smem_value*			smem_container::dock			(u32 dwCRC, u32 dwLength, const void* ptr)
{
	PROF_EVENT(__FUNCTION__);
	VERIFY(dwLength && ptr); // данные пришли валидные с dwCRC = 0, мне охренеть как "повезло"?

	{
		xrSRWLockGuard guard(RWLock, true);
		if (smem_value* result = find_locked(dwCRC, dwLength, ptr))
		{
			++result->dwReference;
			return result;
		}
	}

	// if not found - create new entry outside the lock
	smem_value* result = (smem_value*)Memory.mem_alloc(4 * sizeof(u32) + dwLength);
	result->dwReference = 0;
	result->dwCRC = dwCRC;
	result->dwLength = dwLength;
	CopyMemory(result->value, ptr, dwLength);

	{
		xrSRWLockGuard guard(RWLock, false);

		// double-check: another thread could have inserted the same value
		if (smem_value* existing = find_locked(dwCRC, dwLength, ptr))
		{
			++existing->dwReference;
			xr_free(result);
			return existing;
		}

		auto it = std::lower_bound(container.begin(), container.end(), std::make_pair(dwCRC, dwLength), smem_less);
		container.insert(it, result);
		++result->dwReference;
		return result;
	}
}

void				smem_container::clean			()
{
	xrSRWLockGuard	guard(RWLock, false);

	cdb::iterator	it	= container.begin	();
	cdb::iterator	end	= container.end		();
	for (; it!=end; it++)	if (0==(*it)->dwReference)	xr_free	(*it);
	container.erase	(remove(container.begin(),container.end(),(smem_value*)nullptr),container.end());
	if (container.empty())	container.clear	();
}

void				smem_container::dump			()
{
	xrSRWLockGuard	guard(RWLock, true);

	cdb::iterator	it	= container.begin	();
	cdb::iterator	end	= container.end		();

	FILE* F = nullptr;
	fopen_s(&F, "x:\\$smem_dump$.txt","w");
	if (nullptr == F)
		return;

	for (; it!=end; it++)
		fprintf		(F,"%4d : crc[%6x], %d bytes\n",(*it)->dwReference.load(),(*it)->dwCRC,(*it)->dwLength);
	fclose			(F);
}

u32					smem_container::stat_economy	()
{
	xrSRWLockGuard	guard(RWLock, true);

	cdb::iterator	it		= container.begin	();
	cdb::iterator	end		= container.end		();
	s64				counter	= 0;
	counter			-= sizeof(*this);
	counter			-= sizeof(cdb::allocator_type);
	const int		node_size = 20;
	for (; it!=end; it++)	{
		counter		-= 16;
		counter		-= node_size;
		counter		+= s64((s64((*it)->dwReference) - 1)*s64((*it)->dwLength));
	}

	return			u32(s64(counter)/s64(1024));
}

smem_container::~smem_container	()
{
	clean			();
}
