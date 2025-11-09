#pragma once

const		u32					occq_size			= 2*768; //256	;	// queue for occlusion queries

// must conform to following order of allocation/free
// a(A), a(B), a(C), a(D), ....
// f(A), f(B), f(C), f(D), ....
// a(A), a(B), a(C), a(D), ....
//	this mean:
//		use as litle of queries as possible
//		first try to use queries allocated first
//	assumption:
//		used queries number is much smaller than total count

class R_occlusion
{
public:
#ifdef USE_DX11
	typedef u64 occq_result;
#else
	typedef u32 occq_result;
#endif

private:
	struct _Q
	{
		u32 order;
		ID3DQuery* Q = nullptr;
		u32 UserHandle = 0xFFFFFFFF; // invalid
		bool Active = false;
	};

	static const u32 iInvalidHandle = 0xFFFFFFFF;
	static const u32 OCCQ_PENDING = 0xFFFFFFFE;
	static const u32 OCCQ_LOST = 0xFFFFFFFD;

	BOOL enabled = TRUE;
	xr_vector<_Q> pool;
	xr_vector<_Q> used;
	xr_vector<u32> fids;
	xr_hash_map<u32, occq_result> results;
	static constexpr u32 kLatency = 3;
	std::array<xr_vector<u32>, kLatency> frames;
	u32 occq_size = 1536;

public:
	R_occlusion();
	~R_occlusion();

	void occq_create(u32 limit);
	void occq_destroy();
	void occq_refresh();
	void occq_stats() const;

	u32 occq_begin(u32& ID);
	void occq_end(u32& ID);
	occq_result occq_get(u32& ID, u32 timeout_ms = 5);
	void OnFrameEnd();
};