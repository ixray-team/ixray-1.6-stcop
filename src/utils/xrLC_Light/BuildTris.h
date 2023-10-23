#pragma once

class TRI_Build
{
public:
	u32				verts[3];				// 3*4 = 12b
#ifdef _M_X64
	union 
	{
		u64			dummy;					// 8b
		struct 
		{
			u64		material : 14;			// 
			u64		suppress_shadows : 1;	// 
			u64		suppress_wm : 1;		// 
			u64		sector : 16;			// 
			u64		dumb : 32;
		};
		struct 
		{
			u32 dummy_low;
			u32 dummy_high;
		};
	};
#else
	union 
	{
		u32			dummy;					// 4b
		struct 
		{
			u32		material : 14;			// 
			u32		suppress_shadows : 1;	// 
			u32		suppress_wm : 1;		// 
			u32		sector : 16;			// 
		};
	};
#endif
public:
	inline u32			IDvert(u32 ID) { return verts[ID]; }
};