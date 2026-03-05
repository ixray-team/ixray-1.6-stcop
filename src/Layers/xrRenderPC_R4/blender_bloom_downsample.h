#pragma once

class CBlender_bloom_downsample : public IBlender
{
public:
	virtual		LPCSTR		getComment() { return "INTERNAL: bloom downsample"; }
	virtual		BOOL		canBeDetailed() { return FALSE; }
	virtual		BOOL		canBeLMAPped() { return FALSE; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_bloom_downsample();
	virtual ~CBlender_bloom_downsample();
}; 
