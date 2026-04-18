#pragma once

class CBlender_bloom_downsample : public IBlender
{
public:
	virtual		const char*		getComment() { return "INTERNAL: bloom downsample"; }
	virtual		bool		canBeDetailed() { return FALSE; }
	virtual		bool		canBeLMAPped() { return FALSE; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_bloom_downsample();
	virtual ~CBlender_bloom_downsample();
}; 
