#pragma once

class CBlender_bloom_downsample : public IBlender
{
public:
	virtual		const char*		getComment() { return "INTERNAL: bloom downsample"; }
	virtual		bool		canBeDetailed() { return false; }
	virtual		bool		canBeLMAPped() { return false; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_bloom_downsample();
	virtual ~CBlender_bloom_downsample();
}; 
