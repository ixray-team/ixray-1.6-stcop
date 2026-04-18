#pragma once


class CBlender_bloom_upsample : public IBlender
{
public:
	virtual		const char*		getComment() { return "INTERNAL: bloom upsample"; }
	virtual		bool		canBeDetailed() { return FALSE; }
	virtual		bool		canBeLMAPped() { return FALSE; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_bloom_upsample();
	virtual ~CBlender_bloom_upsample();
};