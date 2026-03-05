#pragma once


class CBlender_bloom_upsample : public IBlender
{
public:
	virtual		LPCSTR		getComment() { return "INTERNAL: bloom upsample"; }
	virtual		BOOL		canBeDetailed() { return FALSE; }
	virtual		BOOL		canBeLMAPped() { return FALSE; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_bloom_upsample();
	virtual ~CBlender_bloom_upsample();
};