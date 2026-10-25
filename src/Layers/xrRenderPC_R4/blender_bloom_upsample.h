#pragma once


class CBlender_bloom_upsample : public IBlender
{
public:
	virtual const char*		getComment() override { return "INTERNAL: bloom upsample"; }
	virtual bool		canBeDetailed() override { return false; }
	virtual bool		canBeLMAPped() override { return false; }

	virtual void		Compile(CBlender_Compile& C) override;

	CBlender_bloom_upsample();
	virtual ~CBlender_bloom_upsample() override;
};