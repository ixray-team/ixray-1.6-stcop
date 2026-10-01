#pragma once

class CBlender_new_adaptation : public IBlender
{
public:
	virtual		const char*		getComment() { return "INTERNAL: new adaptation calc"; }
	virtual		bool		canBeDetailed() { return false; }
	virtual		bool		canBeLMAPped() { return false; }

	virtual		void		Compile(CBlender_Compile& C);

	CBlender_new_adaptation();
	virtual ~CBlender_new_adaptation();
};

class CBlender_histogram_debug : public IBlender
{
public:
    CBlender_histogram_debug() { description.CLS = 0; }
    virtual const char* getComment() { return "INTERNAL: combine histogram debug"; }
    virtual bool canBeLMAPped() { return false; }
    virtual void Compile(CBlender_Compile& C);
};
