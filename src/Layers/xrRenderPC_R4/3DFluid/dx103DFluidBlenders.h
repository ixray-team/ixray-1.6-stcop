#ifndef	dx103DFluidBlenders_included
#define	dx103DFluidBlenders_included
#pragma once

class CBlender_fluid_advect	: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: 3dfluid maths";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;
};

class CBlender_fluid_advect_velocity	: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: 3dfluid maths";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;
};

class CBlender_fluid_simulate	: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: 3dfluid maths";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;
};

class CBlender_fluid_obst	: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: 3dfluid maths 2";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;
};

class CBlender_fluid_emitter	: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: 3dfluid emitters";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;
};

class CBlender_fluid_obstdraw	: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: 3dfluid maths 2";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;
};

class CBlender_fluid_raydata	: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: 3dfluid maths 2";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;
};

class CBlender_fluid_raycast	: public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: 3dfluid maths 2";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;
};

#endif	//	dx103DFluidBlenders_included