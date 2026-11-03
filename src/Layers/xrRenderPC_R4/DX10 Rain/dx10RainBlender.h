#ifndef	dx10RainBlender_included
#define	dx10RainBlender_included

class CBlender_rain : public IBlender  
{
public:
	virtual const char*		getComment() override { return "INTERNAL: DX10 rain blender";	}
	virtual bool		canBeDetailed() override { return false;	}
	virtual bool		canBeLMAPped() override { return false;	}

	virtual void		Compile			(CBlender_Compile& C) override;
};

#endif	//	dx10RainBlender_included