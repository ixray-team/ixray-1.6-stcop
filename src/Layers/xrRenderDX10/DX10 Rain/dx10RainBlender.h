#ifndef	dx10RainBlender_included
#define	dx10RainBlender_included

class CBlender_rain : public IBlender  
{
public:
	virtual		const char*		getComment()	{ return "INTERNAL: DX10 rain blender";	}
	virtual		bool		canBeDetailed()	{ return FALSE;	}
	virtual		bool		canBeLMAPped()	{ return FALSE;	}

	virtual		void		Compile			(CBlender_Compile& C);
};

#endif	//	dx10RainBlender_included