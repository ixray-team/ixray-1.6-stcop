#pragma once
class IPhysicsShellHolder;
class IPHCharacter;
class IClimableObject
{
public:
//virtual	const Fvector&	Axis				()const		=0;
virtual	float			DDAxis				(Fvector& dir)const=0;
//
//virtual	const Fvector&	Side				()const=0;
virtual	float			DDSide				(Fvector& dir)const=0;
//
virtual	const Fvector&	Norm				()const=0;
virtual	float			DDNorm				(Fvector &dir)const=0;
virtual	bool			BeforeLadder		(IPHCharacter *actor,float tolerance=0.f)const=0;
virtual	float			DDLowerP			(IPHCharacter *actor,Fvector &out_dir)const=0;//returns distance and dir to lover point
virtual	float			DDUpperP			(IPHCharacter *actor,Fvector &out_dir)const=0;//returns distance and dir to upper point
//
//virtual	void			DToAxis				(IPHCharacter *actor,Fvector &dir)const=0;
virtual	float			DDToAxis			(IPHCharacter *actor,Fvector &out_dir)const=0;//returns distance and dir to ladder axis
//virtual	void			POnAxis				(IPHCharacter *actor,Fvector	&P)const=0;
//
virtual	float			AxDistToUpperP		(IPHCharacter *actor)const=0;
virtual	float			AxDistToLowerP		(IPHCharacter *actor)const=0;
//
//virtual	void			DSideToAxis			(IPHCharacter *actor,Fvector	&dir)const=0;
//virtual	float			DDSideToAxis		(IPHCharacter *actor,Fvector	&dir)const=0;
//	
virtual	void			DToPlain			(IPHCharacter *actor,Fvector	&dist)const=0;
virtual	float			DDToPlain			(IPHCharacter *actor,Fvector	&dir)const=0;
//virtual	bool			InRange				(IPHCharacter *actor)const=0;
virtual	bool			InTouch				(IPHCharacter *actor)const=0;
virtual u16				Material			()const=0;
//
//virtual	void			LowerPoint			(Fvector	&P)const=0;
//virtual	void			UpperPoint			(Fvector	&P)const=0;
//virtual	void			DefineClimbState	(IPHCharacter *actor)const=0;

virtual IPhysicsShellHolder			*cast_IPhysicsShellHolder	()=0;

protected:
	virtual	~IClimableObject() = default;
};