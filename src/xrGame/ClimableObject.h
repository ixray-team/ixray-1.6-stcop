#pragma once
#include "PhysicsShellHolder.h"
#include "../xrPhysics/iclimableobject.h"
class IPHStaticGeomShell;
class IPHCharacter;
struct SGameMtl;
class CPHContact;
class CClimableObject: 
public CPhysicsShellHolder,
public IClimableObject
#ifdef DEBUG_DRAW
,public pureRender
#endif
{
	typedef	CPhysicsShellHolder		inherited;
	//CPHLeaderGeomShell* m_pStaticShell;
	IPHStaticGeomShell* m_pStaticShell;
	Fobb				m_box;
	Fvector				m_axis;
	Fvector				m_side;
	Fvector				m_norm;
	float				m_radius;
	u16					m_material;
public:
					CClimableObject		();
	virtual ~CClimableObject	() override;
	virtual void	Load				( const char* section) override;
	virtual bool	net_Spawn			( CSE_Abstract* DC) override;
	virtual void	net_Destroy			() override;
	virtual void	shedule_Update		( u32 dt) override;							// Called by sheduler
	virtual void	UpdateCL			( ) override;								// Called each frame, so no need for dt
	virtual void	Center				(Fvector &C) const override;
	virtual float	Radius				() const override;

	const Fobb&		BBox				() { return m_box; }
	const Fvector&	Axis				() { return m_axis; }
	const Fvector&	Side				() { return m_side; }
	const Fvector&	Norm				() { return m_norm; }

////////////////////////////////////////////////////////////////////
	virtual IPhysicsShellHolder			*cast_IPhysicsShellHolder	() override { return this; }
	virtual CPhysicsShellHolder*		cast_physics_shell_holder() override { return this; }
	virtual CClimableObject*			cast_climable_object() override { return this; }

#ifdef DEBUG_DRAW
	virtual void	OnRender			() override;
#endif
protected:
	virtual bool	UsedAI_Locations	() override;
public:
			const Fvector&	Axis				()const		{return m_axis;}
	virtual float			DDAxis				(Fvector& dir)const override;

	virtual const Fvector&	Side				()const {return m_side;}
	virtual float			DDSide				(Fvector& dir)const override;

	virtual const Fvector&	Norm				()const override {return m_norm;}
	virtual float			DDNorm				(Fvector &dir)const override;
	virtual bool			BeforeLadder		(IPHCharacter *actor,float tolerance=0.f)const override;
	virtual float			DDLowerP			(IPHCharacter	*actor,Fvector &out_dir)const override;//returns distance and dir to lover point
	virtual float			DDUpperP			(IPHCharacter	*actor,Fvector &out_dir)const override;//returns distance and dir to upper point

			void			DToAxis				(IPHCharacter	*actor,Fvector &dir)const;
	virtual float			DDToAxis			(IPHCharacter	*actor,Fvector &out_dir)const override;//returns distance and dir to ladder axis
			void			POnAxis				(IPHCharacter	*actor,Fvector	&P)const;

	virtual float			AxDistToUpperP		(IPHCharacter	*actor)const override;
	virtual float			AxDistToLowerP		(IPHCharacter	*actor)const override;

			void			DSideToAxis			(IPHCharacter	*actor,Fvector	&dir)const;
			float			DDSideToAxis		(IPHCharacter	*actor,Fvector	&dir)const;

	virtual void			DToPlain			(IPHCharacter	*actor,Fvector	&dist)const override;
	virtual float			DDToPlain			(IPHCharacter	*actor,Fvector	&dir)const override;
			bool			InRange				(IPHCharacter	*actor)const;
	virtual bool			InTouch				(IPHCharacter	*actor)const override;
	virtual u16				Material			()const override {return m_material; }
			void			LowerPoint			(Fvector	&P)const;
			void			UpperPoint			(Fvector	&P)const;
			void			DefineClimbState	(IPHCharacter	*actor)const;
	static 	void			ObjectContactCallback(bool& /**do_colide/**/,bool bo1,CPHContact& c,SGameMtl * /*material_1*/,SGameMtl * /*material_2*/);

public:
	virtual bool	register_schedule	() const override {return false;}
};
