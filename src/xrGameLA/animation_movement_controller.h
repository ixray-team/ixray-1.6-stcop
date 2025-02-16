#pragma once
#include "../Include/xrRender/KinematicsAnimated.h"

class CBlend;
class IKinematics;
class animation_movement_controller
{
	Fmatrix&		m_pObjXForm;
	Fmatrix			m_startObjXForm;
	Fmatrix			m_startRootXform;
	IKinematics*	m_pKinematicsC;
	CBlend*			m_control_blend;
	static void	__stdcall RootBoneCallback				(CBoneInstance* B);
	void			deinitialize					();
public:		
			animation_movement_controller		( Fmatrix	*_pObjXForm, IKinematics *_pKinematicsC,CBlend *b );
			~animation_movement_controller		( );
			void	ObjStartXform				( Fmatrix &m )const { m.set( m_startObjXForm ) ;}
			CBlend*	ControlBlend				( ) const { return m_control_blend; }
			bool	isActive					( ) const ;
			void	OnFrame						( );
};