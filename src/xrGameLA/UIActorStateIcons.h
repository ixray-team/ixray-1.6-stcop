#pragma once
#ifndef __UIACTORSTATEICONS_H__
#define __UIACTORSTATEICONS_H__

#include "ActorState.h"
#include "UI\UIStatic.h"

class CUIActorStateIcons : public CUIStatic
{		
	private:
		typedef		 CUIWindow			inherited;
		enum	{ MAX_ICONS = 28, };
	public:
		
						CUIActorStateIcons				();
		virtual			~CUIActorStateIcons				();
		virtual void	Init							();
				void	SetIcons						(Flags32 set);
	private:
		CUIStatic		m_icons[MAX_ICONS];
};

#endif