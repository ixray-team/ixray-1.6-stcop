// File:		UI_IB_FrameLineWnd.cpp
// Description:	Inheritance of UIInteractiveBackground template class with some
//				CUIFrameLineWnd features
// Created:		07.04.2014
// Author:		SkyLoader
// Mail:		nope

// Copyright 2014 Lost Alpha, вот так нах

#include "StdAfx.h"
#include "UI_IB_FrameLineWnd.h"

void CUI_IB_FrameLineWnd::SetStretchBETextures(bool stretch)
{
	for(int i=0; i<S_Total; ++i)
		if(m_states[i])
			m_states[i]->SetStretchBETextures(stretch);
}
