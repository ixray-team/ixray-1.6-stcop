// File:		UI_IB_FrameLineWnd.h
// Description:	Inheritance of UIInteractiveBackground template class with some
//				CUIFrameLineWnd features
// Created:		07.04.2014
// Author:		SkyLoader
// Mail:		nope

// Copyright 2014 Lost Alpha, вот так нах

#pragma once

#include "UIInteractiveBackground.h"
#include "UIFrameLineWnd.h"

class CUI_IB_FrameLineWnd : public CUIInteractiveBackground<CUIFrameLineWnd> 
{
public:
	virtual void SetStretchBETextures	(bool stretch);
};