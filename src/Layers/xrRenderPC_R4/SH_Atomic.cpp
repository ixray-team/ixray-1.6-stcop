#include "stdafx.h"
#include "SH_Atomic.h"
#include "ResourceManager.h"
#include "dxRenderDeviceRender.h"

///////////////////////////////////////////////////////////////////////
//	SVS
SVS::SVS() : vs(nullptr)
{
}

SVS::~SVS()
{
	DEV->_DeleteVS(this);
	_RELEASE(vs);
	_RELEASE(Signature);
}

///////////////////////////////////////////////////////////////////////
//	SPS
SPS::~SPS								()			{	_RELEASE(ps);		DEV->_DeletePS			(this);	}

///////////////////////////////////////////////////////////////////////
//	SGS
SGS::~SGS								()			{	_RELEASE(gs);		DEV->_DeleteGS			(this);	}
SHS::~SHS								()			{	_RELEASE(sh);		DEV->_DeleteHS			(this);	}
SDS::~SDS								()			{	_RELEASE(sh);		DEV->_DeleteDS			(this);	}
SCS::~SCS								()			{	_RELEASE(sh);		DEV->_DeleteCS			(this);	}

///////////////////////////////////////////////////////////////////////
//	SState
SState::~SState							()			{	_RELEASE(state);	DEV->_DeleteState		(this);	}
