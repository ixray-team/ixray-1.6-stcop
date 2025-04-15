// xrXRC.cpp: implementation of the xrXRC class.
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#pragma hdrstop

#include "xrXRC.h"

thread_local CDB::COLLIDER CL;
ENGINE_API xrXRC XRC;

IC CDB::COLLIDER* xrXRC::collider()
{
	return &CL;
}
