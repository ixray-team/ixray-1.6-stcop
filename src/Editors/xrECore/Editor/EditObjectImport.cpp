//----------------------------------------------------
// file: CEditableObjectImport.cpp
//----------------------------------------------------

#include "stdafx.h"


#include "EditObject.h"
#include "EditMesh.h"

using VMIndexLink = xr_map<void*, int>;
using VMIndexLinkIt = VMIndexLink::iterator;

bool CompareFunc(const st_VMapPt& vm0, const st_VMapPt& vm1) 
{
	return vm0.vmap_index < vm1.vmap_index;
};