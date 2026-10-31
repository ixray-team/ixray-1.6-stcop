#include "stdafx.h"

#include "ResourceManager.h"
#include "r_constants.h"
#include "dxRenderDeviceRender.h"

R_constant_table::~R_constant_table()
{
	DEV->_DeleteConstantTable(this);
}
