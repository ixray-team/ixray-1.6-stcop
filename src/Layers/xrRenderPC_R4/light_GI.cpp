#include "StdAfx.h"
#include "../xrRender/light.h"

IC bool		pred_LI			(const light_indirect& A, const light_indirect& B)
{
	return A.E > B.E;
}
