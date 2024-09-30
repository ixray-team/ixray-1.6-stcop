#include "stdafx.h"

#include "xrlight_implicitcalcglobs.h"
#include "xrLight_implicitdeflector.h"
#include "xrface.h"
#include "xrlc_globaldata.h"


void ImplicitCalcGlobs::Allocate()
{
		ImplicitHash	= new IHASH	();
}
void	ImplicitCalcGlobs::Deallocate()
{
	xr_delete( ImplicitHash );
}

void	ImplicitCalcGlobs::Initialize( ImplicitDeflector &d )
{
	defl = &d;
	R_ASSERT( defl );
	Fbox2 bounds;
	defl->Bounds_Summary			(bounds);
	Hash().initialize	(bounds,(u32)defl->faces.size());
	for (u32 fid=0; fid<defl->faces.size(); fid++)
	{
		Face* F				= defl->faces[fid];
		F->AddChannel		(F->tc[0].uv[0],F->tc[0].uv[1],F->tc[0].uv[2]); // make compatible format with LMAPs
		defl->Bounds			(fid,bounds);
		ImplicitHash->add	(bounds,F);
	}
}