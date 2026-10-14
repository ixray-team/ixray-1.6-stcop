#include "stdafx.h"


#include "Blender.h"

//////////////////////////////////////////////////////////////////////
#include "Blender_CLSID.h"
IC bool		p_sort			(IBlender* A, IBlender* B)
{
	return _stricmp(A->getComment(),B->getComment())<0;
}

void		IBlender::CreatePalette(xr_vector<IBlender*> &palette)
{
	// Create palette itself
	R_ASSERT(palette.empty());
	palette.push_back(Create(B_DEFAULT));
	palette.push_back(Create(B_DEFAULT_AREF));
	palette.push_back(Create(B_VERT));
	palette.push_back(Create(B_VERT_AREF));
	palette.push_back(Create(B_SCREEN_SET));
	palette.push_back(Create(B_SCREEN_GRAY));
	palette.push_back(Create(B_EDITOR_WIRE));
	palette.push_back(Create(B_EDITOR_SEL));
	//palette.push_back(Create(B_LIGHT));
	palette.push_back(Create(B_LaEmB));
	palette.push_back(Create(B_LmEbB));
	palette.push_back(Create(B_BmmD));
	//palette.push_back(Create(B_B));
	palette.push_back(Create(B_SHADOW_WORLD));
	palette.push_back(Create(B_BLUR));
	palette.push_back(Create(B_MODEL));
	palette.push_back(Create(B_MODEL_EbB));
	palette.push_back(Create(B_DETAIL));
	palette.push_back(Create(B_TREE));
	palette.push_back(Create(B_PARTICLE));

	// Every class of shaders.xr stays a template. One implementation may serve several classes
	// (the game renderer draws B_VERT with the B_DEFAULT blender) and keeps the requested class,
	// so duplicates are told apart by class, not by type.
	for (u32 i=0; i<palette.size(); i++)
	{
		IBlender* A		= palette[i];
		R_ASSERT2(A, "Blender class of the palette is not provided by the renderer or the editors");
		for (u32 j=i+1; j<palette.size(); )
		{
			IBlender* B		= palette[j];
			if (B && A->getDescription().CLS == B->getDescription().CLS)
			{
				xr_delete(palette[j]);
				palette.erase(palette.begin() + j);
				continue;
			}
			j++;
		}
	}

	// Sort by desc and return
	std::sort		(palette.begin(),palette.end(),p_sort);
}

// Engine
#include "../../../xrEngine/Render.h"
#include "../EditorRenderHost.h"

IBlender* IBlender::Create(CLASS_ID cls)
{
	if (g_EditorRenderHost)
	{
		if (IBlender* EditorBlender = g_EditorRenderHost->CreateBlender(cls))
		{
			return EditorBlender;
		}
	}

	return ::RImplementation.blender_create	(cls);
}
