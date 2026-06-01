#include "stdafx.h"
#include "XRayFHierrarhyVisual.h"

void CDS0_FHierrarhyVisual::Release()
{
	if (!m_DontDelete) {
		for (u32 i = 0; i < children.size(); i++)
			children[i]->Release();
	}
}

void CDS0_FHierrarhyVisual::Load(const char* N, IReader* data, u32 dwFlags)
{
	CDS0_RenderVisual::Load(N, data, dwFlags);
	if (data->find_chunk(OGF_CHILDREN_L))
	{
		// From Link
		u32 count = data->r_u32();
		children.resize(count);
		for (u32 i = 0; i < count; i++) {

			u32 ID = data->r_u32();
			children[i] = nullptr;// GRenderInterface.GetVisual(ID);
			VERIFY(!"Check Me!");
		}
		m_DontDelete = true;
	}
	else
	{
		if (data->find_chunk(OGF_CHILDREN))
		{
			// From stream
			IReader* OBJ = data->open_chunk(OGF_CHILDREN);
			if (OBJ) {
				IReader* O = OBJ->open_chunk(0);
				for (int count = 1; O; count++) 
				{
					string_path			name_load, short_name, num;
					xr_strcpy(short_name, N);
					if (strext(short_name)) *strext(short_name) = 0;
					xr_strcpy(name_load, short_name);
					xr_strcat(name_load, ":");
					xr_strcat(name_load, itoa(count, num, 10));
					children.push_back((CDS0_RenderVisual*)::Render->model_CreateChild(name_load, O));
					O->close();
					O = OBJ->open_chunk(count);
				}
				OBJ->close();
			}
			m_DontDelete = false;
		}
		else
		{
			FATAL("Invalid visual");
		}
	}
}

void CDS0_FHierrarhyVisual::Copy(CDS0_RenderVisual* from)
{
	CDS0_RenderVisual::Copy(from);
	CDS0_FHierrarhyVisual* pFrom = (CDS0_FHierrarhyVisual*)from;

	children.clear();
	children.reserve(pFrom->children.size());

	for (u32 i = 0; i < pFrom->children.size(); i++) 
	{
		CDS0_RenderVisual* p = (CDS0_RenderVisual*) ::Render->model_Duplicate(pFrom->children[i]);
		children.push_back(p);
	}
	m_DontDelete = false;
}

CDS0_FHierrarhyVisual::CDS0_FHierrarhyVisual()
{
}

CDS0_FHierrarhyVisual::~CDS0_FHierrarhyVisual()
{
	if (!m_DontDelete) {
		for (u32 i = 0; i < children.size(); i++)
			::Render->model_Delete((IRenderVisual*&)children[i]);
	}
	children.clear();
}
