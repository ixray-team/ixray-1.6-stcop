
#include "XRayFHierrarhyVisual.h"

#include "../../xrEngine/Fmesh.h"

void	XRayFHierrarhyVisual::Release()
{
	if (!m_DontDelete) {
		for (u32 i = 0; i < children.size(); i++)
			children[i]->Release();
	}
}

void XRayFHierrarhyVisual::Load(const char* N, IReader* data, u32 dwFlags)
{
	XRayRenderVisual::Load(N, data, dwFlags);
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
		m_DontDelete = TRUE;
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
					children.push_back((XRayRenderVisual*)::Render->model_CreateChild(name_load, O));
					O->close();
					O = OBJ->open_chunk(count);
				}
				OBJ->close();
			}
			m_DontDelete = FALSE;
		}
		else
		{
			FATAL("Invalid visual");
		}
	}
}

void XRayFHierrarhyVisual::Copy(XRayRenderVisual* from)
{
	XRayRenderVisual::Copy(from);

	XRayFHierrarhyVisual* pFrom = (XRayFHierrarhyVisual*)from;

	children.clear();
	children.reserve(pFrom->children.size());
	for (u32 i = 0; i < pFrom->children.size(); i++) {
		XRayRenderVisual* p = (XRayRenderVisual*) ::Render->model_Duplicate(pFrom->children[i]);
		children.push_back(p);
	}
	m_DontDelete = FALSE;
}

XRayFHierrarhyVisual::XRayFHierrarhyVisual()
{
}

XRayFHierrarhyVisual::~XRayFHierrarhyVisual()
{
	if (!m_DontDelete) {
		for (u32 i = 0; i < children.size(); i++)
			::Render->model_Delete((IRenderVisual*&)children[i]);
	}
	children.clear();
}
