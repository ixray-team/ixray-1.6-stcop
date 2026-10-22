#include "stdafx.h"

#include "xrMU_Model.h"
void xrMU_Model::calc_faceopacity()
{
	for (auto& m_face : m_faces)
	{
		m_face->CacheOpacity();
	}
}
