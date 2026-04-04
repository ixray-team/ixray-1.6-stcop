#include "stdafx.h"

#include "xrMU_Model.h"
void xrMU_Model::calc_faceopacity()
{
	for (auto Face : m_faces)
	{
		Face->CacheOpacity();
	}
}
