#include "stdafx.h"

XRayWallMarkArray::XRayWallMarkArray()
{
}

void XRayWallMarkArray::Copy(IWallMarkArray & _in)
{
}

void XRayWallMarkArray::AppendMark(LPCSTR s_textures)
{
}

void XRayWallMarkArray::clear()
{
}

bool XRayWallMarkArray::empty()
{
	return false;
}

wm_shader XRayWallMarkArray::GenerateWallmark()
{
	static wm_shader s_wallmark;
	return s_wallmark;
}
