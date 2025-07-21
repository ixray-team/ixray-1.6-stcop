#include "stdafx.h"

CDS0_WallMarkArray::CDS0_WallMarkArray()
{
}

void CDS0_WallMarkArray::Copy(IWallMarkArray & _in)
{
}

void CDS0_WallMarkArray::AppendMark(LPCSTR s_textures)
{
}

void CDS0_WallMarkArray::clear()
{
}

bool CDS0_WallMarkArray::empty()
{
	return false;
}

wm_shader CDS0_WallMarkArray::GenerateWallmark()
{
	static wm_shader s_wallmark;
	return s_wallmark;
}
