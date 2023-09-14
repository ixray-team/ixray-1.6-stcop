#include "pch.h"

using namespace Rendering;

WallMarkArray::~WallMarkArray()
{
}

void 
WallMarkArray::Copy(IWallMarkArray& _in)
{
}

void
WallMarkArray::AppendMark(LPCSTR s_textures)
{
}

void 
WallMarkArray::clear()
{
}

bool 
WallMarkArray::empty()
{
	return false;
}

wm_shader 
WallMarkArray::GenerateWallmark()
{
	return wm_shader();
}
