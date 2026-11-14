#include "stdafx.h"
#include "EditorViewport.h"

bool EditorViewport::IsTargetSized() const
{
	return Target && Target->pSurface && Target->dwWidth == u32(Size.x) && Target->dwHeight == u32(Size.y);
}

bool EditorViewport::UpdateTarget()
{
	if (!HasSize() || IsTargetSized())
	{
		return false;
	}

	// The name is unique per view: _CreateRT returns a live target of the same name as is, whatever its size
	const xr_string Name = "$user$viewport_" + xr_string::ToString(Id);
	Target.destroy();
	Target.create(Name.c_str(), u32(Size.x), u32(Size.y), ERHI_FORMAT::B8G8R8X8_UNORM);
	return true;
}
