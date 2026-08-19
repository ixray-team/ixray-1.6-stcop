#include "../../Layers/xrRenderTiramisu/Editor/TiramisuEditorViewportSceneCulling.h"

#include <iostream>
#include <limits>

namespace
{
int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return 1;
}
} // namespace

int main(int argc, char** argv)
{
	if (argc < 2 || xr_string_view(argv[1]) != "-rdbg")
	{
		return Fail("xrEditorViewportSceneCullingTests requires -rdbg");
	}
	FEditorViewportCamera Camera;
	Camera.ViewProjection = {
		1.0f, 0.0f, 0.0f, 0.0f,
		0.0f, 1.0f, 0.0f, 0.0f,
		0.0f, 0.0f, 1.0f, 0.0f,
		0.0f, 0.0f, 0.0f, 1.0f
	};
	FEditorDecalInstance Decal;
	if (!IsTiramisuEditorDecalVisible(Decal, Camera))
	{
		return Fail("Visible identity decal was culled");
	}

	Decal.LocalToWorld[12] = 100.0f;
	if (IsTiramisuEditorDecalVisible(Decal, Camera))
	{
		return Fail("Off-screen decal was not culled");
	}

	Decal.LocalToWorld[0] = 400.0f;
	if (!IsTiramisuEditorDecalVisible(Decal, Camera))
	{
		return Fail("Frustum-intersecting large decal was culled");
	}

	Decal.LocalToWorld[0] = 1.0f;
	Decal.LocalToWorld[12] = 0.0f;
	Decal.LocalToWorld[14] = 100.0f;
	if (IsTiramisuEditorDecalVisible(Decal, Camera))
	{
		return Fail("Far clip-space decal was not culled");
	}

	Decal.LocalToWorld[14] = 0.0f;
	Decal.LocalToWorld[12] =
		std::numeric_limits<float>::quiet_NaN();
	if (!IsTiramisuEditorDecalVisible(Decal, Camera))
	{
		return Fail("Non-finite decal must be handled conservatively");
	}
	return 0;
}
