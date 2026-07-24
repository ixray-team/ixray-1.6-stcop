#include "../xrEUI/stdafx.h"

#include <cstdlib>
#include <iostream>

namespace
{
class FTestUIManager final : public XrUIManager
{
public:
	bool ApplyShortCut(DWORD, TShiftState) override
	{
		return false;
	}
};

class FTestRenderBackend final : public IXrUIRendererBackend
{
public:
	[[nodiscard]] EXrUIRendererPlatform GetPlatform() const noexcept override
	{
		return EXrUIRendererPlatform::Vulkan;
	}

	[[nodiscard]] bool SupportsPlatformViewports() const noexcept override
	{
		return false;
	}

	[[nodiscard]] bool OwnsMainPresentation() const noexcept override
	{
		return true;
	}

	[[nodiscard]] bool Initialize() override
	{
		++InitializeCount;
		return true;
	}

	void Shutdown() override { ++ShutdownCount; }
	void BeginFrame() override { ++BeginFrameCount; }
	void RenderDrawData(ImDrawData&) override { ++RenderCount; }
	void InvalidateDeviceObjects() override { ++InvalidateCount; }
	void CreateDeviceObjects() override { ++CreateCount; }

	int InitializeCount = 0;
	int ShutdownCount = 0;
	int BeginFrameCount = 0;
	int RenderCount = 0;
	int InvalidateCount = 0;
	int CreateCount = 0;
};

int Fail(const char* Message)
{
	std::cerr << Message << '\n';
	return EXIT_FAILURE;
}
} // namespace

int main()
{
	FTestUIManager Manager;
	FTestRenderBackend First;
	FTestRenderBackend Second;

	if (!Manager.InstallRenderBackend(&First))
	{
		return Fail("A renderer backend could not be installed before initialization");
	}
	if (Manager.GetRenderBackend() != &First)
	{
		return Fail("The installed renderer backend was not published");
	}
	if (!Manager.InstallRenderBackend(&Second))
	{
		return Fail("A renderer backend could not be replaced before initialization");
	}
	if (Manager.GetRenderBackend() != &Second)
	{
		return Fail("The replacement renderer backend was not published");
	}
	if (!Manager.UsesExternalMainPresentation())
	{
		return Fail("The manager did not expose external main presentation ownership");
	}
	Manager.PresentMainFrame();
	if (Second.RenderCount != 0)
	{
		return Fail("Presenting without a completed ImGui frame reached the backend");
	}
	if (!Manager.InstallRenderBackend(nullptr))
	{
		return Fail("The renderer backend could not be reset to the DX9 fallback");
	}
	if (Manager.GetRenderBackend() != nullptr)
	{
		return Fail("Resetting the renderer backend did not select lazy DX9 creation");
	}
	if (Manager.UsesExternalMainPresentation())
	{
		return Fail("The lazy DX9 fallback was reported as an external presenter");
	}

	return EXIT_SUCCESS;
}
