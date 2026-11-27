#pragma once

namespace XRay::Engine
{
	void PreRenderThread();
	void GameThread();
	void GameThreadBody();
	void GCThread();
}