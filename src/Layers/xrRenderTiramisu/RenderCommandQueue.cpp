#include "stdafx.h"
#include "RenderCommandQueue.h"

namespace Tiramisu::RenderCommands
{
	void ExecuteRenderCommands()
	{
		CheckIsRenderThread();
		GetRenderCommandQueue().Execute();
	}
	
	void FlushRenderCommands()
	{
		CheckIsGameThread();
		if (IsRenderThreadRunning())
		{
			std::promise<void> done;
			auto future = done.get_future();
	
			ENQUEUE_RENDER_COMMAND(FlushRenderCommands)
			(
				[&done]
				{
					CheckIsRenderThread();
					done.set_value();
				}
			);
	
			GRender->SubmitFrame();
			future.wait();
		}
		else
		{
			if (GRender)
			{
				GRender->WaitGPU_RenderThread();
			}
			ExecuteRenderCommands();
		}
	}
} // namespace Tiramisu::RenderCommands
