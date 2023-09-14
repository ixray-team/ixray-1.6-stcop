#pragma once
#include "../../Include/xrRender/StatGraphRender.h"

namespace Rendering
{
	class StatGraphRender : public IStatGraphRender
	{
	public:
		~StatGraphRender();
		void Copy(IStatGraphRender& _in) override;

		void OnDeviceCreate() override;
		void OnDeviceDestroy() override;

		void OnRender(CStatGraph& owner) override;
	};
}