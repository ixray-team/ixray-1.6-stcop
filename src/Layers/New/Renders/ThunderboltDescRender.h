#pragma once
#include "../../Include/xrRender/ThunderboltDescRender.h"

namespace Rendering
{
	class ThunderboltDescRender : public IThunderboltDescRender
	{
	public:
		~ThunderboltDescRender();

		void Copy(IThunderboltDescRender& _in) override;
		void CreateModel(LPCSTR m_name) override;
		void DestroyModel() override;
	};
}