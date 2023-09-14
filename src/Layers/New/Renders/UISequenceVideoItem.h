#pragma once
#include "../../Include/xrRender/UISequenceVideoItem.h"

namespace Rendering
{
	class UISequenceVideoItem : public IUISequenceVideoItem
	{
	public:
		~UISequenceVideoItem() override;
		void Copy(IUISequenceVideoItem& _in) override;

		bool HasTexture() override;
		void CaptureTexture() override;
		void ResetTexture() override;
		BOOL video_IsPlaying() override;
		void video_Sync(u32 _time) override;
		void video_Play(BOOL looped, u32 _time = 0xFFFFFFFF) override;
		void video_Stop() override;
	};
}