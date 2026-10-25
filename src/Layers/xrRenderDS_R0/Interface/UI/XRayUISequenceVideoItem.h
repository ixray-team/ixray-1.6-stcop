#pragma once
class CDS0_UISequenceVideoItem:public IUISequenceVideoItem
{
public:
	CDS0_UISequenceVideoItem();
	virtual void Copy(IUISequenceVideoItem &_in) override;

	virtual bool HasTexture() override;
	virtual void CaptureTexture() override;
	virtual void ResetTexture() override;
	virtual bool video_IsPlaying() override;
	virtual void video_Sync(u32 _time) override;;
	virtual void video_Play(bool looped, u32 _time = 0xFFFFFFFF) override;;
	virtual void video_Stop() override;
};
