#pragma once
#ifdef DEBUG
class CDS0_ObjectSpaceRender:public IObjectSpaceRender
{
public:
	CDS0_ObjectSpaceRender();
	virtual void Copy(IObjectSpaceRender &_in) override;;

	virtual void dbgRender() override;;
	virtual void dbgAddSphere(const Fsphere &sphere, u32 colour) override;;
	virtual void SetShader() override;;
};
#endif