#pragma once
#ifdef DEBUG
class CDS0_ObjectSpaceRender:public IObjectSpaceRender
{
public:
	CDS0_ObjectSpaceRender();
	virtual void Copy(IObjectSpaceRender &_in);;

	virtual void dbgRender();;
	virtual void dbgAddSphere(const Fsphere &sphere, u32 colour);;
	virtual void SetShader();;
};
#endif