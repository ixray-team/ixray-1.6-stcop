#pragma once
class CEffect_Rain;

class IRainRender
{
public:
	virtual ~IRainRender() {;}
	virtual void Copy(IRainRender &_in) = 0;
	virtual void Render(CEffect_Rain &owner) = 0;
	virtual const Fsphere& GetDropBounds() const = 0;
};
