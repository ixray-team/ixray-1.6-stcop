#pragma once
class CDS0_RainRender:
	public IRainRender
{
public:
	CDS0_RainRender();
	virtual void Copy(IRainRender &_in) ;

	virtual void Render(CEffect_Rain&owner) ;

	virtual const Fsphere& GetDropBounds() const;
};