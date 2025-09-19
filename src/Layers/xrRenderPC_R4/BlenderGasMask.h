#pragma once

class CBlenderGasMask :
	public IBlender
{
public:
	CBlenderGasMask();
	virtual ~CBlenderGasMask() = default;

	virtual LPCSTR getComment() { return "GasMask effect"; }
	virtual BOOL canBeDetailed() { return FALSE; }
	virtual BOOL canBeLMAPped() { return FALSE; }

	virtual void Compile(CBlender_Compile& C);

};