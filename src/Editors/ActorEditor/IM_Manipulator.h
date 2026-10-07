#pragma once

class CBone;

class IM_Manipulator
{
public:
	bool m_active = false;
	u32 MatrixMode = 0;

	void Render(float canvasX, float canvasY, float canvasWidth, float canvasHeight);

private:
	CBone* GetShapeBone() const;
	void BuildShapeMatrix(CBone* Bone, Fmatrix& Result, bool WithScale) const;
	void ApplyTransform(CBone* Bone, const Fmatrix& BoneMatrix, const Fmatrix& ShapeMatrix);

	void CommandMove(CBone* Bone, const Fmatrix& BoneMatrix);
	void CommandRotate(CBone* Bone, const Fmatrix& BoneMatrix);
	void CommandScale(CBone* Bone, const Fmatrix& BoneMatrix);

	void UpdateState();
};

extern IM_Manipulator imManipulator;
