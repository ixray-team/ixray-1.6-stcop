#include "stdafx.h"
#include "puddle.h"

CPuddle::CPuddle(LPVOID data, LPCSTR name):
	CEditShape(data,name)
{
	Construct(data);
	FScale.set(1, 0.3f, 1);
	m_RT_Flags.set(flRT_Visible, true);
}

void CPuddle::Construct(LPVOID data)
{
	CEditShape::Construct(data);

	FClassID = OBJCLASS_PUDDLES;
	m_shape_type = CShapeData::cfBox;

	shape_def& Shape = shapes.emplace_back();
	Shape.type = CShapeData::cfBox;
	Shape.data.box = FTransform;

	m_DrawTranspColor = color_rgba(20, 20, 187, 150);

}

CPuddle::~CPuddle()
{
}

bool CPuddle::LoadLTX(CInifile& ini, LPCSTR sect_name)
{
	bool Result = CEditShape::LoadLTX(ini, sect_name);

	OnUpdateTransform();
	return Result;
}

void CPuddle::SaveLTX(CInifile& ini, LPCSTR sect_name)
{
	CEditShape::SaveLTX(ini, sect_name);
}

void CPuddle::OnUpdateTransform()
{
	CEditShape::OnUpdateTransform();
	ComputeBounds();
}

bool CPuddle::LoadStream(IReader& F)
{
	return CEditShape::LoadStream(F);
}

void CPuddle::SaveStream(IWriter& F)
{
	CEditShape::SaveStream(F);
}

void CPuddle::OnFrame()
{
	CCustomObject::OnFrame();
}