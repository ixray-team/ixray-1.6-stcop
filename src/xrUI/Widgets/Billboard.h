#pragma once

//#include "UIWindow.h"
#include "UIStatic.h"

/*
	TODO Emmis:
	��� ���� ���������� �������� �����: �������� ����� Canvas, � ������� ����� ���������
	UI, �������� ����� TargetTexture ��� ��� �����-�� ����������� ��� ��������. ������� ����� � 3� ����������� ����
	� ������� ���� Canvas ������ ����� � ������. 
	����� ������� �� ���� �������� ������ override ������� UI. �� ������ ����� ���������� �� �� �����, � � Canvas

	��� ������ ��������� �������� ����� ��� ������� ������ ��� ���������, �� � � ��� ��������� � �������� ���������
*/
class UI_API CBillboard : public CUIStatic
{
public:
	CBillboard(const char* xml_node);
	~CBillboard();

	void DrawBillboard(const Fvector& world_pos);

	void DrawTexture() override;
	void DrawWidgetText() override;

private:
	typedef CUIStatic base;

	Fvector m_currentWorldPosition;

	Fvector3 m_offset;
};