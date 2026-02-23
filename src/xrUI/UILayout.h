#pragma once

class CUIWindow;

enum class EUILayoutType
{
	None,
	Stack,
	Grid
};

class UI_API ILayoutProvider
{
public:
	virtual ~ILayoutProvider() = default;
	virtual void LayoutChildren(CUIWindow* pParent) = 0;
	virtual EUILayoutType GetLayoutType() const { return EUILayoutType::None; }
};

enum class EUIStackLayoutDir
{
	Horizontal,
	Vertical
};

class UI_API CUIStackLayout final : public ILayoutProvider
{
public:
	CUIStackLayout(EUIStackLayoutDir direction, float spacing = 0.0f,
		float paddingLeft = 0.0f, float paddingTop = 0.0f,
		float paddingRight = 0.0f, float paddingBottom = 0.0f,
		bool reverse = false);

	void LayoutChildren(CUIWindow* pParent) override;
	EUILayoutType GetLayoutType() const override { return EUILayoutType::Stack; }

	void SetSpacing(float spacing) { m_spacing = spacing; }
	void SetPadding(float left, float top, float right, float bottom);
	void SetReverse(bool reverse) { m_reverse = reverse; }

	float GetSpacing() const { return m_spacing; }
	float GetPaddingLeft() const { return m_paddingLeft; }
	float GetPaddingTop() const { return m_paddingTop; }
	float GetPaddingRight() const { return m_paddingRight; }
	float GetPaddingBottom() const { return m_paddingBottom; }
	EUIStackLayoutDir GetDirection() const { return m_direction; }
	bool GetReverse() const { return m_reverse; }

private:
	EUIStackLayoutDir m_direction;
	float m_spacing;
	float m_paddingLeft;
	float m_paddingTop;
	float m_paddingRight;
	float m_paddingBottom;
	bool m_reverse;
};

class UI_API CUIGridLayout final : public ILayoutProvider
{
public:
	CUIGridLayout(int cols, int rows = 0,
		float cellSpacingX = 0.0f, float cellSpacingY = 0.0f,
		float cellWidth = 0.0f, float cellHeight = 0.0f,
		float paddingLeft = 0.0f, float paddingTop = 0.0f,
		float paddingRight = 0.0f, float paddingBottom = 0.0f);

	void LayoutChildren(CUIWindow* pParent) override;
	EUILayoutType GetLayoutType() const override { return EUILayoutType::Grid; }

	void SetCols(int cols) { m_cols = cols; }
	void SetRows(int rows) { m_rows = rows; }
	void SetCellSpacing(float x, float y) { m_cellSpacingX = x; m_cellSpacingY = y; }
	void SetCellSize(float width, float height) { m_cellWidth = width; m_cellHeight = height; }

	int GetCols() const { return m_cols; }
	int GetRows() const { return m_rows; }
	float GetCellSpacingX() const { return m_cellSpacingX; }
	float GetCellSpacingY() const { return m_cellSpacingY; }
	float GetCellWidth() const { return m_cellWidth; }
	float GetCellHeight() const { return m_cellHeight; }
	float GetPaddingLeft() const { return m_paddingLeft; }
	float GetPaddingTop() const { return m_paddingTop; }
	float GetPaddingRight() const { return m_paddingRight; }
	float GetPaddingBottom() const { return m_paddingBottom; }

private:
	int m_cols;
	int m_rows;
	float m_cellSpacingX;
	float m_cellSpacingY;
	float m_cellWidth;
	float m_cellHeight;
	float m_paddingLeft;
	float m_paddingTop;
	float m_paddingRight;
	float m_paddingBottom;
};
