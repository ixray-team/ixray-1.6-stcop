#pragma once

struct UITransform2D
{
	float XX = 1.0f, XY = 0.0f, YX = 0.0f, YY = 1.0f;
	float TX = 0.0f, TY = 0.0f;

	bool IsIdentity() const
	{
		return XX == 1.0f && XY == 0.0f && YX == 0.0f && YY == 1.0f && TX == 0.0f && TY == 0.0f;
	}

	void Apply(float& X, float& Y) const
	{
		const float OldX = X;
		X = XX * OldX + XY * Y + TX;
		Y = YX * OldX + YY * Y + TY;
	}

	void ApplyInverse(float& X, float& Y) const
	{
		const float OldX = X - TX;
		const float OldY = Y - TY;
		const float Det = XX * YY - XY * YX;
		X = (YY * OldX - XY * OldY) / Det;
		Y = (XX * OldY - YX * OldX) / Det;
	}

	UITransform2D Inverted() const
	{
		const float Det = XX * YY - XY * YX;
		UITransform2D Result;
		Result.XX = YY / Det;
		Result.XY = -XY / Det;
		Result.YX = -YX / Det;
		Result.YY = XX / Det;
		Result.TX = -(Result.XX * TX + Result.XY * TY);
		Result.TY = -(Result.YX * TX + Result.YY * TY);
		return Result;
	}

	UITransform2D Compose(const UITransform2D& Local) const
	{
		UITransform2D Result;
		Result.XX = XX * Local.XX + XY * Local.YX;
		Result.XY = XX * Local.XY + XY * Local.YY;
		Result.YX = YX * Local.XX + YY * Local.YX;
		Result.YY = YX * Local.XY + YY * Local.YY;
		Result.TX = XX * Local.TX + XY * Local.TY + TX;
		Result.TY = YX * Local.TX + YY * Local.TY + TY;
		return Result;
	}
};
