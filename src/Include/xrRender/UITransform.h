#pragma once
#include <algorithm>
#include <cmath>

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

class IUIShader;

// Screen-space light; radius is measured in physical pixels at composition time.
struct UIFlashlight
{
	bool Enabled = false, IsolatedLayer = false;
	float X = 512.0f, Y = 384.0f, Radius = 300.0f, Softness = 0.8f;
	float Darkness = 0.4f, Saturation = 0.9f, Multiply = 0.65f;
	bool MaskEnabled = false;
	IUIShader* MaskShader = nullptr;
	float MaskX = 0.0f, MaskY = 0.0f, MaskWidth = 1.0f, MaskHeight = 1.0f;
	bool VolumeEnabled = false, DustEnabled = false;
	float SourceX = 1100.0f, SourceY = -100.0f, SourceDistancePixels = 650.0f, VolumeDensity = 0.08f;
	float VolumeNearClipPixels = 0.0f;
	int VolumeSteps = 24, DustCount = 48;
	float VolumeRed = 1.0f, VolumeGreen = 1.0f, VolumeBlue = 1.0f;
	float DustRed = 1.0f, DustGreen = 1.0f, DustBlue = 1.0f;
	float DustSize = 0.8f, DustSpeed = 0.6f, DustOpacity = 0.16f;
	bool DustAnimationEnabled = false;
	float DustMotionPhase = 0.0f, DustGlintPhase = 0.0f, DustGlintIntensity = 1.0f;
	bool InterferenceEnabled = false;
	float InterferenceStrength = 0.0f;
};

// Coordinates are in control units; Transform maps them into the current UI draw space.
struct UIRoundedRect
{
	UIFlashlight Flashlight;
	UITransform2D Transform;
	float Width = 0.0f, Height = 0.0f;
	float Radius = 0.0f, Thickness = 0.0f;
	u32 Color = 0;
	bool ScreenSpace = true;

	bool Contains(float X, float Y) const
	{
		if (Width <= 0.0f || Height <= 0.0f)
		{
			return false;
		}
		const float Det = Transform.XX * Transform.YY - Transform.XY * Transform.YX;
		if (!std::isfinite(Det) || std::abs(Det) < 1e-12f)
		{
			return false;
		}
		Transform.ApplyInverse(X, Y);
		const float HalfX = Width * 0.5f, HalfY = Height * 0.5f;
		const float QX = std::abs(X - HalfX) - (HalfX - Radius);
		const float QY = std::abs(Y - HalfY) - (HalfY - Radius);
		const float DX = std::max(QX, 0.0f), DY = std::max(QY, 0.0f);
		return std::sqrt(DX * DX + DY * DY) + std::min(std::max(QX, QY), 0.0f) <= Radius;
	}
};
