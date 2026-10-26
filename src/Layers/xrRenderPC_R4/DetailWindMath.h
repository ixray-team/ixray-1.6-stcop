#pragma once

#include <algorithm>
#include <cmath>
#include <cstdint>

// Portable numerical core for the volume field and inertial response.
namespace DetailWindMath
{
struct Vec3
{
	float X = 0, Y = 0, Z = 0;
	Vec3 operator+(const Vec3& B) const { return {X + B.X, Y + B.Y, Z + B.Z}; }
	Vec3 operator-(const Vec3& B) const { return {X - B.X, Y - B.Y, Z - B.Z}; }
	Vec3 operator*(float S) const { return {X * S, Y * S, Z * S}; }
};

inline float Smooth(float X)
{
	X = std::clamp(X, 0.0f, 1.0f);
	return X * X * X * (X * (6 * X - 15) + 10);
}
inline std::uint64_t Hash(std::uint64_t X)
{
	X ^= X >> 30;
	X *= 0xbf58476d1ce4e5b9ULL;
	X ^= X >> 27;
	X *= 0x94d049bb133111ebULL;
	return X ^ (X >> 31);
}
inline float Random(std::uint64_t& State)
{
	State += 0x9e3779b97f4a7c15ULL;
	return float(Hash(State) >> 40) * (1.0f / 16777216.0f);
}

struct NoiseSample
{
	float Value = 0;
	Vec3 Gradient;
};

// Four-dimensional simplex gradient noise: XYZ is space, W evolves the field.
// Integer hashing has no small permutation table or tiled texture period.
inline NoiseSample Noise(double X, double Y, double Z, double W, std::uint64_t Seed)
{
	constexpr double F = 0.30901699437494745;
	constexpr double G = 0.13819660112501052;
	double P[4] = {X, Y, Z, W};
	std::int64_t Cell[4];
	const double Skew = (X + Y + Z + W) * F;
	double Sum = 0;
	for (int A = 0; A < 4; ++A)
	{
		Cell[A] = static_cast<std::int64_t>(std::floor(P[A] + Skew));
		Sum += double(Cell[A]);
	}
	float Delta[4];
	int Rank[4] = {};
	for (int A = 0; A < 4; ++A)
	{
		Delta[A] = float(P[A] - (double(Cell[A]) - Sum * G));
	}
	for (int A = 0; A < 4; ++A)
	{
		for (int B = A + 1; B < 4; ++B)
		{
			++Rank[Delta[A] > Delta[B] ? A : B];
		}
	}
	NoiseSample Result;
	for (int Corner = 0; Corner < 5; ++Corner)
	{
		float D[4];
		std::uint64_t H = Seed;
		// A compact 0.5 support avoids derivative seams at simplex boundaries.
		float Radius = 0.5f;
		for (int A = 0; A < 4; ++A)
		{
			const int Step = Corner == 0 ? 0 : (Rank[A] >= 4 - Corner ? 1 : 0);
			D[A] = Delta[A] - float(Step) + float(Corner * G);
			Radius -= D[A] * D[A];
			H = Hash(H ^ Hash(std::uint64_t(Cell[A] + Step) + std::uint64_t(A + 1) * 0x9e3779b97f4a7c15ULL));
		}
		if (Radius <= 0)
		{
			continue;
		}
		float Gradient[4] = {};
		const int ZeroAxis = int((H >> 3) & 3);
		int Bit = 0;
		float Dot = 0;
		for (int A = 0; A < 4; ++A)
		{
			if (A != ZeroAxis)
			{
				Gradient[A] = (H & (1ULL << Bit++)) ? 1.0f : -1.0f;
			}
			Dot += Gradient[A] * D[A];
		}
		const float R3 = Radius * Radius * Radius;
		const float R4 = R3 * Radius;
		Result.Value += R4 * Dot;
		Result.Gradient.X += R4 * Gradient[0] - 8 * R3 * Dot * D[0];
		Result.Gradient.Y += R4 * Gradient[1] - 8 * R3 * Dot * D[1];
		Result.Gradient.Z += R4 * Gradient[2] - 8 * R3 * Dot * D[2];
	}
	Result.Value *= 60;
	Result.Gradient = Result.Gradient * 60;
	return Result;
}

// Exact transition matrix for a constant target over one time step.
// Shared coefficients avoid transcendental work for every plant and axis.
struct SpringMatrix
{
	float XX = 1, XV = 0, VX = 0, VV = 1;
};

inline SpringMatrix SpringTransition(float Frequency, float Damping, float Dt)
{
	if (Dt <= 0)
	{
		return {};
	}
	const double Omega = std::max(.01f, Frequency) * 6.283185307179586;
	const double Zeta = std::max(.01f, Damping), A = Zeta * Omega;
	if (Zeta < .999)
	{
		const double B = Omega * std::sqrt(1 - Zeta * Zeta), E = std::exp(-A * Dt);
		const double C = std::cos(B * Dt), S = std::sin(B * Dt) / B;
		return {float(E * (C + A * S)), float(E * S), float(-E * Omega * Omega * S), float(E * (C - A * S))};
	}
	if (Zeta <= 1.001)
	{
		const double E = std::exp(-Omega * Dt);
		return {float(E * (1 + Omega * Dt)), float(E * Dt), float(-E * Omega * Omega * Dt), float(E * (1 - Omega * Dt))};
	}
	const double B = Omega * std::sqrt(Zeta * Zeta - 1), R1 = -A + B, R2 = -A - B;
	const double E1 = std::exp(R1 * Dt), E2 = std::exp(R2 * Dt), Inv = 1 / (R1 - R2);
	return {float((-R2 * E1 + R1 * E2) * Inv), float((E1 - E2) * Inv), float(-Omega * Omega * (E1 - E2) * Inv), float((R1 * E1 - R2 * E2) * Inv)};
}

inline void Spring(float& Position, float& Velocity, float Target, const SpringMatrix& Matrix)
{
	const float X = Position - Target, V = Velocity;
	Position = Target + Matrix.XX * X + Matrix.XV * V;
	Velocity = Matrix.VX * X + Matrix.VV * V;
}


inline Vec3 LimitBendModes(Vec3 Modes, float Limit)
{
	const float Total = std::sqrt(Modes.X * Modes.X + Modes.Y * Modes.Y) + std::abs(Modes.Z);
	const float Denominator = std::sqrt(Limit * Limit + Total * Total);
	return Denominator > .000001f ? Modes * (Limit / Denominator) : Vec3{};
}
} // namespace DetailWindMath
