#pragma once

// Generic editor spline (Catmull-Rom through control points).
// Shared by roads and any other spline-driven objects (rivers, fences, ...).
namespace XRay::Editor::Spline
{
	struct SSplinePoint
	{
		Fvector Pos = {0, 0, 0}; // Local space of the owner object
		float Width = 6.f;       // Per-point width (meaning is up to the owner)
		float Roll = 0.f;        // Banking around the tangent, radians
	};

	struct SSplineSample
	{
		Fvector Pos;
		Fvector Tangent;  // Normalized
		Fvector Right;    // Normalized, with roll applied
		Fvector Up;       // Normalized, with roll applied
		float Width;
		float Distance;   // Arc length from the start
		float Param;      // Spline parameter: segment index + fraction
	};

	class CSpline
	{
	public:
		xr_vector<SSplinePoint> Points;
		bool Closed = false;

		u32 SegmentCount() const;
		bool IsValid() const { return Points.size() >= 2; }

		// t in [0, SegmentCount()]
		SSplinePoint Evaluate(float t) const;
		Fvector EvaluateTangent(float t) const;

		// Samples roughly every Step units of arc length (always includes both ends)
		void Sample(float Step, xr_vector<SSplineSample>& Out) const;

		// Index of the control point closest to the ray (within Radius), or -1
		int PickPoint(const Fvector& Start, const Fvector& Dir, float Radius) const;
		// Best segment to insert a new point near P (index of the point to insert before)
		u32 FindInsertIndex(const Fvector& P) const;

		void Save(IWriter& F) const;
		bool Load(IReader& F);

	private:
		const SSplinePoint& At(int i) const;
	};
}
