#pragma once
#include "Editor/Spline/Spline.h"

// Spline-driven road: a mesh strip (road + optional shoulders) generated along a spline.
// Control points are stored in the object's local space, so the whole road can be moved/rotated.
class CRoad :
	public CCustomObject
{
	typedef CCustomObject inherited;
	friend class ESceneRoadTool;
	friend class TUI_ControlRoadEdit;

public:
	struct SSurface
	{
		shared_str Shader;
		shared_str ShaderXRLC;
		shared_str GameMtl;
		shared_str Texture;
	};

	XRay::Editor::Spline::CSpline Spline;

	float SampleStep = 2.f;        // Spline sampling along the length (m)
	int CrossSegments = 4;         // Subdivisions across the road (more = better draping)
	float ShoulderWidth = 1.5f;    // Width of each shoulder strip (m), 0 = none
	float HeightOffset = 0.05f;    // Lift above the terrain to avoid z-fighting
	float UVLength = 8.f;          // Road: meters per texture repeat along the road
	float RoadTileWidth = 0.f;     // Road: meters per repeat across; 0 = stretch once over the width
	float ShoulderUVLength = 8.f;  // Shoulder: meters per repeat along
	float ShoulderTileWidth = 0.f; // Shoulder: meters per repeat across; 0 = stretch once over the shoulder
	float ConformFalloff = 6.f;    // Terrain blend distance beyond the shoulders
	bool FollowTerrain = true;     // Drape vertices over the terrain instead of the spline height

	// Terrain materials (by name) the road surface / shoulders assign to the terrain under them; empty = none
	shared_str MaskRoadMaterial = "road";
	shared_str MaskShoulderMaterial = "shoulder";

	SSurface RoadSurface;
	SSurface ShoulderSurface;

	xr_vector<u32> SelectedPoints; // Editor state: selected control points (sorted)

	// Point mode: Road target + Edit Points sub-target. Standard Select / Move / Delete act on control points.
	static bool IsPointMode();
	Fmatrix GetPointsTransform();   // Gizmo placement in point mode

	virtual void Select(int flag) override;
	virtual bool RaySelect(int flag, const Fvector& start, const Fvector& dir, bool bRayTest = false) override;
	virtual bool FrustumSelect(int flag, const CFrustum& frustum) override;
	virtual void Move(Fvector& amount) override;
	virtual void RotateParent(Fvector& axis, float angle) override;
	virtual void RotateLocal(Fvector& axis, float angle) override;
	virtual void Scale(Fvector& amount) override;
	virtual bool OnSelectionRemove() override;
	virtual void OnFrame() override;

	bool IsPointSelected(u32 Index) const;
	void SelectPoint(u32 Index, bool Add);
	void TogglePoint(u32 Index);
	void ClearPointSelection() { SelectedPoints.clear(); }
	void DeleteSelectedPoints();

public:
	CRoad(LPVOID data, const char* name);
	virtual ~CRoad();

	virtual bool CanAttach() override { return false; }
	virtual bool LoadStream(IReader&) override;
	virtual void SaveStream(IWriter&) override;

	void RebuildMesh();
	float GetLength() const;

	// World <-> local helpers for control points
	Fvector PointWorld(u32 Index);
	void SetPointWorld(u32 Index, const Fvector& World);
	u32 InsertPointWorld(u32 Index, const Fvector& World, float Width);

	virtual bool RayPick(float& dist, const Fvector& S, const Fvector& D, SRayPickInfo* pinf) override;
	virtual void Render(int priority, bool strictB2F) override;
	virtual void OnUpdateTransform() override;
	virtual void OnDeviceDestroy() override;
	virtual void FillProp(const char* pref, PropItemVec& items) override;

	virtual void BoxQuery(SPickQuery& pinf) override;
	virtual void RayQuery(SPickQuery& pinf) override;
	virtual bool GetBox(Fbox& box) override;
	virtual bool GetUTBox(Fbox& box) override;

	bool BoxPick(const Fbox& box, SBoxPickInfoVec& pinf);
	bool HasMesh() const;
	IC CEditableObject* GetReference() { return RoadObject; }

	CEditableObject* RoadObject;

	// World height of the terrain under a world XZ position
	static bool SampleTerrainHeight(float X, float Z, float& Y);

private:
	Fbox m_TBBox;
	bool m_MeshDirty = false;      // Points moved by the gizmo: rebuild when the drag ends

	int PickPointRay(const Fvector& Start, const Fvector& Dir, float* OutT = nullptr);
	void SetPointSelected(u32 Index, int Flag);

	void RenderSpline();
	void OnChangeMeshProp(PropValue* sender);

	// Property buffers for the selected points
	float EditWidth = 6.f;
	float EditBank = 0.f;
	float EditHeight = 0.f;
	Fvector EditPos = {0, 0, 0};
	void FillPointProps(const char* pref, PropItemVec& items);
	void AfterPointEdit();
	void OnPointWidthChange(PropValue* sender);
	void OnPointBankChange(PropValue* sender);
	void OnPointHeightChange(PropValue* sender);
	void OnPointPosChange(PropValue* sender);
	void OnDeletePointsClick(ButtonValue* sender, bool& bModif, bool& bSafe);
};
