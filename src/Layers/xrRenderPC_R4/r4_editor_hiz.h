#pragma once

// Occlusion culling of the editors (no HOM, sectors or portals): the scene depth of a frame is
// reduced on the GPU to a grid of farthest depths, read back without stalling one or two frames
// later, and boxes are tested against it on the CPU with the camera of that frame.

class CEditorHiZ
{
public:
	CEditorHiZ() = default;
	CEditorHiZ(const CEditorHiZ&) = delete;
	CEditorHiZ& operator=(const CEditorHiZ&) = delete;
	~CEditorHiZ();

	void Destroy();
	void Build();
	void Fetch();
	bool IsVisible(const Fbox& Box) const;

	bool IsUsable() const
	{
		return Usable;
	}

private:
	struct SReadback
	{
		ID3D11Texture2D* Texture = nullptr;
		Fmatrix ViewProject;
		Fvector CameraPosition;
		Fvector CameraDirection;
		bool Pending = false;
	};

	bool CreateResources(u32 Width, u32 Height);
	void BuildMips();

	static constexpr u32 GridWidth = 256;
	static constexpr u32 ReadbackCount = 3;

	ID3D11Texture2D* GridTexture = nullptr;
	ID3D11UnorderedAccessView* GridUAV = nullptr;
	SReadback Readbacks[ReadbackCount];
	u32 NextReadback = 0;

	u32 ViewWidth = 0;
	u32 ViewHeight = 0;
	u32 GridHeight = 0;

	IBlender* Blender = nullptr;
	ref_shader Shader;

	struct SLevel
	{
		u32 Width = 0;
		u32 Height = 0;
		xr_vector<float> Depth;
	};
	xr_vector<SLevel> Levels;

	bool HasData = false;
	bool Usable = false;
	Fmatrix ViewProject;
	Fvector CameraPosition;
	Fvector CameraDirection;
};
