#pragma once

// Mini viewport of the Thumbnail View and the Image Editor: a sphere, box, cylinder or plane covered
// with the texture. Reads the edited STextureParams every frame, so bump, detail, parallax and material
// changes show up before the thm is saved. Rendered to its own target on the primary thread.
class ECORE_API CMaterialPreview
{
public:
	enum class EShape : u8
	{
		Sphere,
		Box,
		Cylinder,
		Plane,
		Count
	};

	struct Vertex
	{
		Fvector P;
		Fvector N;
		Fvector T;
		Fvector B;
		Fvector2 UV;
	};

public:
	CMaterialPreview() = default;
	~CMaterialPreview();

	CMaterialPreview(const CMaterialPreview&) = delete;
	CMaterialPreview& operator=(const CMaterialPreview&) = delete;

	// TextureName is relative to $game_textures$, without extension
	void SetTexture(const xr_string& TextureName);
	// Full path of a thm under $game_textures$ to the texture name
	static xr_string GetTextureName(const xr_string& ThmFile);
	void Draw(const STextureParams& Params, const ImVec2& Size);
	void Clear();

	EShape GetShape() const { return Shape; }
	void SetShape(EShape Value) { Shape = Value; }
	// Off when the owner draws its own shape switcher
	bool ShowShapeSelector = true;

private:
	void BuildMesh();
	void UpdateShader(const STextureParams& Params);
	bool Render(const STextureParams& Params, u32 Width, u32 Height);
	void DrawToolbar();

	static void BuildSphere(xr_vector<Vertex>& Vertices, xr_vector<u16>& Indices);
	static void BuildBox(xr_vector<Vertex>& Vertices, xr_vector<u16>& Indices);
	static void BuildCylinder(xr_vector<Vertex>& Vertices, xr_vector<u16>& Indices);
	static void BuildPlane(xr_vector<Vertex>& Vertices, xr_vector<u16>& Indices);

private:
	xr_string TextureName;

	EShape Shape = EShape::Sphere;
	EShape MeshShape = EShape::Count;
	IRHIBuffer* VertexBuffer = nullptr;
	IRHIBuffer* IndexBuffer = nullptr;
	ref_geom Geometry;
	u32 VertexCount = 0;
	u32 IndexCount = 0;

	ref_shader PreviewShader;
	xr_string ShaderKey;
	bool UseBump = false;
	bool UseDetail = false;
	bool UseDetailBump = false;

	ref_rt TargetColor;
	ref_rt TargetDepth;

	// Orbit camera around the origin, the light follows it unless detached
	float Yaw = 0.6f;
	float Pitch = 0.35f;
	float Distance = 2.6f;
	float LightYaw = -0.8f;
	float LightPitch = 0.7f;
	float Ambient = 0.35f;
	float UVTiling = 1.f;
	bool AutoRotate = false;
	bool AlphaTest = false;
};
