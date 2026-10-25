#include "stdafx.h"
#include "MaterialPreview.h"

static RHIInputElementDesc PreviewVertexDecl[] =
{
	{ "POSITION", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, 0, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "NORMAL", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, 12, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TANGENT", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, 24, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "BINORMAL", 0, ERHI_FORMAT::R32G32B32_FLOAT, 0, 36, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
	{ "TEXCOORD", 0, ERHI_FORMAT::R32G32_FLOAT, 0, 48, ERHI_INPUT_CLASSIFICATION::VERTEX_DATA, 0 },
};

static const char* ShapeNames[] = {"Sphere", "Box", "Cylinder", "Plane"};
static_assert(std::size(ShapeNames) == size_t(CMaterialPreview::EShape::Count));

static const float PreviewClearColor[4] = {0.16f, 0.16f, 0.17f, 1.0f};
static constexpr float PreviewFov = 40.0f;
static constexpr u32 SphereSlices = 64;
static constexpr u32 SphereStacks = 32;
static constexpr float CylinderRadius = 0.7f;

// D3D culls counter-clockwise triangles: keep the face normal of each triangle outward
static void AddTriangle(xr_vector<u16>& Indices, const xr_vector<CMaterialPreview::Vertex>& Vertices, u16 A, u16 B, u16 C)
{
	Fvector E1, E2, Normal;
	E1.sub(Vertices[B].P, Vertices[A].P);
	E2.sub(Vertices[C].P, Vertices[A].P);
	Normal.crossproduct(E1, E2);

	Fvector Outward = Vertices[A].N;
	Outward.add(Vertices[B].N).add(Vertices[C].N);

	Indices.push_back(A);
	if (Normal.dotproduct(Outward) >= 0.f)
	{
		Indices.push_back(B);
		Indices.push_back(C);
	}
	else
	{
		Indices.push_back(C);
		Indices.push_back(B);
	}
}

static void AddQuad(xr_vector<u16>& Indices, const xr_vector<CMaterialPreview::Vertex>& Vertices, u16 A, u16 B, u16 C, u16 D)
{
	AddTriangle(Indices, Vertices, A, B, C);
	AddTriangle(Indices, Vertices, A, C, D);
}

// Planar face: P = Center + U * x + V * y for x, y in [-1, 1], UV follows U and V
static void AddFace(xr_vector<CMaterialPreview::Vertex>& Vertices, xr_vector<u16>& Indices, const Fvector& Center, const Fvector& N, const Fvector& U, const Fvector& V, float Extent)
{
	const u16 Base = u16(Vertices.size());
	const float Corners[4][2] = {{0.f, 0.f}, {1.f, 0.f}, {1.f, 1.f}, {0.f, 1.f}};

	for (const auto& Corner : Corners)
	{
		CMaterialPreview::Vertex& Vert = Vertices.emplace_back();
		Vert.P.set(Center).mad(U, (Corner[0] * 2.f - 1.f) * Extent).mad(V, (Corner[1] * 2.f - 1.f) * Extent);
		Vert.N.set(N);
		Vert.T.set(U);
		Vert.B.set(V);
		Vert.UV.set(Corner[0], Corner[1]);
	}

	AddQuad(Indices, Vertices, Base, Base + 1, Base + 2, Base + 3);
}

void CMaterialPreview::BuildSphere(xr_vector<Vertex>& Vertices, xr_vector<u16>& Indices)
{
	for (u32 Stack = 0; Stack <= SphereStacks; ++Stack)
	{
		const float V = float(Stack) / float(SphereStacks);
		const float Theta = V * PI;

		for (u32 Slice = 0; Slice <= SphereSlices; ++Slice)
		{
			const float U = float(Slice) / float(SphereSlices);
			const float Phi = U * PI_MUL_2;

			Vertex& Vert = Vertices.emplace_back();
			Vert.N.set(std::sin(Theta) * std::cos(Phi), std::cos(Theta), std::sin(Theta) * std::sin(Phi));
			Vert.P.set(Vert.N);
			// dP/du and dP/dv, as the mesh exporters build them
			Vert.T.set(-std::sin(Phi), 0.f, std::cos(Phi));
			Vert.B.set(std::cos(Theta) * std::cos(Phi), -std::sin(Theta), std::cos(Theta) * std::sin(Phi)).normalize_safe();
			// The equator is twice as long as a meridian: keep the texels square
			Vert.UV.set(U * 2.f, V);
		}
	}

	const u32 Row = SphereSlices + 1;
	for (u32 Stack = 0; Stack < SphereStacks; ++Stack)
	{
		for (u32 Slice = 0; Slice < SphereSlices; ++Slice)
		{
			const u16 A = u16(Stack * Row + Slice);
			const u16 B = u16(A + 1);
			const u16 C = u16(A + Row + 1);
			const u16 D = u16(A + Row);

			if (Stack != 0)
			{
				AddTriangle(Indices, Vertices, A, B, C);
			}

			if (Stack != SphereStacks - 1)
			{
				AddTriangle(Indices, Vertices, A, C, D);
			}
		}
	}
}

void CMaterialPreview::BuildBox(xr_vector<Vertex>& Vertices, xr_vector<u16>& Indices)
{
	const float Extent = 0.75f;
	const Fvector Up = {0.f, 1.f, 0.f};

	// Sides: U is to the right of a viewer facing the side, V goes down
	for (const Fvector& N : {Fvector{0.f, 0.f, 1.f}, Fvector{1.f, 0.f, 0.f}, Fvector{0.f, 0.f, -1.f}, Fvector{-1.f, 0.f, 0.f}})
	{
		Fvector U, V;
		U.crossproduct(N, Up);
		V.crossproduct(N, U);
		AddFace(Vertices, Indices, Fvector().mul(N, Extent), N, U, V, Extent);
	}

	for (const Fvector& N : {Fvector{0.f, 1.f, 0.f}, Fvector{0.f, -1.f, 0.f}})
	{
		const Fvector U = {1.f, 0.f, 0.f};
		Fvector V;
		V.crossproduct(N, U);
		AddFace(Vertices, Indices, Fvector().mul(N, Extent), N, U, V, Extent);
	}
}

void CMaterialPreview::BuildCylinder(xr_vector<Vertex>& Vertices, xr_vector<u16>& Indices)
{
	const float HalfHeight = 0.9f;
	const u16 Base = u16(Vertices.size());

	for (u32 Ring = 0; Ring <= 1; ++Ring)
	{
		for (u32 Slice = 0; Slice <= SphereSlices; ++Slice)
		{
			const float U = float(Slice) / float(SphereSlices);
			const float Phi = U * PI_MUL_2;

			Vertex& Vert = Vertices.emplace_back();
			Vert.N.set(std::cos(Phi), 0.f, std::sin(Phi));
			Vert.P.set(Vert.N.x * CylinderRadius, Ring == 0 ? HalfHeight : -HalfHeight, Vert.N.z * CylinderRadius);
			Vert.T.set(-std::sin(Phi), 0.f, std::cos(Phi));
			Vert.B.set(0.f, -1.f, 0.f);
			// Circumference against the height, rounded to whole tiles to hide the seam
			Vert.UV.set(U * 2.f, float(Ring));
		}
	}

	const u16 Row = u16(SphereSlices + 1);
	for (u16 Slice = 0; Slice < SphereSlices; ++Slice)
	{
		const u16 A = u16(Base + Slice);
		AddQuad(Indices, Vertices, A, A + 1, A + Row + 1, A + Row);
	}

	for (float Sign : {1.f, -1.f})
	{
		const Fvector N = {0.f, Sign, 0.f};
		const Fvector T = {1.f, 0.f, 0.f};
		Fvector B;
		B.crossproduct(N, T);

		const u16 Center = u16(Vertices.size());
		Vertex& CenterVert = Vertices.emplace_back();
		CenterVert.P.set(0.f, HalfHeight * Sign, 0.f);
		CenterVert.N.set(N);
		CenterVert.T.set(T);
		CenterVert.B.set(B);
		CenterVert.UV.set(0.5f, 0.5f);

		for (u32 Slice = 0; Slice <= SphereSlices; ++Slice)
		{
			const float Phi = float(Slice) / float(SphereSlices) * PI_MUL_2;

			Vertex& Vert = Vertices.emplace_back();
			Vert.P.set(std::cos(Phi) * CylinderRadius, HalfHeight * Sign, std::sin(Phi) * CylinderRadius);
			Vert.N.set(N);
			Vert.T.set(T);
			Vert.B.set(B);
			Vert.UV.set(0.5f + Vert.P.dotproduct(T) * 0.5f / CylinderRadius, 0.5f + Vert.P.dotproduct(B) * 0.5f / CylinderRadius);
		}

		for (u16 Slice = 0; Slice < SphereSlices; ++Slice)
		{
			AddTriangle(Indices, Vertices, Center, u16(Center + 1 + Slice), u16(Center + 2 + Slice));
		}
	}
}

void CMaterialPreview::BuildPlane(xr_vector<Vertex>& Vertices, xr_vector<u16>& Indices)
{
	AddFace(Vertices, Indices, Fvector().set(0.f, 0.f, 0.f), Fvector().set(0.f, 1.f, 0.f), Fvector().set(1.f, 0.f, 0.f), Fvector().set(0.f, 0.f, -1.f), 1.f);
}

CMaterialPreview::~CMaterialPreview()
{
	Clear();
}

void CMaterialPreview::Clear()
{
	PreviewShader.destroy();
	ShaderKey.clear();
	Geometry.destroy();
	MeshShape = EShape::Count;

	if (VertexBuffer != nullptr)
	{
		VertexBuffer->Release();
		VertexBuffer = nullptr;
	}

	if (IndexBuffer != nullptr)
	{
		IndexBuffer->Release();
		IndexBuffer = nullptr;
	}

	TargetColor.destroy();
	TargetDepth.destroy();
}

void CMaterialPreview::SetTexture(const xr_string& Name)
{
	TextureName = Name;
	PreviewShader.destroy();
	ShaderKey.clear();
}

void CMaterialPreview::BuildMesh()
{
	if (MeshShape == Shape && Geometry)
	{
		return;
	}

	Geometry.destroy();
	if (VertexBuffer != nullptr)
	{
		VertexBuffer->Release();
		VertexBuffer = nullptr;
	}

	if (IndexBuffer != nullptr)
	{
		IndexBuffer->Release();
		IndexBuffer = nullptr;
	}

	xr_vector<Vertex> Vertices;
	xr_vector<u16> Indices;

	switch (Shape)
	{
	case EShape::Sphere:	BuildSphere(Vertices, Indices); break;
	case EShape::Box:		BuildBox(Vertices, Indices); break;
	case EShape::Cylinder:	BuildCylinder(Vertices, Indices); break;
	default:				BuildPlane(Vertices, Indices); break;
	}

	VertexCount = u32(Vertices.size());
	IndexCount = u32(Indices.size());

	if (!RHIUtils::CreateVertexBuffer(&VertexBuffer, Vertices.data(), u32(Vertices.size() * sizeof(Vertex))) ||
		!RHIUtils::CreateIndexBuffer(&IndexBuffer, Indices.data(), u32(Indices.size() * sizeof(u16))))
	{
		Msg("! Can't create the material preview mesh");
		return;
	}

	Geometry.create(PreviewVertexDecl, std::size(PreviewVertexDecl), VertexBuffer, IndexBuffer);
	MeshShape = Shape;
}

static bool TextureExists(const xr_string& Name)
{
	string_path Path;
	FS.update_path(Path, _game_textures_, (Name + ".dds").c_str());
	return FS.exist(Path) != nullptr;
}

// $game_textures$\wood\wood_a.thm -> wood\wood_a
xr_string CMaterialPreview::GetTextureName(const xr_string& ThmFile)
{
	string_path Root;
	FS.update_path(Root, _game_textures_, "");

	xr_string Name = ThmFile;
	xr_strlwr(Name);
	xr_string RootStr = Root;
	xr_strlwr(RootStr);
	std::replace(Name.begin(), Name.end(), '/', '\\');
	std::replace(RootStr.begin(), RootStr.end(), '/', '\\');

	if (!RootStr.empty() && Name.starts_with(RootStr))
	{
		Name.erase(0, RootStr.size());
	}

	const size_t Ext = Name.rfind('.');
	if (Ext != xr_string::npos)
	{
		Name.erase(Ext);
	}

	return Name;
}

void CMaterialPreview::UpdateShader(const STextureParams& Params)
{
	const bool IsPBR = Params.material == STextureParams::tmPBR_Material;

	xr_string Bump = Params.bump_name.size() ? Params.bump_name.c_str() : "";
	UseBump = Params.bump_mode != STextureParams::tbmNone && !Bump.empty() && TextureExists(Bump) && TextureExists(Bump + "#");

	xr_string Detail = Params.detail_name.size() ? Params.detail_name.c_str() : "";
	const bool HasDetail = !Detail.empty() && TextureExists(Detail);
	UseDetail = HasDetail && Params.flags.is_any(STextureParams::flDiffuseDetail | STextureParams::flBumpDetail);
	UseDetailBump = UseDetail && Params.flags.is(STextureParams::flBumpDetail) && TextureExists(Detail + "_bump") && TextureExists(Detail + "_bump#");

	xr_string Textures = TextureName;
	Textures += ",";
	Textures += UseBump ? Bump : "$null";
	Textures += ",";
	Textures += UseDetail ? Detail : "$null";

	const char* ShaderName = UseDetailBump ? "editor_material_preview_db" : "editor_material_preview";
	xr_string Key = xr_string(ShaderName) + "|" + Textures + (IsPBR ? "|pbr" : "");
	if (Key == ShaderKey && PreviewShader)
	{
		return;
	}

	ShaderKey = Key;
	PreviewShader.create(ShaderName, Textures.c_str());
}

bool CMaterialPreview::Render(const STextureParams& Params, u32 Width, u32 Height)
{
	if (!TargetColor || TargetColor->dwWidth != Width || TargetColor->dwHeight != Height)
	{
		TargetColor.destroy();
		TargetDepth.destroy();
		TargetColor.create("$user$material_preview_color", Width, Height, ERHI_FORMAT::B8G8R8A8_UNORM);
		TargetDepth.create("$user$material_preview_depth", Width, Height, ERHI_FORMAT::R24G8_TYPELESS);
	}

	BuildMesh();
	UpdateShader(Params);

	if (!TargetColor || !TargetDepth || TargetColor->pRT == nullptr || TargetDepth->pZRT == nullptr)
	{
		return false;
	}

	if (!Geometry || !PreviewShader || !PreviewShader->E[4])
	{
		return false;
	}

	Fvector Direction;
	Direction.setHP(Yaw, -Pitch);

	Fvector Position;
	Position.mad(Fvector().set(0.f, 0.f, 0.f), Direction, -Distance);

	Fmatrix View;
	View.build_camera_dir(Position, Direction, Fvector().set(0.f, 1.f, 0.f));

	Fmatrix Project;
	Project.build_projection(deg2rad(PreviewFov), float(Height) / float(Width), 0.05f, 50.f);

	// The light hangs over the right shoulder of the camera
	Fvector LightDir;
	LightDir.setHP(Yaw + LightYaw, -LightPitch);
	LightDir.invert();

	IRHIRenderTargetView* SavedRT[4] = {};
	for (u32 Idx = 0; Idx < 4; ++Idx)
	{
		SavedRT[Idx] = GRHI->GetRenderTargetView(Idx);
	}
	IRHIDepthStencilView* SavedDSV = GRHI->GetDepthStencilView();

	RCache.set_xform_world(Fidentity);
	RCache.set_xform_view(View);
	RCache.set_xform_project(Project);

	RCache.set_RT(TargetColor->pRT);
	RCache.set_RT(nullptr, 1);
	RCache.set_RT(nullptr, 2);
	RCache.set_RT(nullptr, 3);
	GRHI->SetDepthStencilView(TargetDepth->pZRT);
	GRHI->ApplyRenderTargetChange();

	GRHI->ClearTarget(TargetColor->pRT, PreviewClearColor);
	GRHI->ClearDepthStencil(TargetDepth->pZRT, ERHI_CLEAR_TARGET::DEPTH | ERHI_CLEAR_TARGET::STENCIL, 1.0f, 0);

	RHIViewport Viewport = {0, 0, (float)Width, (float)Height, 0.f, 1.f};
	GRHI->SetViewport(Viewport);

	const bool IsPBR = Params.material == STextureParams::tmPBR_Material;
	const bool UseParallax = UseBump && Params.bump_mode == STextureParams::tbmUseParallax;

	// Bind the element before set_c: the constants resolve against its table
	RCache.set_Element(PreviewShader->E[4]);
	RCache.set_c("preview_flags", UseBump ? 1.f : 0.f, UseParallax ? 1.f : 0.f, UseDetail ? 1.f : 0.f, UseDetailBump ? 1.f : 0.f);
	RCache.set_c("preview_material", Params.detail_scale, float(Params.material) + Params.material_weight, IsPBR ? 1.f : 0.f, AlphaTest ? 1.f : 0.f);
	RCache.set_c("preview_light", LightDir.x, LightDir.y, LightDir.z, Ambient);
	RCache.set_c("preview_eye", Position.x, Position.y, Position.z, UVTiling);
	RCache.set_Geometry(Geometry);
	RCache.Render(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 0, 0, VertexCount, 0, IndexCount / 3);

	RCache.set_xform_view(Device.mView);
	RCache.set_xform_project(Device.mProject);

	for (u32 Idx = 0; Idx < 4; ++Idx)
	{
		RCache.set_RT(SavedRT[Idx], Idx);
	}
	GRHI->SetDepthStencilView(SavedDSV);
	GRHI->ApplyRenderTargetChange();

	return true;
}

void CMaterialPreview::DrawToolbar()
{
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 7.f);
	int ShapeIdx = int(Shape);
	if (ImGui::Combo("##PreviewShape", &ShapeIdx, ShapeNames, int(std::size(ShapeNames))))
	{
		Shape = EShape(ShapeIdx);
	}

	ImGui::SameLine();
	ImGui::Checkbox("Rotate", &AutoRotate);
	ImGui::SameLine();
	ImGui::Checkbox("Alpha test", &AlphaTest);

	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 7.f);
	ImGui::SliderFloat("Tiling", &UVTiling, 0.25f, 8.f, "%.2f", ImGuiSliderFlags_Logarithmic);
	ImGui::SameLine();
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 7.f);
	ImGui::SliderFloat("Ambient", &Ambient, 0.f, 1.f, "%.2f");
	ImGui::SameLine();
	ImGui::TextDisabled("(?)");
	ImGui::SetItemTooltip("LMB: orbit, RMB: light, wheel: zoom");
}

void CMaterialPreview::Draw(const STextureParams& Params, const ImVec2& Size)
{
	if (TextureName.empty())
	{
		return;
	}

	DrawToolbar();

	ImVec2 CanvasSize = ImGui::GetContentRegionAvail();
	CanvasSize.x = std::max(CanvasSize.x, 32.f);
	CanvasSize.y = std::max(Size.y, 32.f);

	const ImVec2 CanvasPos = ImGui::GetCursorScreenPos();
	ImGui::InvisibleButton("##MaterialPreviewCanvas", CanvasSize, ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);

	const ImGuiIO& IO = ImGui::GetIO();
	if (ImGui::IsItemActive() && ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		Yaw += IO.MouseDelta.x * 0.01f;
		Pitch = clampr(Pitch + IO.MouseDelta.y * 0.01f, -1.5f, 1.5f);
	}
	else if (ImGui::IsItemActive() && ImGui::IsMouseDown(ImGuiMouseButton_Right))
	{
		LightYaw += IO.MouseDelta.x * 0.01f;
		LightPitch = clampr(LightPitch + IO.MouseDelta.y * 0.01f, -1.5f, 1.5f);
	}
	else if (AutoRotate)
	{
		Yaw += IO.DeltaTime * 0.5f;
	}

	if (ImGui::IsItemHovered() && IO.MouseWheel != 0.f)
	{
		Distance = clampr(Distance * (IO.MouseWheel > 0.f ? 0.9f : 1.1f), 1.3f, 8.f);
	}

	const u32 Width = u32(CanvasSize.x);
	const u32 Height = u32(CanvasSize.y);

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	const ImVec2 CanvasEnd = {CanvasPos.x + CanvasSize.x, CanvasPos.y + CanvasSize.y};

	if (Render(Params, Width, Height))
	{
		DrawList->AddImage(TargetColor->pTexture->get_SRView()->GetRawSRV(), CanvasPos, CanvasEnd);
	}
	else
	{
		DrawList->AddRectFilled(CanvasPos, CanvasEnd, IM_COL32(40, 40, 43, 255));
		DrawList->AddText({CanvasPos.x + 8.f, CanvasPos.y + 8.f}, IM_COL32(200, 200, 200, 255), "Preview unavailable");
	}
}
