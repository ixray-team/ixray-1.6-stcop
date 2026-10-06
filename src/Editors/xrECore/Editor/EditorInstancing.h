#pragma once

class ECORE_API CEditorInstanceBatcher
{
public:
	bool Enabled = true;

	void Begin();
	void End();
	void OnDeviceDestroy();

	bool IsActive() const { return Active; }
	static bool IsShaderSupported(Shader* S);

	void Add(ref_shader& S, ref_geom& Geom, u32 VertexCount, const Fmatrix& World);

private:
	struct SInstance
	{
		Fvector4 Rows[3];
	};

	struct SItem
	{
		Shader* ShaderPtr;
		SGeometry* GeomPtr;
		u32 VertexCount;
		u32 InstanceID;
	};

	bool CreateBuffer();
	void Flush();
	void DrawRun(const SItem& First, u32 Offset, u32 Count);

	static constexpr u32 BufferCapacity = 4096;

	xr_vector<SItem> Items;
	xr_vector<SInstance> Instances;
	IRHIBuffer* Buffer = nullptr;
	IRHIShaderResourceView* SRV = nullptr;
	shared_str ParamsName;
	bool Active = false;
};

extern ECORE_API CEditorInstanceBatcher GEditorInstancing;
