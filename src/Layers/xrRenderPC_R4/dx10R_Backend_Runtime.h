#pragma once

IC void GfxCmdList::Dispatch(u32 GroupsX, u32 GroupsY, u32 GroupsZ)
{
	Stats.Calls++;
	GRHI->ShaderResourceCache->Apply();
	FlushStates();
	Constants.Flush();
	GRHI->Dispatch(GroupsX, GroupsY, GroupsZ);
}

IC bool GfxCmdList::PrepareDraw(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 Count, u32 Instances, bool Indexed)
{
	if (Count == 0 || Instances == 0 || Topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_FAN)
	{
		return false;
	}
	if (CurrentVS && CurrentVS->Input == EVertexInput::Legacy)
	{
		if (!CurrentVS->MismatchLogged)
		{
			CurrentVS->MismatchLogged = true;
			Msg("! Draw skipped: vertex shader '%s' expects input-assembler vertices", *CurrentVS->cName);
		}
		return false;
	}
	if (Indexed && GRHI->IsTessPass())
	{
		R_ASSERT(Topology == ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST);
		Topology = ERHI_PRIMITIVE_TOPOLOGY::CONTROL_POINT_3_PATCH;
	}
	Stats.Calls++;
	Stats.Polys += RHITopologyUtils::GetPrimitiveCount(Count, Topology) * Instances;
	if (Heap && Heap->HasPending())
	{
		Heap->Flush();
		Heap->Bind(*this);
	}
	GRHI->SetPrimitiveTopology(Topology);
	GRHI->ShaderResourceCache->Apply();
	GRHI->ApplyRenderTargetChange();
	ApplyVertexLayout();
	FlushStates();
	Constants.Flush();
	return true;
}

ICF void GfxCmdList::SetDrawBase(u32 VertexBase, u32 Flags)
{
	const CBDraw& Current = Constants.Read<CBDraw>();
	if (Current.draw_vertex_base != VertexBase || Current.draw_source_base != SourceBase || Current.draw_flags != Flags)
	{
		CBDraw& Draw = Constants.Write<CBDraw>();
		Draw.draw_vertex_base = VertexBase;
		Draw.draw_source_base = SourceBase;
		Draw.draw_flags = Flags;
	}
}

static_assert(u32(EGeomSource::Static) == DRAW_SOURCE_STATIC && u32(EGeomSource::Skinned) == DRAW_SOURCE_SKINNED && u32(EGeomSource::Ring) == DRAW_SOURCE_RING);

ICF void GfxCmdList::SetPulledBase(u32 VertexBase)
{
	SetDrawBase(VertexBase, u32(GeomSource) | (GeomPage << DRAW_PAGE_SHIFT));
}

IC void GfxCmdList::Draw(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 StartVertex)
{
	DrawInstanced(Topology, VertexCount, 1, StartVertex);
}

IC void GfxCmdList::DrawIndexed(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 IndexCount, u32 StartIndex, u32 BaseVertex)
{
	DrawIndexedInstanced(Topology, IndexCount, 1, StartIndex, BaseVertex, 0);
}

IC void GfxCmdList::DrawInstanced(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 VertexCount, u32 InstanceCount, u32 StartVertex, u32 StartInstance)
{
	SetPulledBase(StartVertex);
	if (PrepareDraw(Topology, VertexCount, InstanceCount, false))
	{
		GRHI->DrawInstanced(VertexCount, InstanceCount, 0, StartInstance);
	}
}

IC void GfxCmdList::DrawIndexedInstanced(ERHI_PRIMITIVE_TOPOLOGY Topology, u32 IndexCount, u32 InstanceCount, u32 StartIndex, u32 BaseVertex, u32 StartInstance)
{
	SetPulledBase(BaseVertex);
	if (PrepareDraw(Topology, IndexCount, InstanceCount, true))
	{
		GRHI->DrawIndexedInstanced(IndexCount, InstanceCount, StartIndex, 0, StartInstance);
	}
}

IC void GfxCmdList::DrawQuads(u32 Quads, u32 BaseVertex)
{
	VERIFY(GeomSource == EGeomSource::Ring);
	SetDrawBase(BaseVertex, DRAW_SOURCE_RING | DRAW_QUADS);
	if (PrepareDraw(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, 6 * Quads, 1, false))
	{
		GRHI->DrawInstanced(6 * Quads, 1, 0, 0);
	}
}

IC void GfxCmdList::DrawNoInput(u32 VertexCount)
{
	Draw(ERHI_PRIMITIVE_TOPOLOGY::TRIANGLE_LIST, VertexCount, 0);
}

IC void GfxCmdList::SetGeometry(SGeometry* _geom)
{
	SetIndices(_geom->ib);
	GeomSource = _geom->Source;
	GeomPage = _geom->Page;
	SourceBase = 0;
}

IC void GfxCmdList::ApplyVertexLayout()
{
	if (m_pInputLayout)
	{
		return;
	}
	if (!PulledLayout)
	{
		CreatePulledLayout();
	}
	m_pInputLayout = PulledLayout;
	GRHI->SetInputLayout(PulledLayout);
	IotaVB->SetVertexBuffer(0, sizeof(u32), 0);
}

ICF void GfxCmdList::SetVS(ref_vs& _vs)
{
	SetVS(_vs._get());
}

ICF void GfxCmdList::SetVS(SVS* _vs)
{
	CurrentVS = _vs;
	GRHI->SetShader(_vs ? _vs->vs : nullptr, ERHI_SHADER_TYPE::VS);
}

IC float GfxCmdList::GetWidth()
{
	return HalfTarget.x;
}

IC float GfxCmdList::GetHeight()
{
	return HalfTarget.y;
}

IC float GfxCmdList::GetTargetWidth()
{
	return float(RDEVICE.TargetWidth);
}

IC float GfxCmdList::GetTargetHeight()
{
	return float(RDEVICE.TargetHeight);
}

