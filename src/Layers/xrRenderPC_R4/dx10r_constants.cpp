#include "stdafx.h"
#include "dx10FixedConstants.h"

#include "ResourceManager.h"

#include "../../xrCore/xrPool.h"
#include "r_constants.h"
#include "dxRenderDeviceRender.h"
#include "dx10ConstantBuffer.h"

IC bool p_sort(ref_constant C1, ref_constant C2)
{
	return xr_strcmp(C1->name, C2->name) < 0;
}

bool R_constant_table::parseConstants(const RHIShaderBufferDesc* pTable, u32 destination, int fixed)
{
	VERIFY(pTable);
	const auto& TableDesc = *pTable;

	for (u32 i = 0; i < TableDesc.Variables.size(); ++i)
	{
		const auto& VarDesc = TableDesc.Variables[i];
		const auto& TypeDesc = VarDesc.Type;

		const char* name = VarDesc.Name.c_str();

		u16 type = u16(-1);
		switch (TypeDesc.Type)
		{
			case ERHI_SHADER_VARIABLE_TYPE::FLOAT:
				type = RC_float;
				break;
			case ERHI_SHADER_VARIABLE_TYPE::BOOL:
				type = RC_bool;
				break;
			case ERHI_SHADER_VARIABLE_TYPE::INT:
				type = RC_int;
				break;
			default:
				fatal("R_constant_table::parse: unexpected shader variable type.");
		}

		VERIFY(VarDesc.StartOffset < 0x10000);
		u16 r_index = u16(VarDesc.StartOffset);
		u16 r_type = u16(-1);

		bool bSkip = false;

		switch (TypeDesc.Class)
		{
			case ERHI_SHADER_VARIABLE_CLASS::SCALAR:
				r_type = RC_1x1;
				break;
			case ERHI_SHADER_VARIABLE_CLASS::VECTOR:
			{
				switch (TypeDesc.Columns)
				{
					case 4:
						r_type = RC_1x4;
						break;
					case 3:
						r_type = RC_1x3;
						break;
					case 2:
						r_type = RC_1x2;
						break;
					default:
						fatal("Vector: 1 components is scalar - there is special case for this!!!!!");
						break;
				}
			}
			break;
			case ERHI_SHADER_VARIABLE_CLASS::MATRIX_ROWS:
			{
				switch (TypeDesc.Columns)
				{
					case 4:
						switch (TypeDesc.Rows)
						{
							case 2:
								r_type = RC_2x4;
								break;
							case 3:
								r_type = RC_3x4;
								break;
							case 4:
								r_type = RC_4x4;
								break;
							default:
								fatal("MATRIX_ROWS: unsupported number of Rows");
								break;
						}
						break;
					default:
						fatal("MATRIX_ROWS: unsupported number of Columns");
						break;
				}
			}
			break;
			case ERHI_SHADER_VARIABLE_CLASS::MATRIX_COLUMNS:
				fatal("Pclass MATRIX_COLUMNS unsupported");
				break;
			case ERHI_SHADER_VARIABLE_CLASS::STRUCT:
				fatal("Pclass D3DXPC_STRUCT unsupported");
				break;
			case ERHI_SHADER_VARIABLE_CLASS::OBJECT:
			{
				//	TODO: DX10:
				VERIFY(!"Implement shader object parsing.");
			}
				bSkip = true;
				break;
			default:
				bSkip = true;
				break;
		}
		if (bSkip)
		{
			continue;
		}

		// We have determined all valuable info, search if constant already created
		ref_constant C = get(name);
		if (!C)
		{
			C = new RHIShaderConstant(); //.g_constant_allocator.create();
			C->name = name;
			C->name_hash = FixedConstants::NameHash(*C->name);
			C->destination = destination;
			C->type = type;
			C->fixed_id = (s8)fixed;
			// RHIShaderConstant::Loader& L	=	(destination&1)?C->ps:C->vs;
			RHIShaderConstant::Loader& L = C->get_load(destination); /*((destination&RC_dest_pixel)
								? C->ps : (destination&RC_dest_vertex)
								? C->vs : C->gs);*/
			L.index = r_index;
			L.cls = r_type;
			table.push_back(C);
		}
		else
		{
			if (fixed)
			{
				C->fixed_id = (s8)fixed;
			}
			C->destination |= destination;
			VERIFY(C->type == type);
			// RHIShaderConstant::Loader& L	=	(destination&1)?C->ps:C->vs;
			RHIShaderConstant::Loader& L = C->get_load(destination); /*((destination&RC_dest_pixel)
								? C->ps : (destination&RC_dest_vertex)
								? C->vs : C->gs);*/
			L.index = r_index;
			L.cls = r_type;
		}
	}
	return true;
}

bool R_constant_table::parseResources(const RHIShaderReflection* pReflection, int ResNum, u32 destination)
{
	for (int i = 0; i < ResNum; ++i)
	{
		const auto& ResDesc = pReflection->Resources[i];

		u16 type = 0;

		switch (ResDesc.Type)
		{
			case ERHI_SHADER_RESOURCE_TYPE::TEXTURE:
				type = RC_dx10texture;
				break;
			case ERHI_SHADER_RESOURCE_TYPE::SAMPLER:
				type = RC_sampler;
				break;
			case ERHI_SHADER_RESOURCE_TYPE::UAV_RWTYPED:
				type = RC_dx11UAV;
				break;
			default:
				continue;
		}

		VERIFY(ResDesc.BindCount == 1);

		u16 r_index = u16(-1);

		if (destination & RC_dest_pixel)
		{
			r_index = u16(ResDesc.BindPoint + CTexture::rstPixel);
		}
		else if (destination & RC_dest_vertex)
		{
			r_index = u16(ResDesc.BindPoint + CTexture::rstVertex);
		}
		else if (destination & RC_dest_geometry)
		{
			r_index = u16(ResDesc.BindPoint + CTexture::rstGeometry);
		}
		else if (destination & RC_dest_hull)
		{
			r_index = u16(ResDesc.BindPoint + CTexture::rstHull);
		}
		else if (destination & RC_dest_domain)
		{
			r_index = u16(ResDesc.BindPoint + CTexture::rstDomain);
		}
		else if (destination & RC_dest_compute)
		{
			r_index = u16(ResDesc.BindPoint + CTexture::rstCompute);
		}
		else
		{
			VERIFY(0);
		}

		ref_constant C = get(ResDesc.Name.c_str());
		if (!C)
		{
			C = new RHIShaderConstant(); //.g_constant_allocator.create();
			C->name = ResDesc.Name;
			C->name_hash = FixedConstants::NameHash(*C->name);
			C->destination = RC_dest_sampler;
			C->type = type;
			RHIShaderConstant::Loader& L = C->samp;
			L.index = r_index;
			L.cls = type;
			table.push_back(C);
		}
		else
		{
			R_ASSERT(C->destination == RC_dest_sampler);
			R_ASSERT(C->type == type);
			RHIShaderConstant::Loader& L = C->samp;
			R_ASSERT(L.index == r_index);
			R_ASSERT(L.cls == type);
		}
	}
	return true;
}

IC u32 dest_to_shift_value(u32 destination)
{
	switch (destination & 0xFF)
	{
		case RC_dest_vertex:
			return RC_dest_vertex_cb_index_shift;
		case RC_dest_pixel:
			return RC_dest_pixel_cb_index_shift;
		case RC_dest_geometry:
			return RC_dest_geometry_cb_index_shift;
		case RC_dest_hull:
			return RC_dest_hull_cb_index_shift;
		case RC_dest_domain:
			return RC_dest_domain_cb_index_shift;
		case RC_dest_compute:
			return RC_dest_compute_cb_index_shift;
		default:
			FATAL("invalid enumeration for shader");
	}
	return 0;
}

IC u32 dest_to_cbuf_type(u32 destination)
{
	switch (destination & 0xFF)
	{
		case RC_dest_vertex:
			return CB_BufferVertexShader;
		case RC_dest_pixel:
			return CB_BufferPixelShader;
		case RC_dest_geometry:
			return CB_BufferGeometryShader;
		case RC_dest_hull:
			return CB_BufferHullShader;
		case RC_dest_domain:
			return CB_BufferDomainShader;
		case RC_dest_compute:
			return CB_BufferComputeShader;
		default:
			FATAL("invalid enumeration for shader");
	}
	return 0;
}

bool R_constant_table::parse(const RHIShaderReflection* pReflection, u32 destination)
{



	if (pReflection->Buffers.size())
	{
		m_CBTable.reserve(pReflection->Buffers.size());
		//	Parse single constant table
		const RHIShaderBufferDesc* pTable = 0;

		for (u16 iBuf = 0; iBuf < pReflection->Buffers.size(); ++iBuf)
		{
			pTable = &pReflection->Buffers[iBuf];
			if (pTable)
			{
				const auto& TableDesc = *pTable;
				if (TableDesc.Type == 3)
				{
					continue;
				}

				u32 bindSlot = TableDesc.BindPoint;

				u32 updatedDest = destination;
				updatedDest |= bindSlot << dest_to_shift_value(destination);

				const int fixed = FixedConstants::FixedClass(TableDesc.Name.c_str());
				parseConstants(pTable, updatedDest, fixed);
				if (fixed)
				{
					continue;
				}

				u32 uiBufferIndex = bindSlot;
				uiBufferIndex |= dest_to_cbuf_type(destination);

				ref_cbuffer tempBuffer = DEV->_CreateConstantBuffer(pTable);
				m_CBTable.push_back(cb_table_record(uiBufferIndex, tempBuffer));
			}
		}
	}

	if (pReflection->Resources.size())
	{
		parseResources(pReflection, pReflection->Resources.size(), destination);
	}

	std::sort(table.begin(), table.end(), p_sort);
	return true;
}