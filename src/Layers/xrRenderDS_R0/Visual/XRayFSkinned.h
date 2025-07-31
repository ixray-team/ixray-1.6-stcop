#pragma once
#include "XRayFVisual.h"
#include "XRaySkeletonX.h"
#include "XRayFProgressive.h"

struct SEnumVerticesCallback
{
	virtual void operator () (const Fvector& p) = 0;
};


class CDS0_SkeletonXExt : public CDS0_SkeletonX	// shared code for SkeletonX derivates
{
protected:
	//BearFactoryPointer<BearRHI::BearRHIUniformBuffer> m_UniformBuffer;
	virtual void			_Load_hw(CDS0_FVisual& V, void* data);
	virtual void			_CollectBoneFaces(CDS0_FVisual* V, size_t iBase, size_t iCount);
	void			        _EnumBoneVertices(SEnumVerticesCallback& C, CDS0_FVisual* V, u16 bone_id, size_t iBase, size_t iCount) const;

	virtual BOOL			_PickBoneHW1W(IKinematics::pick_result& r, float range, const Fvector& S, const Fvector& D, CDS0_FVisual* V, u16* indices, CBoneData::FacesVec& faces);
	virtual BOOL			_PickBoneHW2W(IKinematics::pick_result& r, float range, const Fvector& S, const Fvector& D, CDS0_FVisual* V, u16* indices, CBoneData::FacesVec& faces);
	virtual BOOL			_PickBoneHW3W(IKinematics::pick_result& r, float range, const Fvector& S, const Fvector& D, CDS0_FVisual* V, u16* indices, CBoneData::FacesVec& faces);
	virtual BOOL			_PickBoneHW4W(IKinematics::pick_result& r, float range, const Fvector& S, const Fvector& D, CDS0_FVisual* V, u16* indices, CBoneData::FacesVec& faces);

	virtual BOOL			_PickBone(IKinematics::pick_result& r, float range, const Fvector& S, const Fvector& D, CDS0_FVisual* V, u16 bone_id, size_t iBase, size_t iCount);
	void UpdateUniform( void* ptr);
public:
};

class CDS0_SkeletonX_ST : public CDS0_FVisual, public CDS0_SkeletonXExt
{
private:
	typedef CDS0_FVisual			inherited1;
	typedef CDS0_SkeletonXExt	inherited2;
public:
	CDS0_SkeletonX_ST() {}
	virtual					~CDS0_SkeletonX_ST() {}
	//virtual bool		Render(float LOD, EShaderElement SEType, CDS0_ObjectRender& Item);
	virtual void			Load(const char* N, IReader* data, u32 dwFlags);
	virtual void			Copy(CDS0_RenderVisual* pFrom);
	virtual void			AfterLoad(CDS0_Kinematics* parent, u16 child_idx);
	virtual void			EnumBoneVertices(SEnumVerticesCallback& C, u16 bone_id);
	virtual BOOL			PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id);
//	virtual  void UpdateUniform(CDS0_UniformAllocator::EUniformType Type, void* ptr);;
private:
};

class CDS0_SkeletonX_PM : public CDS0_FProgressive, public CDS0_SkeletonXExt
{
private:
	typedef CDS0_FProgressive	inherited1;
	typedef CDS0_SkeletonXExt	inherited2;
public:
	CDS0_SkeletonX_PM() {}
	virtual					~CDS0_SkeletonX_PM() {}
	//virtual bool Render(float LOD, EShaderElement SEType, CDS0_ObjectRender& Item);
	virtual void			Load(const char* N, IReader* data, u32 dwFlags);
	virtual void			Copy(CDS0_RenderVisual* pFrom);
	virtual void			AfterLoad(CDS0_Kinematics* parent, u16 child_idx);
	virtual void			EnumBoneVertices(SEnumVerticesCallback& C, u16 bone_id);
	virtual BOOL			PickBone(IKinematics::pick_result& r, float dist, const Fvector& start, const Fvector& dir, u16 bone_id);
//	virtual  void UpdateUniform(CDS0_UniformAllocator::EUniformType Type, void* ptr);;
private:
};