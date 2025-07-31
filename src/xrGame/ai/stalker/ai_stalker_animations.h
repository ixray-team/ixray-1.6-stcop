#pragma once

#include "../Include/xrRender/animation_motion.h"
//#include "../../movement_manager.h"

template<typename T>
IC	void write_bits(const u32& bit_count, const u32& value, u32& current, T& output)
{
	output |= ((value & ((T(1) << bit_count) - 1)) << current);
	current += bit_count;
}

template<typename T>
IC	u32 read_bits(const u32& bit_count, u32& current, const T& output)
{
	u32			result = (output >> current) & ((T(1) << bit_count) - 1);
	current += bit_count;
	
	return result;
}

IC	float unpack_float_bits(const u32& packed_value, const u32 bit_count)
{
	u32			max_value = (u32(1) << bit_count) - 1;
	float		result = float(packed_value) / (float(max_value) + .0001f);
	return		(result);
}

IC	u32 pack_float_bits(const float& unpacked_value, const u32 bit_count)
{
	float inValue = unpacked_value;
	clamp(inValue, 0.f, 1.f);
	u32			max_value = (u32(1) << bit_count) - 1;
	u32			result = iFloor(float(max_value) * inValue + .5f);

 
	if (bit_count > 1)
		if (result == 0 && unpacked_value != 0)
			result += 1;

	clamp(result, u32(0), max_value);
	return		(result);
}

struct Motions_Global
{
	u8 idx;
	u8 bone_part;
	MotionID id;
	Fmatrix target_matrix;

	bool mixing_anims;
 	bool use_controller;
	bool local_animation;
};
 
struct Motions_NUM
{
	MotionID id;
   	u8 boneID;
	bool AnimationCtrl;
	u32 IDX;
	Fmatrix motion_matrix;
};
 
struct BlendData
{
	u16 IDX;
	float current;
	float total;
	u16 ID;
};

struct StalkerMotionData
{
	bool LastIsScript = false;

	u32 script_dwReciveTime = 0;

	u8 legs_IDX;
	u8 torso_IDX;
	u8 head_IDX;
	u8 script_IDX;
	u8 global_IDX;

	u8 apply_IDX_torso;
	u8 apply_IDX_legs;
	u8 apply_IDX_head;
	u8 apply_IDX_script;
	u8 apply_IDX_global;
 
	u32 WaitingScript;

	MotionID head;
	MotionID torso;
	MotionID legs;
	MotionID script;
	MotionID global;

	u8 ScriptBone;
	u8 GlobalBone;


	// Script AnimCtrl
	bool ScriptAnimCtrl;
	bool GlobalAnimCtrl;
	Fmatrix ScriptMatrix;
 
	// stalker->num_torso_sv, stalker->num_legs_sv, stalker->num_head_sv
	void Initialize(StalkerMotionData server_data)
	{
 		*this = server_data;
	}

	void GetBlendData()
	{
		
	}

	// SETTER
	void SetTorso(Motions_NUM& data)
	{
		torso = data.id;
 		torso_IDX = data.IDX;
	};
	void SetLegs(Motions_NUM& data)
	{
		legs = data.id;
		legs_IDX = data.IDX;
	};
	void SetHead(Motions_NUM& data)
	{
		head = data.id;
		head_IDX = data.IDX;
	};
	void SetScript(Motions_NUM& data)
	{
		script				= data.id;
		ScriptBone			= data.boneID;
		script_IDX			= data.IDX;
		ScriptAnimCtrl		= data.AnimationCtrl;
		ScriptMatrix		= data.motion_matrix;
	};

	void SetGlobal(Motions_NUM& data)
	{
		global			= data.id;
		GlobalBone		= data.boneID;
		global_IDX		= data.IDX;
 		GlobalAnimCtrl	= data.AnimationCtrl;
 	}
	// GETTER

	void GetTorso(Motions_NUM& data)
	{
		data.id				= torso			;
		data.IDX = torso_IDX;
	};
	void GetLegs(Motions_NUM& data)
	{
		data.id = legs;
		data.IDX = legs_IDX;
	};
	void GetHead(Motions_NUM& data)
	{
		data.id = head;
		data.IDX = head_IDX;
	};
	void GetScript(Motions_NUM& data)
	{
		data.id			= script;
		data.IDX		= script_IDX;
		data.boneID		= ScriptBone;
 		data.AnimationCtrl = ScriptAnimCtrl;
		data.motion_matrix = ScriptMatrix;
  	};
	void GetGlobal(Motions_NUM& data)
	{
		data.id				= global;
		data.IDX			= global_IDX;
		data.boneID			= GlobalBone;
 		data.AnimationCtrl	= GlobalAnimCtrl;
 	};
	
	// CPP
	void Serialize(NET_Packet& packet);
	void Deserialize(NET_Packet& packet);
	void Quantize(NET_Packet& packet);
	void Dequantize(NET_Packet& packet);
};

struct MovementSyncData
{
	u16 movement_game_id;
	u32 movement_level_dest;
	bool movement_wait;
};

enum AnimationType
{
	eTypeHead,
	eTypeLegs,
	eTypeTorso,
	eTypeGlobal,
	eTypeScript
};
 