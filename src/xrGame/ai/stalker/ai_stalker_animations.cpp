#include "stdafx.h"
#include "ai_stalker_animations.h"



enum Quantize
{
	MotionBits = 12,
	SlotBits = 2,
	IdxBits = 8,
	bBits = 1
};

#define MATPUSH(a)	\
((a)._11), ((a)._12), ((a)._13), ((a)._14), \
((a)._21), ((a)._22), ((a)._23), ((a)._24), \
((a)._31), ((a)._32), ((a)._33), ((a)._34), \
((a)._41), ((a)._42), ((a)._43), ((a)._44)

void StalkerMotionData::Quantize(NET_Packet& packet)										// Сжалось с 24 байт в 8 байт (54 бита еще 10 бит свободны)
{
	 
	u32 current = 0;
	u64 output = 0;

	write_bits<u64>(MotionBits, head.idx, current, output);				// 12
	write_bits<u64>(MotionBits, torso.idx, current, output);			// 24
	write_bits<u64>(MotionBits, legs.idx, current, output);				// 36
	write_bits<u64>(MotionBits, script.idx, current, output);			// 48
	write_bits<u64>(MotionBits, global.idx, current, output);			// 60
	write_bits<u64>(bBits, LastIsScript, current, output);				// 61

	packet.w_u64(output);

	current = 0;
	
	u64 output_32 = 0;
	write_bits<u64>(SlotBits, head.slot, current, output_32);			//2
	write_bits<u64>(SlotBits, torso.slot, current, output_32);			//4
	write_bits<u64>(SlotBits, legs.slot, current, output_32);			//6
	write_bits<u64>(SlotBits, script.slot, current, output_32);			//8			
	write_bits<u64>(SlotBits, global.slot, current, output_32);			//10

	write_bits<u64>(IdxBits, head_IDX, current, output_32);			// 4
	write_bits<u64>(IdxBits, torso_IDX, current, output_32);			// 8
	write_bits<u64>(IdxBits, legs_IDX, current, output_32);			// 12
	write_bits<u64>(IdxBits, script_IDX, current, output_32);		// 16
	write_bits<u64>(IdxBits, global_IDX, current, output_32);		// 20 * 2	

 	write_bits<u64>(bBits, ScriptAnimCtrl, current, output_32);			//1
	write_bits<u64>(bBits, GlobalAnimCtrl, current, output_32);			//2	 

	// 10 + 20 + 2 = 32 = 4 BYTE u32()
	packet.w_u64(output_32);

	// CUSTOM

	packet.w_u8(ScriptBone);
	packet.w_u8(GlobalBone);
	packet.w_u32(script_dwReciveTime);
//	if (ScriptAnimCtrl)
//		packet.w_matrix(ScriptMatrixImport);
//	if (GlobalAnimCtrl)
//		packet.w_matrix(GlobalMatrixImport);
}

void StalkerMotionData::Dequantize(NET_Packet& packet)
{
	u64 output = packet.r_u64();
	u64 output_32 = packet.r_u64();

	packet.r_u8(ScriptBone);
	packet.r_u8(GlobalBone);
	packet.r_u32(script_dwReciveTime);

	// PROCESS DATA
	u32 current = 0;
	head.idx	= read_bits<u64>(MotionBits, current, output);			// 12
	torso.idx	= read_bits<u64>(MotionBits, current, output);			// 24
	legs.idx	= read_bits<u64>(MotionBits, current, output);			// 36
	script.idx	= read_bits<u64>(MotionBits, current, output);			// 48
	global.idx	= read_bits<u64>(MotionBits, current, output);			// 60
	LastIsScript = read_bits<u64>(bBits, current, output);				// 61
	//

	current = 0;

	head.slot		= read_bits<u64>(SlotBits, current, output_32);		// 2	 
	torso.slot		= read_bits<u64>(SlotBits, current, output_32);		// 4
	legs.slot		= read_bits<u64>(SlotBits, current, output_32);		// 6
	script.slot		= read_bits<u64>(SlotBits, current, output_32);		// 8 
	global.slot		= read_bits<u64>(SlotBits, current, output_32);		// 10

	head_IDX		= read_bits<u64>(IdxBits, current, output_32);		// 4
	torso_IDX		= read_bits<u64>(IdxBits, current, output_32);		// 8
	legs_IDX		= read_bits<u64>(IdxBits, current, output_32);		// 12
	script_IDX		= read_bits<u64>(IdxBits, current, output_32);		// 16
	global_IDX		= read_bits<u64>(IdxBits, current, output_32);		// 20 * 2

	ScriptAnimCtrl  = read_bits<u64>(bBits, current, output_32);		// 1
	GlobalAnimCtrl  = read_bits<u64>(bBits, current, output_32);		// 2



	// БЕЗ МАТРИЦ  12 + 2 Байта с матрицами + 96	= 110
//	if (ScriptAnimCtrl)
//		packet.r_matrix(ScriptMatrixImport);	// 48 BYTES
//	if (GlobalAnimCtrl)
//		packet.r_matrix(GlobalMatrixImport);	// 48 BYTES
}


void StalkerMotionData::Serialize(NET_Packet& packet) // 21
{
	packet.w(&head, sizeof(MotionID));
	packet.w(&legs, sizeof(MotionID));
	packet.w(&torso, sizeof(MotionID));
	packet.w(&script, sizeof(MotionID));
	packet.w(&global, sizeof(MotionID));

	packet.w_u8(torso_IDX);
	packet.w_u8(head_IDX);
	packet.w_u8(legs_IDX);
	packet.w_u8(script_IDX);
	packet.w_u8(global_IDX);

	// ADVANCED
	packet.w_u8(ScriptBone);
	packet.w_u8(GlobalBone);

	packet.w_u8(ScriptAnimCtrl);// XFORM (NEED) 
 	packet.w_u8(GlobalAnimCtrl); 
}

void StalkerMotionData::Deserialize(NET_Packet& packet)
{
	packet.r(&head, sizeof(MotionID));
	packet.r(&legs, sizeof(MotionID));
	packet.r(&torso, sizeof(MotionID));
	packet.r(&script, sizeof(MotionID));
	packet.r(&global, sizeof(MotionID));	// 20

	packet.r_u8(torso_IDX);
	packet.r_u8(head_IDX);
	packet.r_u8(legs_IDX);
	packet.r_u8(script_IDX);
	packet.r_u8(global_IDX);				// 25

	// ADVANCED
	packet.r_u8(ScriptBone);
	packet.r_u8(GlobalBone);				 

	ScriptAnimCtrl = packet.r_u8();			// 29
 	GlobalAnimCtrl = packet.r_u8(); 
}
