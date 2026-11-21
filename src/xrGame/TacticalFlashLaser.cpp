#include "StdAfx.h"
#include "TacticalFlashLaser.h"
#include "HudTorchLight.h"

void CTacticalFlashLaser::Load(const char* section)
{
	inherited::Load(section);

	if (pSettings->read_if_exists<bool>(section, "torch_installed", false))
	{
		THudLightTorch& LightTorch = CreateComponent<THudLightTorch>();
		LightTorch.NewTorchlight(section);
	}

	if (pSettings->read_if_exists<bool>(section, "laser_installed", false))
	{
		THudLightLaser& LightLaser = CreateComponent<THudLightLaser>();
		LightLaser.NewTorchlight(section);
	}
}

void CTacticalFlashLaser::UpdateCL()
{
	inherited::UpdateCL();
	UpdateLights();
}

void CTacticalFlashLaser::OnH_A_Chield()
{
	inherited::OnH_A_Chield();
	UpdateLights();
}

void CTacticalFlashLaser::OnMoveToRuck(const SInvItemPlace& prev)
{
	inherited::OnMoveToRuck(prev);
	UpdateLights();
}

void CTacticalFlashLaser::UpdateLights()
{
	if (THudLightTorch* LightTorch = GetComponent<THudLightTorch>())
	{
		LightTorch->UpdateTorch(this, TorchStatus);
		LightTorch->UpdateTorchFromObject();
	}

	if (THudLightLaser* LightLaser = GetComponent<THudLightLaser>())
	{
		LightLaser->UpdateLaser(this, LaserStatus);
		LightLaser->UpdateTorchFromObject();
	}
}

void CTacticalFlashLaser::SwitchLaser(bool status)
{
	LaserStatus = status;
	UpdateLights();
}

void CTacticalFlashLaser::SwitchTorch(bool status)
{
	TorchStatus = status;
	UpdateLights();
}

void CTacticalFlashLaser::save(NET_Packet& output_packet)
{
	inherited::save(output_packet);
	output_packet.w_u8(TorchStatus ? 1 : 0);
	output_packet.w_u8(LaserStatus ? 1 : 0);
}

void CTacticalFlashLaser::load(IReader& input_packet)
{
	inherited::load(input_packet);
	TorchStatus = !!input_packet.r_u8();
	LaserStatus = !!input_packet.r_u8();
}

void CTacticalFlashLaser::Serialize(ISaveObject& Object)
{
	BEGIN_CHUNK(Object, "CTacticalFlashLaser::Serialize")
	{
		inherited::Serialize(Object);
		Object << TorchStatus << LaserStatus;
	}
}