////////////////////////////////////////////////////////////////////////////
//	Module 		: CameraRecoil.h
//	Created 	: 26.05.2008
//	Author		: Evgeniy Sokolov
//	Description : Camera Recoil struct
////////////////////////////////////////////////////////////////////////////
#pragma once

//отдача при стрельбе
struct CameraRecoil
{
	// Структура паттернов отдачи
	struct PatternParams
	{
		float Factor;
		float FactorAgility;
		float FactorAgilityVel;
		float FactorAgilityAccel;
		float FactorAgilityCrouch;
		float FactorAgilityCrouchNoAcc;
		float Stiffness;
		float Damping;
		float Impulse;
		bool  Loop;
		float ReturnSpeed;
		bool  ReturnEnable;
		Fvector2 RandomOffsetX;
		Fvector2 RandomOffsetY;
		bool  RandomOffsetEnable;

		PatternParams() :
			Factor(0.0f),
			FactorAgility(0.0f),
			FactorAgilityVel(0.0f),
			FactorAgilityAccel(0.0f),
			FactorAgilityCrouch(0.0f),
			FactorAgilityCrouchNoAcc(0.0f),
			Stiffness(0.0f),
			Damping(0.0f),
			Impulse(0.0f),
			Loop(true),
			ReturnSpeed(0.0f),
			ReturnEnable(true),
			RandomOffsetX(0.0f, 0.0f),
			RandomOffsetY(0.0f, 0.0f),
			RandomOffsetEnable(false)
		{
		}

		IC void Reset()
		{
			Factor = 0.0f;
			FactorAgility = 0.0f;
			FactorAgilityVel = 0.0f;
			FactorAgilityAccel = 0.0f;
			FactorAgilityCrouch = 0.0f;
			FactorAgilityCrouchNoAcc = 0.0f;
			Stiffness = 0.0f;
			Damping = 0.0f;
			Impulse = 0.0f;
			Loop = true;
			ReturnSpeed = 0.0f;
			ReturnEnable = true;
			RandomOffsetX.set(0.0f, 0.0f);
			RandomOffsetY.set(0.0f, 0.0f);
			RandomOffsetEnable = false;
		}
	};

	float		RelaxSpeed;
	float		RelaxSpeed_AI;
	float		Dispersion;
	float		DispersionInc;
	float		DispersionFrac;
	float		MaxAngleVert;
	float		MaxAngleHorz;
	float		StepAngleHorz;
	bool		ReturnMode;
	bool		StopReturn;

	PatternParams	Pattern;

	CameraRecoil() :
		MaxAngleVert(EPS),
		RelaxSpeed(EPS_L),
		RelaxSpeed_AI(EPS_L),
		Dispersion(EPS),
		DispersionInc(0.0f),
		DispersionFrac(1.0f),
		MaxAngleHorz(EPS),
		StepAngleHorz(0.0f),
		ReturnMode(false),
		StopReturn(false)
	{
	}

	CameraRecoil(const CameraRecoil& clone) { Clone(clone); }

	IC void Clone(const CameraRecoil& clone)
	{
		// *this = clone;
		RelaxSpeed = clone.RelaxSpeed;
		RelaxSpeed_AI = clone.RelaxSpeed_AI;
		Dispersion = clone.Dispersion;
		DispersionInc = clone.DispersionInc;
		DispersionFrac = clone.DispersionFrac;
		MaxAngleVert = clone.MaxAngleVert;
		MaxAngleHorz = clone.MaxAngleHorz;
		StepAngleHorz = clone.StepAngleHorz;

		ReturnMode = clone.ReturnMode;
		StopReturn = clone.StopReturn;

		Pattern = clone.Pattern;

		VERIFY(!fis_zero(RelaxSpeed));
		VERIFY(!fis_zero(RelaxSpeed_AI));
		VERIFY(!fis_zero(MaxAngleVert));
		VERIFY(!fis_zero(MaxAngleHorz));
	}


	IC void Reset()
	{
		RelaxSpeed = EPS_L;
		RelaxSpeed_AI = EPS_L;
		Dispersion = EPS;
		DispersionInc = 0.0f;
		DispersionFrac = 1.0f;
		MaxAngleVert = EPS;
		MaxAngleHorz = EPS;
		StepAngleHorz = 0.0f;
		ReturnMode = false;
		StopReturn = false;

		// Сбрасываем параметры паттерна
		Pattern.Reset();

		// Проверки
		VERIFY(!fis_zero(RelaxSpeed));
		VERIFY(!fis_zero(RelaxSpeed_AI));
		VERIFY(!fis_zero(MaxAngleVert));
		VERIFY(!fis_zero(MaxAngleHorz));
	}
};