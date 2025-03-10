#include"StdAfx.h"
#include "ai/stalker/ai_stalker.h"
#include "stalker_movement_manager_smart_cover.h"
#include "CharacterPhysicsSupport.h"
#include "PHMovementControl.h"

#include "../xrPhysics/PhysicsShell.h"
#include "../xrPhysics/IPHWorld.h"
#include "../xrPhysics/PhysicsCommon.h"

// ÈÍÒÅÐÏÎËßÖÈß
extern int g_cl_InterpolationType;

void CAI_Stalker::make_Interpolation()
{
	// Update Interpolation

	m_dwILastUpdateTime = Level().timeServer();

	if (g_Alive() && m_bInInterpolation)
	{
		u32 CurTime = m_dwILastUpdateTime;

		if (CurTime >= m_dwIEndTime)
		{
			m_bInInterpolation = false;

			CPHSynchronize* pSyncObj = nullptr;
			pSyncObj = PHGetSyncItem(0);
			if (!pSyncObj) return;
			pSyncObj->set_State(PredictedState);
			VERIFY2(_valid(renderable.xform), *cName());
		}
		else
		{
			float factor = 0.0f;

			if (m_dwIEndTime != m_dwIStartTime)
				factor = float(CurTime - m_dwIStartTime) / (m_dwIEndTime - m_dwIStartTime);

			clamp(factor, 0.f, 1.0f);

			Fvector NewPos;
			NewPos.lerp(IStart.Pos, IEnd.Pos, factor);

			VERIFY2(_valid(renderable.xform), *cName());

			movement().m_body.current.pitch = angle_lerp(IStart.o_torso.pitch, IEnd.o_torso.pitch, factor);
			//	movement().m_body.current.roll = angle_lerp(IStart.o_torso.roll, IEnd.o_torso.roll, factor);
			movement().m_body.current.yaw = angle_lerp(IStart.o_torso.yaw, IEnd.o_torso.yaw, factor);

			movement().m_head.current.pitch = angle_lerp(IStart.head.pitch, IEnd.head.pitch, factor);
			//	movement().m_head.current.roll = angle_lerp(IStart.head.roll, IEnd.head.roll, factor);
			movement().m_head.current.yaw = angle_lerp(IStart.head.yaw, IEnd.head.yaw, factor);

			for (u32 k = 0; k < 3; k++)
			{
				IPosL[k] = NewPos[k];
				IPosS[k] = factor * (factor * (factor * SCoeff[k][0] + SCoeff[k][1]) + SCoeff[k][2]) + SCoeff[k][3];
				IPosH[k] = factor * (factor * (factor * HCoeff[k][0] + HCoeff[k][1]) + HCoeff[k][2]) + HCoeff[k][3];
			};

			Fvector SpeedVector, ResPosition;
			switch (g_cl_InterpolationType)
			{
			case 0:
			{
				ResPosition.set(IPosL);
				SpeedVector.sub(IEnd.Pos, IStart.Pos);
				SpeedVector.div(float(m_dwIEndTime - m_dwIStartTime) / 1000.0f);
			}break;
			case 1:
			{
				for (int k = 0; k < 3; k++)
					SpeedVector[k] = (factor * factor * SCoeff[k][0] * 3 + factor * SCoeff[k][1] * 2 + SCoeff[k][2]) / 3; //     3       !!!!

				ResPosition.set(IPosS);
			}break;
			case 2:
			{
				for (int k = 0; k < 3; k++)
					SpeedVector[k] = (factor * factor * HCoeff[k][0] * 3 + factor * HCoeff[k][1] * 2 + HCoeff[k][2]);

				ResPosition.set(IPosH);
			}break;
			default:
				R_ASSERT2(0, "Unknown interpolation curve type!");
				break;
			}


			Position().set(ResPosition);
			character_physics_support()->movement()->SetPosition(ResPosition); // we need it ?
			character_physics_support()->movement()->SetVelocity(SpeedVector);

		};
	}
	else
	{
		m_bInInterpolation = false;
	};
};

void CAI_Stalker::CalculateInterpolationParams()
{

	if (NET_A.empty())
	{
		/// Msg("AI STALKER Strange Buffer NET_A: empty() (CalculateInterpolationParams failed !!!)");
		return;
	}


	CPHSynchronize* pSyncObj = nullptr;
	pSyncObj = PHGetSyncItem(0);

	stalker_interpolation::InterpData* pIStart = &IStart;
	//stalker_interpolation::InterpData* pIRec = &IRec;
	stalker_interpolation::InterpData* pIEnd = &IEnd;

	//pIRec->Pos = RecalculatedState.position;
	//pIRec->Vel = RecalculatedState.linear_vel;
	//pIRec->o_torso = NET_A_Last.o_torso;
	//pIRec->head = NET_A_Last.head;

	pIEnd->Pos = PredictedState.position;
	pIEnd->Vel = PredictedState.linear_vel;
	pIEnd->o_torso = NET_A_Last.o_torso;
	pIEnd->head = NET_A_Last.head;

	Fvector SP0, SP1, SP2, SP3;
	Fvector HP0, HP1, HP2, HP3;

	SP0 = pIStart->Pos;
	HP0 = pIStart->Pos;

	if (m_bInInterpolation)
	{
		u32 CurTime = Level().timeServer();
		float factor = float(CurTime - m_dwIStartTime) / (m_dwIEndTime - m_dwIStartTime);
		if (factor > 1.0f) factor = 1.0f;

		float c = factor;
		for (u32 k = 0; k < 3; k++)
		{
			SP0[k] = c * (c * (c * SCoeff[k][0] + SCoeff[k][1]) + SCoeff[k][2]) + SCoeff[k][3];
			SP1[k] = (c * c * SCoeff[k][0] * 3 + c * SCoeff[k][1] * 2 + SCoeff[k][2]) / 3; //     3       !!!!

			HP0[k] = c * (c * (c * HCoeff[k][0] + HCoeff[k][1]) + HCoeff[k][2]) + HCoeff[k][3];
			HP1[k] = (c * c * HCoeff[k][0] * 3 + c * HCoeff[k][1] * 2 + HCoeff[k][2]) / 3; //     3       !!!!
		};

		SP1.add(SP0);
	}
	else
	{
		if (LastState.linear_vel.x == 0 && LastState.linear_vel.y == 0 && LastState.linear_vel.z == 0)
		{
			HP1.sub(RecalculatedState.position, RecalculatedState.previous_position);
		}
		else
		{
			HP1.sub(LastState.position, LastState.previous_position);
		};
		HP1.mul(1.0f / fixed_step);
		SP1.add(HP1, SP0);
	}

	HP2.sub(PredictedState.position, PredictedState.previous_position);
	HP2.mul(1.0f / fixed_step);
	SP2.sub(PredictedState.position, HP2);

	SP3.set(PredictedState.position);
	HP3.set(PredictedState.position);

	Fvector TotalPath;
	TotalPath.sub(SP3, SP0);
	float TotalLen = TotalPath.magnitude();

	SPHNetState	State0 = (NET_A.back()).State;
	SPHNetState	State1 = PredictedState;

	float lV0 = State0.linear_vel.magnitude();
	float lV1 = State1.linear_vel.magnitude();

	u32 ConstTime = u32((fixed_step - physics_world()->FrameTime()) * 1000) + Level().GetInterpolationSteps() * u32(fixed_step * 1000);

	m_dwIStartTime = m_dwILastUpdateTime;
	m_dwIEndTime = m_dwIStartTime + ConstTime;

	Fvector V0, V1;
	V0.set(HP1);
	V1.set(HP2);
	lV0 = V0.magnitude();
	lV1 = V1.magnitude();

	if (TotalLen != 0)
	{
		if (V0.x != 0 || V0.y != 0 || V0.z != 0)
		{
			if (lV0 > TotalLen / 3)
			{
				HP1.normalize();
				//				V0.normalize();
				//				V0.mul(TotalLen/3);
				HP1.normalize();
				HP1.mul(TotalLen / 3);
				SP1.add(HP1, SP0);
			}
		}

		if (V1.x != 0 || V1.y != 0 || V1.z != 0)
		{
			if (lV1 > TotalLen / 3)
			{
				//				V1.normalize();
				//				V1.mul(TotalLen/3);
				HP2.normalize();
				HP2.mul(TotalLen / 3);
				SP2.sub(SP3, HP2);
			};
		}
	};
	/////////////////////////////////////////////////////////////////////////////
	for (u32 i = 0; i < 3; i++)
	{
		SCoeff[i][0] = SP3[i] - 3 * SP2[i] + 3 * SP1[i] - SP0[i];
		SCoeff[i][1] = 3 * SP2[i] - 6 * SP1[i] + 3 * SP0[i];
		SCoeff[i][2] = 3 * SP1[i] - 3 * SP0[i];
		SCoeff[i][3] = SP0[i];

		HCoeff[i][0] = 2 * HP0[i] - 2 * HP3[i] + HP1[i] + HP2[i];
		HCoeff[i][1] = -3 * HP0[i] + 3 * HP3[i] - 2 * HP1[i] - HP2[i];
		HCoeff[i][2] = HP1[i];
		HCoeff[i][3] = HP0[i];
	};
	/////////////////////////////////////////////////////////////////////////////
	m_bInInterpolation = true;

	if (m_pPhysicsShell)
		m_pPhysicsShell->NetInterpolationModeON();
}

void CAI_Stalker::postprocess_packet(stalker_interpolation::net_update_A& N_A)
{
	if (!NET_A.empty())
		N_A.dwTimeStamp = NET_A.back().dwTimeStamp;
	else
		N_A.dwTimeStamp = Level().timeServer();

	N_A.State.previous_position = N_A.State.position;
	N_A.State.previous_quaternion = N_A.State.quaternion;

	if (Local() && OnClient() || !g_Alive())
		return;

	if (!NET_A.empty() && N_A.dwTimeStamp < NET_A.back().dwTimeStamp) return;

	if (!NET_A.empty() && N_A.dwTimeStamp == NET_A.back().dwTimeStamp)
	{
		NET_A.back() = N_A;
	}
	else
	{
		//VERIFY(valid_pos(N_A.State.position));
		NET_A.push_back(N_A);
		if (NET_A.size() > 5)
		{
			NET_A.pop_front();
		}
	};

	m_bInterpolate = !NET_A.empty();

	if (NET_A.empty())
	{
		Msg("AI STALKER Strange Buffer NET_A: empty() (interpolation failed !!!)");
		return;
	}

	Level().AddObject_To_Objects4CrPr(this);
	CrPr_SetActivated(false);
	CrPr_SetActivationStep(0);
}

void CAI_Stalker::PH_B_CrPr()
{
	if (IsGameTypeSingle())
	{
		inherited::PH_B_CrPr();
		return;
	}

	if (CrPr_IsActivated()) return;
	if (CrPr_GetActivationStep() > physics_world()->StepsNum()) return;


	if (g_Alive())
	{
		CrPr_SetActivated(true);

		stalker_interpolation::InterpData* pIStart = &IStart;
		pIStart->Pos = Position();
		pIStart->Vel = m_pPhysics_support->movement()->GetVelocity();
 
		pIStart->o_torso.yaw = angle_normalize(movement().m_body.current.yaw);
		pIStart->o_torso.pitch = angle_normalize(movement().m_body.current.pitch);
 
		pIStart->head.pitch = angle_normalize(movement().m_head.current.pitch);
		pIStart->head.yaw = angle_normalize(movement().m_head.current.yaw);
 
		CPHSynchronize* pSyncObj = nullptr;
		pSyncObj = PHGetSyncItem(0);
		if (!pSyncObj) return;
		pSyncObj->get_State(LastState);

		if (Local() && OnClient())
		{
			PHUnFreeze();
			pSyncObj->set_State(NET_A.back().State);
		}
		else
			if (!NET_A.empty())
			{

				auto N_A = NET_A.back();
				NET_A_Last = N_A;

				if (!N_A.State.enabled)
				{
					pSyncObj->set_State(N_A.State);
				}
				else
				{
					PHUnFreeze();
					pSyncObj->set_State(N_A.State);
					Position().set(IStart.Pos);
				};
			};
	}
	else
	{
		CrPr_SetActivated(true);
		PHUnFreeze();
	}
}

void CAI_Stalker::PH_I_CrPr()
{
	if (IsGameTypeSingle())
	{
		inherited::PH_I_CrPr();
		return;
	}

	if (!CrPr_IsActivated()) return;

	if (g_Alive())
	{
		CPHSynchronize* pSyncObj = nullptr;
		pSyncObj = PHGetSyncItem(0);
		if (!pSyncObj) return;
		pSyncObj->get_State(RecalculatedState);
	};
}

void CAI_Stalker::PH_A_CrPr()
{
	if (IsGameTypeSingle())
	{
		inherited::PH_A_CrPr();
		return;
	}

	if (!CrPr_IsActivated()) return;
	if (!g_Alive()) return;

	CPHSynchronize* pSyncObj = nullptr;
	pSyncObj = PHGetSyncItem(0);
	if (!pSyncObj) return;

	pSyncObj->get_State(PredictedState);
	pSyncObj->set_State(RecalculatedState);

	if (!m_bInterpolate)
	{
		return;
	}

	CalculateInterpolationParams();
}