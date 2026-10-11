#include "stdafx.h"

#include "D3DUtils.h"
#include "ui_main.h"
#include "UI_ToolsCustom.h"
#include "Library.h"
#include "EditObject.h"
#include "EditorSceneSubmit.h"

IBlender* CreateEditorBlender(CLASS_ID Class);

namespace
{
	class EditorRenderHost final : public IEditorRenderHost
	{
	public:
		bool IsUIReady() override
		{
			return EContext.UI != nullptr && EContext.UI->m_bReady;
		}

		bool IsPlayInEditor() override
		{
			return EContext.UI != nullptr && EContext.UI->IsPlayInEditor();
		}

		SPBItem* ProgressStart(float MaxValue, const char* Text) override
		{
			return EContext.UI->ProgressStart(MaxValue, Text);
		}

		void ProgressInc(SPBItem* Item) override
		{
			Item->Inc();
		}

		void ProgressEnd(SPBItem*& Item) override
		{
			EContext.UI->ProgressEnd(Item);
		}

		bool Confirm(const char* Text) override
		{
			return ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "%s", Text) == mrYes;
		}

		void UpdateProperties() override
		{
			ExecCommand(COMMAND_UPDATE_PROPERTIES);
		}

		bool RayPick(const Fvector& Start, const Fvector& Dir, float& Dist, Fvector* Point, Fvector* Normal) override
		{
			return Tools->RayPick(Start, Dir, Dist, Point, Normal);
		}

		void PlayCurrent(int Index) override
		{
			Tools->PlayCurrent(Index);
		}

		void StopCurrent(bool FinishPlaying) override
		{
			Tools->StopCurrent(FinishPlaying);
		}

		void SelectEffect(const char* Name) override
		{
			Tools->SelectEffect(Name);
		}

		void RemoveAction(u32 Index) override
		{
			Tools->RemoveAction(Index);
		}

		void EditPAC(PS::CPACDef* PAC) override
		{
			Tools->EditPAC(PAC);
		}

		void DrawAxis(const Fmatrix& Transform) override
		{
			if (!AxisObject)
			{
				AxisObject = Lib.CreateEditObject("editor\\axis");
			}
			R_ASSERT(AxisObject);

			Fmatrix M = Fidentity;
			Fmatrix S;
			S.scale(0.04f, 0.04f, 0.04f);
			M.mulB_44(S);

			static int _wh = 50;
			static float _kl = 1.0f;

			Ivector2 pt;
			pt.x = _wh;
			pt.y = iFloor(EContext.UI->GetRenderHeight() - _wh);

			Fvector dir;
			EContext.UI->CurrentView().m_Camera.MouseRayFromPoint(M.c, dir, pt);

			M.c.mad(dir, _kl);
			AxisObject->Render(NULL, M, 2, false);
		}

		void SubmitScene() override
		{
			// Same scene code as the overlay pass; the submit draw pass routes surfaces and
			// models to the scene graph and skips the helper draws.
			if (Tools)
			{
				Tools->Render();
			}
			GEditorSceneSubmit.Flush();
		}

		void DrawSceneHelpers() override
		{
			if (psDeviceFlags.is(rsDrawGrid))
			{
				DU_impl.DrawGrid();
			}
		}

		CDetailManager* GetSceneDetails() override
		{
			return Tools ? Tools->GetSceneDetails() : nullptr;
		}

		void SubmitShadowCasters() override
		{
			if (Tools)
			{
				Tools->SubmitShadowCasters();
			}
			GEditorSceneSubmit.Flush();
		}

		u32 CollectEditorLights(EditorLightDescriptor* Out, u32 Capacity) override
		{
			if (Tools)
			{
				return Tools->CollectEditorLights(Out, Capacity);
			}
			return 0;
		}

		IBlender* CreateBlender(CLASS_ID Class) override
		{
			return CreateEditorBlender(Class);
		}

		void DestroyObjects()
		{
			Lib.RemoveEditObject(AxisObject);
		}

	private:
		CEditableObject* AxisObject = nullptr;
	};

	EditorRenderHost Host;
}

void BindEditorRenderHost()
{
	g_EditorRenderHost = &Host;
	DU = &DU_impl;
}

void DestroyEditorRenderHostObjects()
{
	Host.DestroyObjects();
}
