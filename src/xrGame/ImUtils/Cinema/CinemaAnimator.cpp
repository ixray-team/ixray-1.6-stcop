#include "StdAfx.h"
#include "CinemaEditor.h"
#include "../../Cinema/CinemaOmf.h"
#include "../../Cinema/CinemaSceneIo.h"

namespace Cinema::AnimatorUi
{
constexpr const char* Modes[6] = { "Linear", "Ease in", "Ease out", "Ease in-out", "Bezier", "Hold" };
constexpr float Steps[9] = { 0.05f, 0.1f, 0.25f, 0.5f, 1.0f, 2.0f, 5.0f, 10.0f, 30.0f };
constexpr float SameTime = 1e-4f;
constexpr u8 AllBones = 255;

u8 Code(const Lane& Track, const Curve& Part)
{
	const u32 Kind = Track.Id == Param::BoneRotation ? 0 : Track.Id == Param::BonePosition ? 1 : 2;
	return static_cast<u8>(Kind * 3 + static_cast<u32>(&Part - Track.Curves));
}

template <typename List>
bool IsPicked(const List& Picked, u8 Layer, u8 Channel, float Time)
{
	return std::any_of(Picked.begin(), Picked.end(), [Layer, Channel, Time](const auto& Item) { return (Item.Bone == AllBones || Item.Bone == Layer) && (Item.Channel < 0 || Item.Channel == Channel) && std::fabs(Item.Time - Time) < SameTime; });
}

void KeyTimes(const ClipTrack& Clip, u8 Bone, xr_vector<float>& Out)
{
	Out.clear();
	for (const Lane& Track : Clip.Lanes)
	{
		for (u32 Part = 0; (Bone == AllBones || Track.Layer == Bone) && Part < 3; ++Part)
		{
			for (const Key& Item : Track.Curves[Part].Keys)
			{
				Out.push_back(Item.Time);
			}
		}
	}
	std::sort(Out.begin(), Out.end());
	Out.erase(std::unique(Out.begin(), Out.end(), [](float A, float B) { return std::fabs(A - B) < SameTime; }), Out.end());
}

void RestLocals(const ActorRuntime& Runtime, const xr_vector<Fmatrix>& Model, xr_vector<Fmatrix>& Local)
{
	const u16 Count = Runtime.BoneCount();
	Local.resize(Count);
	for (u16 Bone = 0; Bone < Count; ++Bone)
	{
		const u16 Parent = Runtime.BoneParent(Bone);
		Fmatrix Inverse = Fidentity;
		if (Parent < Count)
		{
			Inverse.invert(Model[Parent]);
		}
		Local[Bone].mul_43(Inverse, Model[Bone]);
	}
}

u32 MakeKeys(const ActorRuntime& Runtime, u32 Frames, const xr_vector<Fquaternion>& Turns, const xr_vector<Fvector>& Shifts, const xr_vector<u8>* Used, float Tolerance, AnimClip& Made, u32& Before, u32& After)
{
	const u16 Count = Runtime.BoneCount();
	xr_vector<Fmatrix> Model, Rest;
	if (!Runtime.RestPose(Model))
	{
		return Count;
	}
	RestLocals(Runtime, Model, Rest);
	u32 Skipped = 0;
	xr_vector<float> Values(static_cast<size_t>(Frames) * 6);
	for (u16 Bone = 0; Bone < Count; ++Bone)
	{
		if (Used && !(*Used)[Bone])
		{
			continue;
		}
		if (Made.Bones.size() >= MaxClipBones)
		{
			++Skipped;
			continue;
		}
		Fmatrix Back;
		Back.invert(Rest[Bone]);
		float Lo[6], Hi[6];
		for (u32 Frame = 0; Frame < Frames; ++Frame)
		{
			Fmatrix Local, Delta;
			Fvector Angles;
			Local.rotation(Turns[static_cast<size_t>(Frame) * Count + Bone]);
			Local.c = Shifts[static_cast<size_t>(Frame) * Count + Bone];
			Delta.mul_43(Back, Local);
			Delta.getHPB(Angles);
			for (u32 Part = 0; Part < 6; ++Part)
			{
				float& Value = Values[static_cast<size_t>(Frame) * 6 + Part];
				const float Degrees = Part < 3 ? rad2deg(Angles[Part]) : 0.0f, Last = Frame ? Values[static_cast<size_t>(Frame - 1) * 6 + Part] : 0.0f;
				Value = Part < 3 ? Degrees + 360.0f * std::round((Last - Degrees) / 360.0f) : Delta.c[Part - 3];
				Lo[Part] = Frame ? std::min(Lo[Part], Value) : Value;
				Hi[Part] = Frame ? std::max(Hi[Part], Value) : Value;
			}
		}
		const u8 Slot = static_cast<u8>(Made.Bones.size());
		PoseBone& Entry = Made.Bones.emplace_back();
		Entry.Name = Runtime.BoneName(Bone);
		Entry.Rotation.set(Values[0], Values[1], Values[2]);
		Entry.Offset.set(Values[3], Values[4], Values[5]);
		for (u32 Part = 0; Part < 6; ++Part)
		{
			if (Hi[Part] - Lo[Part] <= (Part < 3 ? 0.05f : 0.0005f))
			{
				continue;
			}
			Curve& Shape = Curves::EnsureLane(Made.Lanes, Part < 3 ? Param::BoneRotation : Param::BonePosition, Slot).Curves[Part % 3];
			Shape.Keys.resize(Frames);
			for (u32 Frame = 0; Frame < Frames; ++Frame)
			{
				Shape.Keys[Frame].Time = static_cast<float>(Frame) / Omf::MotionRate;
				Shape.Keys[Frame].Value = Values[static_cast<size_t>(Frame) * 6 + Part];
				Shape.Keys[Frame].Mode = Interp::Linear;
			}
			Before += Frames;
			After += Frames - Curves::Fit(Shape, 0.0f, static_cast<float>(Frames - 1) / Omf::MotionRate, Part < 3 ? Tolerance : Tolerance * 0.01f);
		}
	}
	return Skipped;
}

void RetargetPath(string_path& Path, const shared_str& From, const shared_str& To)
{
	string_path Name;
	xr_sprintf(Name, "cinema\\retarget\\%s__%s.ltx", From.c_str(), To.c_str());
	for (char* At = Name + xr_strlen("cinema\\retarget\\"); *At; ++At)
	{
		*At = *At == '\\' || *At == '/' ? '_' : *At;
	}
	FS.update_path(Path, "$game_config$", Name);
}

ImVec2 Arm(const Curve& Part, u32 At, bool IsOut, float Rest)
{
	const Key& Head = Part.Keys[IsOut ? At : At - 1];
	const Key& Item = Part.Keys[At];
	const float Width = Part.Keys[IsOut ? At + 1 : At].Time - Head.Time, Sign = IsOut ? 1.0f : -1.0f, Step = 0.01f * Width;
	if (Head.Mode == Interp::Bezier)
	{
		return IsOut ? ImVec2(std::clamp(Item.OutTime, 0.0f, Width), Item.OutValue) : ImVec2(std::clamp(Item.InTime, -Width, 0.0f), Item.InValue);
	}
	const float Slope = Head.Mode == Interp::Hold || !(Step > 0.0f) ? 0.0f : (Curves::Value(Part, Item.Time + Sign * Step, Rest) - Item.Value) / Step;
	return ImVec2(Sign * Width / 3.0f, Slope * Width / 3.0f);
}

bool IsEmpty(const Lane& Track)
{
	return std::all_of(std::begin(Track.Curves), std::end(Track.Curves), [](const Curve& Part) { return Part.Keys.empty(); });
}
}

namespace Cinema
{
void Editor::PlaceClip(ActorData& Setup, const shared_str& Name, const ActorRuntime* Runtime)
{
	ActorLayer& Over = Setup.Layers.emplace_back(NewRow(Runtime));
	ActorBlock Block;
	Block.Source = ClipSource;
	Block.Name = Name;
	Over.Blocks.push_back(Block);
}

void Editor::NewClip(Player& Core, u32 Id)
{
	const Object* Target = Find(Core.Data, Id);
	if (!Target || Target->Type != ObjectType::Actor || Target->Actor.Clips.size() >= MaxClips)
	{
		SetStatus(true, "An actor holds up to %u animation clips.", MaxClips);
		return;
	}
	ActorData& Setup = Edit(Core, Id, "New animation clip")->Actor;
	AnimClip Made;
	string64 Name;
	u32 Number = static_cast<u32>(Setup.Clips.size()) + 1;
	do
	{
		xr_sprintf(Name, "anim_%u", Number++);
	} while (FindClip(Setup.Clips, Name) >= 0);
	Made.Name = Name;
	Setup.Clips.push_back(Made);
	OpenAnimator(Core, Id, static_cast<s32>(Setup.Clips.size()) - 1);
}

void Editor::OpenAnimator(Player& Core, u32 Id, s32 Clip, bool ShouldFocus)
{
	const Object* Target = Find(Core.Data, Id);
	if (!Target || Target->Type != ObjectType::Actor)
	{
		return;
	}
	SetWorkspace(true);
	IsAnimFresh = false;
	AnimStockBlock = -1;
	if (PoseObject != Id)
	{
		SelectedBones.clear();
	}
	AnimObject = PoseObject = Id;
	AnimClipIndex = Clip < static_cast<s32>(Target->Actor.Clips.size()) ? Clip : -1;
	AnimLayer = 0;
	AnimKeys.clear();
	AnimZoom = 1.0f;
	AnimScroll = AnimTime = 0.0f;
	IsAnimPlaying = IsAnimDragging = false;
	if (!IsSelected(Id))
	{
		Select(Id, false, false);
	}
	(void)ShouldFocus;
}

bool Editor::SaveClip(Player& Core, u32 Id, u32 Index)
{
	const Object* Target = Find(Core.Data, Id);
	const ActorRuntime* Runtime = Target && Target->Type == ObjectType::Actor && Index < Target->Actor.Clips.size() ? Core.FindActor(Id) : nullptr;
	if (!Runtime || !Runtime->BoneCount())
	{
		SetStatus(true, "The actor's model is not loaded: the animation is not saved to the library.");
		return false;
	}
	const AnimClip& Clip = Target->Actor.Clips[Index];
	if (!Omf::IsClipName(Clip.Name.c_str()))
	{
		SetStatus(true, "Library names use a-z, 0-9 and _ only: rename the clip \"%s\".", Clip.Name.c_str());
		return false;
	}
	xr_vector<shared_str> Bones;
	xr_vector<Fquaternion> Turns;
	xr_vector<Fvector> Shifts;
	for (u16 Bone = 0; Bone < Runtime->BoneCount(); ++Bone)
	{
		Bones.push_back(Runtime->BoneName(Bone));
	}
	string_path File;
	xr_sprintf(File, "%s%s.omf", Omf::ClipFolder, Clip.Name.c_str());
	IWriter* Writer = Core.BakeClip(Id, static_cast<s32>(Index), Turns, Shifts) ? FS.w_open(_game_meshes_, File) : nullptr;
	const bool IsWritten = Writer && Writer->valid();
	if (IsWritten)
	{
		CMemoryWriter Output;
		Omf::WriteMotion(Clip.Name.c_str(), Bones, Turns, Shifts, Output);
		Omf::WriteClip(Clip, Output);
		Writer->w(Output.pointer(), Output.size());
	}
	if (Writer)
	{
		FS.w_close(Writer, true);
	}
	if (!IsWritten)
	{
		SetStatus(true, "Could not write the library animation %s.", Clip.Name.c_str());
	}
	return IsWritten;
}

void Editor::SaveLibrary(Player& Core)
{
	for (const Object& Item : Core.Data.Objects)
	{
		for (u32 Index = 0; Item.Type == ObjectType::Actor && Index < Item.Actor.Clips.size(); ++Index)
		{
			const AnimClip& Clip = Item.Actor.Clips[Index];
			AnimClip Stored;
			if (Clip.IsLinked && !(Omf::ReadClip(Clip.Name.c_str(), Stored) && (Stored.IsLinked = true, Stored == Clip)))
			{
				SaveClip(Core, Item.Id, Index);
			}
		}
	}
}

void Editor::OpenClip(Player& Core, u32 Id, const char* Name)
{
	const Object* Target = Find(Core.Data, Id);
	AnimClip Made;
	if (!Target || Target->Type != ObjectType::Actor || !Omf::ReadClip(Name, Made))
	{
		SetStatus(true, "%s has no editable keys: it was not saved from the Animator.", Name);
		return;
	}
	Made.IsLinked = true;
	const s32 Old = FindClip(Target->Actor.Clips, Made.Name);
	if (Old < 0 && Target->Actor.Clips.size() >= MaxClips)
	{
		SetStatus(true, "An actor holds up to %u animation clips.", MaxClips);
		return;
	}
	xr_vector<AnimClip>& Clips = Edit(Core, Id, "Open library animation")->Actor.Clips;
	Old < 0 ? Clips.push_back(Made) : (void)(Clips[Old] = Made);
	OpenAnimator(Core, Id, Old < 0 ? static_cast<s32>(Clips.size()) - 1 : Old);
}

void Editor::ImportClip(Player& Core, u32 Id, u8 Layer, u32 Block)
{
	const Object* Target = Find(Core.Data, Id);
	const ActorRuntime* Runtime = Target && Target->Type == ObjectType::Actor && Layer <= Target->Actor.Layers.size() && Block < BlocksOf(Target->Actor, Layer).size() ? Core.FindActor(Id) : nullptr;
	if (!Runtime || Target->Actor.Clips.size() >= MaxClips)
	{
		SetStatus(true, "The animation cannot be imported: the model is not loaded or the actor already has %u clips.", MaxClips);
		return;
	}
	const ActorBlock Played = BlocksOf(Target->Actor, Layer)[Block];
	xr_vector<Fquaternion> Turns;
	xr_vector<Fvector> Shifts;
	const u32 Frames = Core.BakeSource(Id, Played.Source, Played.Name, Turns, Shifts), Count = Runtime->BoneCount();
	if (Frames < 2 || Turns.size() != static_cast<size_t>(Frames) * Count)
	{
		SetStatus(true, "The animation %s could not be read.", Played.Name.c_str());
		return;
	}
	AnimClip Made;
	string64 Base = {}, Name;
	for (u32 Index = 0; Index < 48 && Index < Played.Name.size(); ++Index)
	{
		const char Symbol = static_cast<char>(tolower(static_cast<u8>(Played.Name.c_str()[Index])));
		Base[Index] = (Symbol >= 'a' && Symbol <= 'z') || (Symbol >= '0' && Symbol <= '9') ? Symbol : '_';
	}
	xr_strcpy(Name, Base);
	for (u32 Number = 2; FindClip(Target->Actor.Clips, Name) >= 0; ++Number)
	{
		xr_sprintf(Name, "%s_%u", Base, Number);
	}
	Made.Name = Name;
	Made.Length = static_cast<float>(Frames - 1) / Omf::MotionRate;
	u32 Before = 0, After = 0;
	const u32 Skipped = AnimatorUi::MakeKeys(*Runtime, Frames, Turns, Shifts, nullptr, AnimTolerance, Made, Before, After);
	Object& Item = *Edit(Core, Id, "Import animation as keys");
	Item.Actor.Clips.push_back(Made);
	ActorBlock& Swapped = BlocksOf(Item.Actor, Layer)[Block];
	Swapped.Source = ClipSource;
	Swapped.Name = Made.Name;
	Refit(Core, Item);
	OpenAnimator(Core, Id, static_cast<s32>(Item.Actor.Clips.size()) - 1);
	SetStatus(Skipped != 0, Skipped ? "Imported %s as the clip %s: %u samples became %u keys; %u bones past the limit were left out." : "Imported %s as the clip %s: %u samples became %u keys.", Played.Name.c_str(), Name, Before, After, Skipped);
}

void Editor::BuildRetargetMap(Player& Core)
{
	const Object *From = Find(Core.Data, RetargetFrom), *To = Find(Core.Data, RetargetTo);
	const ActorRuntime *Source = From ? Core.FindActor(RetargetFrom) : nullptr, *Target = To ? Core.FindActor(RetargetTo) : nullptr;
	RetargetMap.clear();
	RetargetRatio = 1.0f;
	RetargetPair[0] = RetargetFrom;
	RetargetPair[1] = RetargetTo;
	xr_vector<Fmatrix> Low, High;
	if (!Source || !Target || !Source->RestPose(Low) || !Target->RestPose(High))
	{
		return;
	}
	const u16 Many = Source->BoneCount(), Count = Target->BoneCount();
	RetargetMap.assign(Count, -1);
	for (u16 Bone = 0; Bone < Count; ++Bone)
	{
		const u16 Same = Source->FindBone(shared_str(Target->BoneName(Bone)));
		RetargetMap[Bone] = Same < Many ? Same : -1;
	}
	const ActorRig Kept = Rig;
	const shared_str KeptVisual = RigVisual;
	BuildRig(*Source, From->Actor.Visual);
	const ActorRig Old = Rig;
	BuildRig(*Target, To->Actor.Visual);
	const ActorRig New = Rig;
	Rig = Kept;
	RigVisual = KeptVisual;
	const auto Pair = [&](u16 Giver, u16 Taker)
	{
		if (Giver < Many && Taker < Count && RetargetMap[Taker] < 0)
		{
			RetargetMap[Taker] = Giver;
		}
	};
	for (u32 Control = 0; Control < RigControlCount; ++Control)
	{
		Pair(Old.Controls[Control], New.Controls[Control]);
	}
	for (u32 Chain = 0; Chain < RigChainCount; ++Chain)
	{
		for (u32 Link = 0; Link < 3; ++Link)
		{
			Pair(Old.Chains[Chain][Link], New.Chains[Chain][Link]);
		}
	}
	const float Height = Old.Controls[0] < Many ? Low[Old.Controls[0]].c.y : 0.0f, Reach = New.Controls[0] < Count ? High[New.Controls[0]].c.y : 0.0f;
	RetargetRatio = Height > 0.05f && Reach > 0.05f ? Reach / Height : 1.0f;
	string_path Path;
	AnimatorUi::RetargetPath(Path, From->Actor.Visual, To->Actor.Visual);
	const CReaderGuarded File(FS.exist(Path) ? FS.r_open(Path) : nullptr);
	xr_string Error;
	if (!File || static_cast<size_t>(File->length()) > 65536 || !Io::PreScan(*File, Error))
	{
		return;
	}
	File->seek(0);
	const CInifile Ini(&*File, nullptr);
	for (u32 Line = 0; Ini.section_exist("map") && Line < Ini.line_count("map"); ++Line)
	{
		const char *Key = nullptr, *Value = nullptr;
		Ini.r_line("map", static_cast<int>(Line), &Key, &Value);
		const u16 Taker = Key ? Target->FindBone(shared_str(Key)) : BI_NONE, Giver = Value ? Source->FindBone(shared_str(Value)) : BI_NONE;
		if (Taker < Count)
		{
			RetargetMap[Taker] = Giver < Many ? Giver : -1;
		}
	}
}

void Editor::SaveRetargetMap(Player& Core)
{
	const Object *From = Find(Core.Data, RetargetFrom), *To = Find(Core.Data, RetargetTo);
	const ActorRuntime *Source = From ? Core.FindActor(RetargetFrom) : nullptr, *Target = To ? Core.FindActor(RetargetTo) : nullptr;
	if (!Source || !Target || RetargetMap.size() != Target->BoneCount())
	{
		return;
	}
	CInifile Ini(nullptr, false, false, false);
	string_path Path;
	AnimatorUi::RetargetPath(Path, From->Actor.Visual, To->Actor.Visual);
	Ini.w_u32("retarget", "version", 1);
	for (u16 Bone = 0; Bone < Target->BoneCount(); ++Bone)
	{
		const s32 Giver = RetargetMap[Bone];
		Ini.w_string("map", Target->BoneName(Bone), Giver >= 0 && Giver < Source->BoneCount() ? Source->BoneName(static_cast<u16>(Giver)) : "none");
	}
	xr_string Error;
	const bool IsSaved = Io::SaveIni(Ini, Path, Error);
	SetStatus(!IsSaved, "%s", IsSaved ? "Saved the bone map for this pair of models." : Error.c_str());
}

void Editor::RetargetClip(Player& Core)
{
	const Object *From = Find(Core.Data, RetargetFrom), *To = Find(Core.Data, RetargetTo);
	const ActorRuntime *Source = From ? Core.FindActor(RetargetFrom) : nullptr, *Target = To ? Core.FindActor(RetargetTo) : nullptr;
	const bool IsValid = Source && Target && static_cast<u32>(RetargetClipIndex) < From->Actor.Clips.size() && RetargetMap.size() == Target->BoneCount() && To->Actor.Clips.size() < MaxClips;
	xr_vector<Fquaternion> Turns, Made;
	xr_vector<Fvector> Shifts, Moved;
	xr_vector<Fmatrix> Low, High, LowLocal, HighLocal;
	if (!IsValid || !Source->RestPose(Low) || !Target->RestPose(High) || !Core.BakeClip(RetargetFrom, RetargetClipIndex, Turns, Shifts))
	{
		SetStatus(true, "The clip cannot be retargeted: a model is not loaded, or the target already has %u clips.", MaxClips);
		return;
	}
	const AnimClip& Clip = From->Actor.Clips[RetargetClipIndex];
	const u16 Many = Source->BoneCount(), Count = Target->BoneCount();
	const u32 Frames = static_cast<u32>(Turns.size() / Many);
	AnimatorUi::RestLocals(*Source, Low, LowLocal);
	AnimatorUi::RestLocals(*Target, High, HighLocal);
	xr_vector<u8> Held(Many, 0), Used(Count, 0);
	const auto Hold = [&](const ClipTrack& Track)
	{
		for (const PoseBone& Bone : Track.Bones)
		{
			const u16 Id = Source->FindBone(Bone.Name);
			Id < Many ? (void)(Held[Id] = 1) : (void)0;
		}
	};
	Hold(Clip);
	for (const ClipLayer& Layer : Clip.Layers)
	{
		Hold(Layer);
	}
	u32 Mapped = 0;
	for (u16 Bone = 0; Bone < Count; ++Bone)
	{
		Used[Bone] = RetargetMap[Bone] >= 0 && RetargetMap[Bone] < Many && Held[RetargetMap[Bone]];
		Mapped += Used[Bone];
	}
	if (Frames < 2 || !Mapped)
	{
		SetStatus(true, "None of the clip's bones is mapped to a bone of the target model.");
		return;
	}
	xr_vector<Fmatrix> Posed(Many), Result(Count);
	for (u32 Frame = 0; Frame < Frames; ++Frame)
	{
		for (u16 Bone = 0; Bone < Many; ++Bone)
		{
			const u16 Parent = Source->BoneParent(Bone);
			Fmatrix Local;
			Local.rotation(Turns[static_cast<size_t>(Frame) * Many + Bone]);
			Local.c = Shifts[static_cast<size_t>(Frame) * Many + Bone];
			Parent < Many ? (void)Posed[Bone].mul_43(Posed[Parent], Local) : (void)(Posed[Bone] = Local);
		}
		for (u16 Bone = 0; Bone < Count; ++Bone)
		{
			const u16 Parent = Target->BoneParent(Bone);
			const Fmatrix Up = Parent < Count ? Result[Parent] : Fidentity;
			if (Used[Bone])
			{
				const u16 Giver = static_cast<u16>(RetargetMap[Bone]), Elder = Source->BoneParent(Giver);
				Fmatrix Now = Posed[Giver], Was = Low[Giver], Own = High[Bone], Back, Delta;
				Now.c.set(0.0f, 0.0f, 0.0f);
				Was.c.set(0.0f, 0.0f, 0.0f);
				Own.c.set(0.0f, 0.0f, 0.0f);
				Back.invert(Was);
				Delta.mul_43(Now, Back);
				Result[Bone].mul_43(Delta, Own);
				Fvector Home, Seat;
				(Elder < Many ? Posed[Elder] : Fidentity).transform_tiny(Home, LowLocal[Giver].c);
				Up.transform_tiny(Seat, HighLocal[Bone].c);
				Result[Bone].c.mad(Seat, Fvector().sub(Posed[Giver].c, Home), RetargetRatio);
			}
			else
			{
				Result[Bone].mul_43(Up, HighLocal[Bone]);
			}
			Fmatrix Inverse, Local;
			Inverse.invert(Up);
			Local.mul_43(Inverse, Result[Bone]);
			Local.i.normalize_safe();
			Local.j.normalize_safe();
			Local.k.normalize_safe();
			Made.push_back(Fquaternion().set(Local).normalize());
			Moved.push_back(Local.c);
		}
	}
	AnimClip Out;
	string64 Name;
	xr_strcpy(Name, Clip.Name.c_str());
	for (u32 Number = 2; FindClip(To->Actor.Clips, Name) >= 0; ++Number)
	{
		xr_sprintf(Name, "%.48s_%u", Clip.Name.c_str(), Number);
	}
	Out.Name = Name;
	Out.Length = Clip.Length;
	u32 Before = 0, After = 0;
	const u32 Skipped = AnimatorUi::MakeKeys(*Target, Frames, Made, Moved, &Used, AnimTolerance, Out, Before, After);
	xr_vector<AnimClip>& Clips = Edit(Core, RetargetTo, "Retarget animation clip")->Actor.Clips;
	Clips.push_back(Out);
	ShowRetarget = false;
	OpenAnimator(Core, RetargetTo, static_cast<s32>(Clips.size()) - 1);
	SetStatus(Skipped != 0, "Retargeted %s onto %u bones (%u keys)%s", Name, Mapped - Skipped, After, Skipped ? "; bones past the clip limit were left out." : ".");
}

void Editor::DrawRetarget(Player& Core)
{
	if (!ShowRetarget)
	{
		return;
	}
	ImGui::SetNextWindowSize(ImVec2(ImGui::GetFontSize() * 30.0f, ImGui::GetFontSize() * 36.0f), ImGuiCond_FirstUseEver);
	if (!ImGui::Begin("Retarget animation###CinemaRetarget", &ShowRetarget, ImGuiWindowFlags_NoNavInputs))
	{
		ImGui::End();
		return;
	}
	IsOwnWindowFocused |= ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	const Object* From = Find(Core.Data, RetargetFrom);
	const ActorRuntime* Source = From && From->Type == ObjectType::Actor && static_cast<u32>(RetargetClipIndex) < From->Actor.Clips.size() ? Core.FindActor(RetargetFrom) : nullptr;
	if (!Source)
	{
		ImGui::TextDisabled("Open a clip in the Animator and press Retarget.");
		ImGui::End();
		return;
	}
	const Object* To = Find(Core.Data, RetargetTo);
	const ActorRuntime* Target = To && To->Type == ObjectType::Actor && RetargetTo != RetargetFrom ? Core.FindActor(RetargetTo) : nullptr;
	ImGui::Text("%s: %s", DisplayName(From), From->Actor.Clips[RetargetClipIndex].Name.c_str());
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 14.0f);
	if (ImGui::BeginCombo("Onto", Target ? DisplayName(To) : "Pick an actor"))
	{
		for (const Object& Item : Core.Data.Objects)
		{
			if (Item.Type != ObjectType::Actor || Item.Id == RetargetFrom || !Core.FindActor(Item.Id))
			{
				continue;
			}
			ImGui::PushID(static_cast<s32>(Item.Id));
			if (ImGui::Selectable(DisplayName(&Item), Item.Id == RetargetTo))
			{
				RetargetTo = Item.Id;
				RetargetMap.clear();
			}
			ImGui::PopID();
		}
		ImGui::EndCombo();
	}
	if (!Target)
	{
		ImGui::TextDisabled("Pick the actor that receives the animation.");
		ImGui::End();
		return;
	}
	if (RetargetMap.size() != Target->BoneCount() || RetargetPair[0] != RetargetFrom || RetargetPair[1] != RetargetTo)
	{
		BuildRetargetMap(Core);
	}
	if (RetargetMap.size() != Target->BoneCount())
	{
		ImGui::TextDisabled("The models are not loaded yet.");
		ImGui::End();
		return;
	}
	if (ImGui::Button("Reset map"))
	{
		BuildRetargetMap(Core);
	}
	ImGui::SetItemTooltip("Map again by bone name and rig, then by the saved map of this pair of models");
	ImGui::SameLine();
	if (ImGui::Button("Save map"))
	{
		SaveRetargetMap(Core);
	}
	ImGui::SameLine();
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 5.0f);
	ImGui::DragFloat("Travel", &RetargetRatio, 0.01f, 0.05f, 20.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
	ImGui::SetItemTooltip("Scale of the hips' and root's travel: the ratio of the models' hip heights by default");
	const s32 Mapped = static_cast<s32>(std::count_if(RetargetMap.begin(), RetargetMap.end(), [](s32 Giver) { return Giver >= 0; }));
	ImGui::TextDisabled("%d of %d bones take motion", Mapped, static_cast<s32>(RetargetMap.size()));
	ImGui::BeginChild("##RetargetMap", ImVec2(0.0f, -ImGui::GetFrameHeightWithSpacing()), ImGuiChildFlags_Borders);
	for (u16 Bone = 0; Bone < Target->BoneCount(); ++Bone)
	{
		const s32 Giver = RetargetMap[Bone];
		const bool IsMapped = Giver >= 0 && Giver < Source->BoneCount();
		ImGui::PushID(Bone);
		ImGui::AlignTextToFramePadding();
		IsMapped ? ImGui::TextUnformatted(Target->BoneName(Bone)) : ImGui::TextDisabled("%s", Target->BoneName(Bone));
		ImGui::SameLine(ImGui::GetContentRegionAvail().x * 0.5f);
		ImGui::SetNextItemWidth(-1.0f);
		if (ImGui::BeginCombo("##From", IsMapped ? Source->BoneName(static_cast<u16>(Giver)) : "-"))
		{
			if (ImGui::Selectable("-", !IsMapped))
			{
				RetargetMap[Bone] = -1;
			}
			for (u16 Other = 0; Other < Source->BoneCount(); ++Other)
			{
				if (ImGui::Selectable(Source->BoneName(Other), Giver == Other))
				{
					RetargetMap[Bone] = Other;
				}
			}
			ImGui::EndCombo();
		}
		ImGui::PopID();
	}
	ImGui::EndChild();
	if (ImGui::Button("Retarget"))
	{
		RetargetClip(Core);
	}
	ImGui::End();
}

ClipTrack* Editor::EditClip(Player& Core, const char* Label)
{
	const Object* Target = Find(Core.Data, AnimObject);
	const s32 Block = Target && Target->Type == ObjectType::Actor ? PoseBlock(*Target) : -1;
	return Block >= 0 ? &TrackOf(*Edit(Core, AnimObject, Label), Block) : nullptr;
}

void Editor::UpdateAnimator(Player& Core)
{
	const bool WasFollowing = IsAnimFollowing;
	IsAnimFollowing = false;
	const Object* Focused = Focus == FocusKind::Block && FocusBlock >= 0 ? Find(Core.Data, FocusObject) : nullptr;
	const xr_vector<ActorBlock>* Row = Focused && Focused->Type == ObjectType::Actor && FocusLayer <= Focused->Actor.Layers.size() ? &BlocksOf(Focused->Actor, FocusLayer) : nullptr;
	const ActorBlock* Pressed = Row && FocusBlock < static_cast<s32>(Row->size()) ? &(*Row)[FocusBlock] : nullptr;
	const bool IsSeen = Pressed && AnimSeenObject == FocusObject && AnimSeenLayer == FocusLayer && AnimSeenBlock == FocusBlock;
	const bool IsAsked = Pressed && !ShowAnimator && Drag == DragKind::None && IsAnimJump;
	IsAnimJump = Pressed && !ShowAnimator && !IsSeen && ImGui::IsMouseReleased(ImGuiMouseButton_Left) && !ImGui::IsMouseDragPastThreshold(ImGuiMouseButton_Left, 4.0f) && MainBox.Contains(ImGui::GetIO().MousePos);
	if (ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		AnimSeenObject = Pressed ? FocusObject : 0;
		AnimSeenLayer = FocusLayer;
		AnimSeenBlock = Pressed ? FocusBlock : -1;
	}
	const s32 Chosen = IsAsked && IsClipBlock(Pressed->Source) ? FindClip(Focused->Actor.Clips, Pressed->Name) : -1;
	if (Chosen >= 0)
	{
		OpenAnimator(Core, FocusObject, Chosen, false);
	}
	else if (IsAsked)
	{
		if (!IsSelected(FocusObject))
		{
			Select(FocusObject, false, false);
		}
		SetWorkspace(true);
	}
	const Object* Target = ShowAnimator ? Find(Core.Data, AnimObject) : nullptr;
	if (!Target || Target->Type != ObjectType::Actor || AnimClipIndex >= static_cast<s32>(Target->Actor.Clips.size()))
	{
		AnimClipIndex = -1;
	}
	if (AnimClipIndex < 0)
	{
		AnimKeys.clear();
		IsAnimDragging = false;
		return;
	}
	const AnimClip& Clip = Target->Actor.Clips[AnimClipIndex];
	const ActorRuntime* Runtime = Core.FindActor(AnimObject);
	float Played = 0.0f;
	IsAnimFollowing = IsAnimLinked && Runtime && Runtime->ClipMoment(AnimClipIndex, Played);
	if (IsAnimFollowing)
	{
		const float Moved = Core.Time - AnimLastScene, Rate = std::fabs(Moved) > 1e-4f ? (Played - AnimLastTime) / Moved : 0.0f;
		AnimRate = WasFollowing && Rate > 0.05f && Rate < 20.0f ? Rate : AnimRate;
		AnimTime = Played;
	}
	else
	{
		AnimTime = IsAnimPlaying ? std::fmod(AnimTime + Device.fTimeDelta, std::max(Clip.Length, MinLength)) : std::clamp(AnimTime, 0.0f, Clip.Length);
		Core.PreviewClip(AnimObject, AnimClipIndex, AnimTime);
	}
	AnimLastScene = Core.Time;
	AnimLastTime = AnimTime;
	ClipOrigin = Core.Time - AnimTime;
}

void Editor::SeekAnimator(Player& Core, const Object& Target, float Time)
{
	Time = std::clamp(Time, 0.0f, Target.Actor.Clips[AnimClipIndex].Length);
	if (IsAnimFollowing)
	{
		Core.Seek(Core.Time + (Time - AnimTime) / AnimRate);
	}
	AnimTime = Time;
	ClipOrigin = Core.Time - AnimTime;
}

void Editor::DeleteAnimKeys(Player& Core)
{
	ClipTrack* Clip = AnimKeys.empty() ? nullptr : EditClip(Core, "Delete animation keys");
	if (!Clip)
	{
		return;
	}
	for (Lane& Track : Clip->Lanes)
	{
		for (Curve& Part : Track.Curves)
		{
			std::erase_if(Part.Keys, [&](const Key& Item) { return AnimatorUi::IsPicked(AnimKeys, Track.Layer, AnimatorUi::Code(Track, Part), Item.Time); });
		}
	}
	std::erase_if(Clip->Lanes, AnimatorUi::IsEmpty);
	AnimKeys.clear();
}

void Editor::SetAnimMode(Player& Core, Interp Mode)
{
	ClipTrack* Clip = AnimKeys.empty() ? nullptr : EditClip(Core, "Animation key curve");
	if (!Clip)
	{
		return;
	}
	for (Lane& Track : Clip->Lanes)
	{
		for (Curve& Part : Track.Curves)
		{
			for (u32 Index = 0; Index < Part.Keys.size(); ++Index)
			{
				if (AnimatorUi::IsPicked(AnimKeys, Track.Layer, AnimatorUi::Code(Track, Part), Part.Keys[Index].Time))
				{
					Curves::SetMode(Part, Index, Mode);
				}
			}
		}
	}
}

void Editor::FitAnimKeys(Player& Core)
{
	float From = FLT_MAX, To = -FLT_MAX;
	for (const AnimKey& Item : AnimKeys)
	{
		From = std::min(From, Item.Time);
		To = std::max(To, Item.Time);
	}
	ClipTrack* Clip = To - From > AnimatorUi::SameTime ? EditClip(Core, "Fit animation curve") : nullptr;
	if (!Clip)
	{
		SetStatus(true, "Select the first and the last key of the run to fit.");
		return;
	}
	u32 Before = 0, Removed = 0;
	for (Lane& Track : Clip->Lanes)
	{
		const bool IsChosen = std::any_of(AnimKeys.begin(), AnimKeys.end(), [&Track](const AnimKey& Item) { return Item.Bone == AnimatorUi::AllBones || Item.Bone == Track.Layer; });
		for (u32 Part = 0; IsChosen && Part < 3; ++Part)
		{
			Before += static_cast<u32>(Track.Curves[Part].Keys.size());
			Removed += Curves::Fit(Track.Curves[Part], From, To, Track.Id == Param::BoneRotation ? AnimTolerance : AnimTolerance * 0.01f);
		}
	}
	AnimKeys.clear();
	SetStatus(false, "Fit curve: %u keys became %u.", Before, Before - Removed);
}

void Editor::MoveAnimKeys(Player& Core, float Shift)
{
	ClipTrack* Clip = EditClip(Core, "Move animation keys");
	if (!Clip)
	{
		return;
	}
	Clip->Lanes = AnimBase;
	AnimKeys = AnimBaseKeys;
	for (AnimKey& Item : AnimKeys)
	{
		Item.Time += Shift;
	}
	for (Lane& Track : Clip->Lanes)
	{
		for (Curve& Part : Track.Curves)
		{
			const u8 Layer = Track.Layer;
			std::erase_if(Part.Keys, [&](const Key& Item) { return !AnimatorUi::IsPicked(AnimBaseKeys, Layer, AnimatorUi::Code(Track, Part), Item.Time) && AnimatorUi::IsPicked(AnimKeys, Layer, AnimatorUi::Code(Track, Part), Item.Time); });
			for (Key& Item : Part.Keys)
			{
				Item.Time += AnimatorUi::IsPicked(AnimBaseKeys, Layer, AnimatorUi::Code(Track, Part), Item.Time) ? Shift : 0.0f;
			}
			std::stable_sort(Part.Keys.begin(), Part.Keys.end(), [](const Key& A, const Key& B) { return A.Time < B.Time; });
		}
	}
}

void Editor::RemoveAnimBone(Player& Core, u8 Bone)
{
	ClipTrack* Clip = EditClip(Core, "Remove bone from clip");
	if (!Clip || Bone >= Clip->Bones.size())
	{
		return;
	}
	std::erase_if(Clip->Lanes, [Bone](const Lane& Track) { return Track.Layer == Bone; });
	for (Lane& Track : Clip->Lanes)
	{
		Track.Layer = static_cast<u8>(Track.Layer > Bone ? Track.Layer - 1 : Track.Layer);
	}
	Clip->Bones.erase(Clip->Bones.begin() + Bone);
	AnimKeys.clear();
}

void Editor::SetWorkspace(bool IsAnimation)
{
	if (ShowAnimator == IsAnimation)
	{
		return;
	}
	bool* const Live[4] = { &Settings.ShowInspector, &Settings.ShowPresets, &Settings.ShowShots, &Settings.ShowGraph };
	for (u32 Index = 0; Index < 4; ++Index)
	{
		WorkspaceShown[ShowAnimator][Index] = *Live[Index];
		*Live[Index] = WorkspaceShown[IsAnimation][Index];
	}
	ShowAnimator = IsPosing = IsAnimFresh = IsAnimation;
	IsAnimPlaying = IsAnimDragging = false;
	if (!IsAnimation)
	{
		Focus = FocusKind::Objects;
		FocusBlock = -1;
	}
	Drag = DragKind::None;
}

void Editor::DrawAnimatorBody(Player& Core)
{
	IsAnimFocused = ImGui::IsWindowFocused(ImGuiFocusedFlags_RootAndChildWindows);
	if (PoseObject && PoseObject != AnimObject)
	{
		AnimObject = PoseObject;
		AnimClipIndex = AnimStockBlock = -1;
		AnimKeys.clear();
		IsAnimFresh = true;
	}
	const Object* Target = Find(Core.Data, AnimObject);
	if (!Target || Target->Type != ObjectType::Actor)
	{
		u32 Chosen = 0;
		s32 ChosenClip = -1, ChosenBlock = -1;
		u8 ChosenLayer = 0;
		const bool HasActors = std::any_of(Core.Data.Objects.begin(), Core.Data.Objects.end(), [](const Object& Item) { return Item.Type == ObjectType::Actor; });
		if (HasActors)
		{
			ImGui::TextDisabled("Pick an actor of the scene, or one of its animations:");
			ImGui::BeginChild("##AnimActors", ImVec2(0.0f, ImGui::GetTextLineHeightWithSpacing() * 6.0f), ImGuiChildFlags_Borders);
		}
		for (const Object& Item : Core.Data.Objects)
		{
			if (Item.Type != ObjectType::Actor)
			{
				continue;
			}
			ImGui::PushID(static_cast<s32>(Item.Id));
			Chosen = ImGui::Selectable(DisplayName(&Item)) ? Item.Id : Chosen;
			ImGui::Indent();
			for (u32 Index = 0; Index < Item.Actor.Clips.size(); ++Index)
			{
				ImGui::PushID(static_cast<s32>(Index));
				if (ImGui::Selectable(Item.Actor.Clips[Index].Name.c_str()))
				{
					Chosen = Item.Id;
					ChosenClip = static_cast<s32>(Index);
				}
				ImGui::PopID();
			}
			for (u8 Layer = 0; Layer <= Item.Actor.Layers.size(); ++Layer)
			{
				const xr_vector<ActorBlock>& Row = BlocksOf(Item.Actor, Layer);
				for (u32 Index = 0; Index < Row.size(); ++Index)
				{
					ImGui::PushID(static_cast<s32>(4096 + Layer * 256 + Index));
					if (!IsClipBlock(Row[Index].Source) && ImGui::Selectable(Row[Index].Name.c_str()))
					{
						Chosen = Item.Id;
						ChosenLayer = Layer;
						ChosenBlock = static_cast<s32>(Index);
					}
					ImGui::PopID();
				}
			}
			ImGui::Unindent();
			ImGui::PopID();
		}
		if (HasActors)
		{
			ImGui::EndChild();
		}
		if (ChosenClip >= 0)
		{
			OpenAnimator(Core, Chosen, ChosenClip, false);
		}
		else if (Chosen)
		{
			if (!IsSelected(Chosen))
			{
				Select(Chosen, false, false);
			}
			SelectedBones.clear();
			AnimObject = PoseObject = Chosen;
			AnimClipIndex = -1;
			AnimStockLayer = ChosenLayer;
			AnimStockBlock = ChosenBlock;
			IsAnimFresh = false;
		}
		else
		{
			DrawActorPicker(Core);
		}
		return;
	}
	const ActorRuntime* Runtime = Core.FindActor(AnimObject);
	const xr_vector<AnimClip>& Clips = Target->Actor.Clips;
	const auto BlockAt = [Target](u8 Layer, s32 Index)
	{
		const xr_vector<ActorBlock>* Row = Index >= 0 && Layer <= Target->Actor.Layers.size() ? &BlocksOf(Target->Actor, Layer) : nullptr;
		return Row && Index < static_cast<s32>(Row->size()) ? &(*Row)[Index] : nullptr;
	};
	if (IsAnimFresh)
	{
		IsAnimFresh = false;
		const ActorBlock* Pressed = Focus == FocusKind::Block && FocusObject == AnimObject ? BlockAt(FocusLayer, FocusBlock) : nullptr;
		if (Pressed)
		{
			const bool IsOwn = IsClipBlock(Pressed->Source);
			AnimClipIndex = IsOwn ? FindClip(Clips, Pressed->Name) : -1;
			AnimStockLayer = FocusLayer;
			AnimStockBlock = IsOwn ? -1 : FocusBlock;
		}
	}
	const ActorBlock* Found = AnimClipIndex < 0 ? BlockAt(AnimStockLayer, AnimStockBlock) : nullptr;
	const ActorBlock* Stock = Found && !IsClipBlock(Found->Source) ? Found : nullptr;
	AnimStockBlock = Stock ? AnimStockBlock : -1;
	AnimClipIndex = AnimClipIndex < 0 && !Stock && !Clips.empty() ? 0 : AnimClipIndex;
	const bool HasClip = AnimClipIndex >= 0;
	AnimLayer = HasClip ? static_cast<u8>(std::min<size_t>(AnimLayer, Clips[AnimClipIndex].Layers.size())) : 0;
	const bool IsHands = Target->Actor.Hud.size() && !Target->Attach.Parent.ObjectId && pSettings->section_exist(Target->Actor.Hud);
	s32 PickedClip = -1;
	shared_str PickedAlias;
	ImGui::SetNextItemWidth(ImGui::GetFontSize() * 12.0f);
	if (ImGui::BeginCombo("##Clip", HasClip ? Clips[AnimClipIndex].Name.c_str() : Stock ? Stock->Name.c_str() : "Pick an animation", ImGuiComboFlags_HeightLarge))
	{
		for (u32 Index = 0; Index < Clips.size(); ++Index)
		{
			ImGui::PushID(static_cast<s32>(Index));
			PickedClip = ImGui::Selectable(Clips[Index].Name.c_str(), static_cast<s32>(Index) == AnimClipIndex) ? static_cast<s32>(Index) : PickedClip;
			ImGui::PopID();
		}
		if (IsHands)
		{
			ImGui::SeparatorText("Weapon animations");
			for (const auto& Line : pSettings->r_section(Target->Actor.Hud).Data)
			{
				const char* Alias = Line.first.c_str();
				if (strstr(Alias, "anm_") == Alias && strstr(Alias, "anm_bp_") != Alias)
				{
					PickedAlias = ImGui::Selectable(Alias) ? Line.first : PickedAlias;
					ImGui::SetItemTooltip("%s", Line.second.c_str());
				}
			}
		}
		ImGui::EndCombo();
	}
	ImGui::SetItemTooltip(IsHands ? "The clip to edit, or a weapon animation to place at the playhead" : "The clip to edit");
	if (PickedAlias.size())
	{
		AddHudMotion(Core, AnimObject, PickedAlias.c_str());
		AnimClipIndex = -1;
		IsAnimFresh = true;
		return;
	}
	if (PickedClip >= 0)
	{
		AnimClipIndex = PickedClip;
		AnimLayer = 0;
		AnimStockBlock = -1;
		AnimKeys.clear();
		return;
	}
	if (HasClip)
	{
		const AnimClip& Clip = Clips[AnimClipIndex];
		s32 PickedLayer = -1;
		ImGui::SameLine();
		ImGui::SetNextItemWidth(ImGui::GetFontSize() * 7.0f);
		if (ImGui::BeginCombo("##Layer", AnimLayer ? Clip.Layers[AnimLayer - 1].Name.c_str() : "Base layer"))
		{
			PickedLayer = ImGui::Selectable("Base layer", AnimLayer == 0) ? 0 : PickedLayer;
			for (u32 Index = 0; Index < Clip.Layers.size(); ++Index)
			{
				ImGui::PushID(static_cast<s32>(Index));
				PickedLayer = ImGui::Selectable(Clip.Layers[Index].Name.c_str(), AnimLayer == Index + 1) ? static_cast<s32>(Index + 1) : PickedLayer;
				ImGui::PopID();
			}
			ImGui::Separator();
			PickedLayer = Clip.Layers.size() < MaxClipLayers && ImGui::Selectable("New layer") ? static_cast<s32>(MaxClipLayers + 1) : PickedLayer;
			ImGui::EndCombo();
		}
		ImGui::SetItemTooltip("The layer that posing and the timeline edit. Layers add on top of the base layer (or replace it) with their own weight");
		if (PickedLayer > static_cast<s32>(MaxClipLayers))
		{
			xr_vector<ClipLayer>& Layers = Edit(Core, AnimObject, "New animation layer")->Actor.Clips[AnimClipIndex].Layers;
			ClipLayer& Made = Layers.emplace_back();
			string32 Label;
			xr_sprintf(Label, "layer_%u", static_cast<u32>(Layers.size()));
			Made.Name = Label;
			Made.Mode = RowMode::Add;
			AnimLayer = static_cast<u8>(Layers.size());
			AnimKeys.clear();
			return;
		}
		if (PickedLayer >= 0)
		{
			AnimLayer = static_cast<u8>(PickedLayer);
			AnimKeys.clear();
			return;
		}
		ImGui::SameLine();
		ImGui::BeginDisabled(SelectedBones.empty() || PoseObject != AnimObject);
		if (ImGui::Button("Key"))
		{
			KeyBones(Core);
		}
		ImGui::SetItemTooltip("Key the selected bones (F)");
		ImGui::EndDisabled();
		ImGui::SameLine();
		ImGui::Checkbox("Link", &IsAnimLinked);
		ImGui::SetItemTooltip("Follow the scene playhead while it is on a block that plays this clip");
		ImGui::SameLine();
		if (ImGui::Button("Save") && SaveClip(Core, AnimObject, static_cast<u32>(AnimClipIndex)))
		{
			Edit(Core, AnimObject, "Link animation clip")->Actor.Clips[AnimClipIndex].IsLinked = true;
			SetStatus(false, "Saved the animation %s to the library.", Clip.Name.c_str());
		}
		ImGui::SetItemTooltip("Save to the library (gamedata\\meshes\\cinema\\<name>.omf) and link the clip to it");
	}
	ImGui::SameLine();
	if (ImGui::Button("Clip"))
	{
		ImGui::OpenPopup("##ClipMenu");
	}
	ImGui::SetItemTooltip("New, open, rename, length, mode, retarget, delete");
	u8 Act = 0;
	shared_str Opened;
	if (ImGui::BeginPopup("##ClipMenu"))
	{
		Act = ImGui::MenuItem("New clip") ? 1 : Act;
		if (ImGui::BeginMenu("Open from library"))
		{
			if (ImGui::IsWindowAppearing())
			{
				FS_FileSet Listed;
				FS.file_list(Listed, _game_meshes_, FS_ListFiles, "*.omf");
				ClipFiles.clear();
				for (const FS_File& File : Listed)
				{
					xr_string Path = File.name;
					std::replace(Path.begin(), Path.end(), '/', '\\');
					const size_t Folder = xr_strlen(Omf::ClipFolder);
					const xr_string Name = Path.size() > Folder + 4 && !Path.compare(0, Folder, Omf::ClipFolder) ? Path.substr(Folder, Path.size() - Folder - 4) : xr_string();
					if (Omf::IsClipName(Name.c_str()))
					{
						ClipFiles.emplace_back(Name.c_str());
					}
				}
			}
			for (const shared_str& Name : ClipFiles)
			{
				Opened = ImGui::MenuItem(Name.c_str()) ? Name : Opened;
			}
			if (ClipFiles.empty())
			{
				ImGui::TextDisabled("The library is empty.");
			}
			ImGui::EndMenu();
		}
		if (HasClip)
		{
			const AnimClip& Clip = Clips[AnimClipIndex];
			ImGui::Separator();
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * 10.0f);
			ImGui::InputText("Name", AnimName, sizeof(AnimName), ImGuiInputTextFlags_CharsNoBlank);
			if (ImGui::IsItemDeactivatedAfterEdit() && AnimName[0] && !strchr(AnimName, ',') && FindClip(Clips, AnimName) < 0)
			{
				ActorData& Setup = Edit(Core, AnimObject, "Rename animation clip")->Actor;
				const shared_str Old = Clip.Name;
				for (u8 Layer = 0; Layer <= Setup.Layers.size(); ++Layer)
				{
					for (ActorBlock& Block : BlocksOf(Setup, Layer))
					{
						Block.Name = IsClipBlock(Block.Source) && Block.Name == Old ? shared_str(AnimName) : Block.Name;
					}
				}
				Setup.Clips[AnimClipIndex].Name = AnimName;
			}
			else if (!ImGui::IsItemActive())
			{
				xr_strcpy(AnimName, Clip.Name.c_str());
			}
			float Length = Clip.Length;
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * 10.0f);
			if (ImGui::DragFloat("Length", &Length, 0.01f, MinLength, 600.0f, "%.2f s", ImGuiSliderFlags_AlwaysClamp))
			{
				Object& Item = *Edit(Core, AnimObject, "Animation clip length");
				Item.Actor.Clips[AnimClipIndex].Length = Length;
				Refit(Core, Item);
			}
			ImGui::SetNextItemWidth(ImGui::GetFontSize() * 10.0f);
			ImGui::DragFloat("Fit tolerance", &AnimTolerance, 0.01f, 0.01f, 10.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp);
			ImGui::SetItemTooltip("Used by Fit curve, Make editable and Retarget: degrees for turns, centimetres for moves");
			Act = ImGui::MenuItem("Override the animation below", nullptr, Clip.Mode == RowMode::Override) ? 2 : Act;
			ImGui::SetItemTooltip("Ticked: the clip replaces the animation below on its bones. Unticked: it turns and moves them on top of it");
			Act = Clip.IsLinked && ImGui::MenuItem("Linked to the library", nullptr, true) ? 3 : Act;
			bool IsUsed = false;
			for (u8 Layer = 0; Layer <= Target->Actor.Layers.size(); ++Layer)
			{
				for (const ActorBlock& Block : BlocksOf(Target->Actor, Layer))
				{
					IsUsed |= IsClipBlock(Block.Source) && Block.Name == Clip.Name;
				}
			}
			Act = !IsUsed && ImGui::MenuItem("Add to timeline", nullptr, false, Target->Actor.Layers.size() < MaxActorLayers && !Target->Actor.IsRelative) ? 4 : Act;
			Act = ImGui::MenuItem("Retarget...") ? 5 : Act;
			Act = ImGui::MenuItem("Delete clip") ? 6 : Act;
			if (AnimLayer)
			{
				const ClipLayer& Layer = Clip.Layers[AnimLayer - 1];
				float Weight = Layer.Weight;
				ImGui::SeparatorText(Layer.Name.c_str());
				ImGui::SetNextItemWidth(ImGui::GetFontSize() * 10.0f);
				if (ImGui::SliderFloat("Layer weight", &Weight, 0.0f, 1.0f, "%.2f", ImGuiSliderFlags_AlwaysClamp))
				{
					Edit(Core, AnimObject, "Animation layer weight")->Actor.Clips[AnimClipIndex].Layers[AnimLayer - 1].Weight = Weight;
				}
				Act = ImGui::MenuItem("Layer replaces the layers below", nullptr, Layer.Mode == RowMode::Override) ? 7 : Act;
				Act = ImGui::MenuItem("Mute layer", nullptr, Layer.IsMuted) ? 8 : Act;
				Act = ImGui::MenuItem("Delete layer") ? 9 : Act;
			}
		}
		ImGui::Separator();
		ImGui::MenuItem("Graph Editor", nullptr, &Settings.ShowGraph);
		if (IsHands)
		{
			ImGui::MenuItem("Camera rig", nullptr, &Engine.External.EditorStates[static_cast<u8>(EditorUI::Game_CameraRig)]);
			ImGui::SetItemTooltip("The game's Camera Rig window: sway, inertia, hand IK share and blends of the first-person view, with per-weapon values");
		}
		ImGui::EndPopup();
	}
	if (Opened.size())
	{
		OpenClip(Core, AnimObject, Opened.c_str());
		return;
	}
	if (Act == 1)
	{
		NewClip(Core, AnimObject);
		return;
	}
	if (Act == 2)
	{
		AnimClip& Edited = Edit(Core, AnimObject, "Animation clip mode")->Actor.Clips[AnimClipIndex];
		Edited.Mode = Edited.Mode == RowMode::Add ? RowMode::Override : RowMode::Add;
	}
	if (Act == 3)
	{
		Edit(Core, AnimObject, "Make animation clip unique")->Actor.Clips[AnimClipIndex].IsLinked = false;
	}
	if (Act == 4)
	{
		const shared_str Name = Clips[AnimClipIndex].Name;
		PlaceClip(Edit(Core, AnimObject, "Add animation clip to timeline")->Actor, Name, Runtime);
	}
	if (Act == 5)
	{
		ShowRetarget = true;
		RetargetFrom = AnimObject;
		RetargetClipIndex = AnimClipIndex;
		RetargetMap.clear();
	}
	if (Act >= 7 && AnimLayer)
	{
		xr_vector<ClipLayer>& Layers = Edit(Core, AnimObject, "Animation layer")->Actor.Clips[AnimClipIndex].Layers;
		ClipLayer& Layer = Layers[AnimLayer - 1];
		if (Act == 7)
		{
			Layer.Mode = Layer.Mode == RowMode::Add ? RowMode::Override : RowMode::Add;
		}
		if (Act == 8)
		{
			Layer.IsMuted = !Layer.IsMuted;
		}
		if (Act == 9)
		{
			Layers.erase(Layers.begin() + (AnimLayer - 1));
			AnimLayer = 0;
			AnimKeys.clear();
			return;
		}
	}
	if (Act == 6)
	{
		Edit(Core, AnimObject, "Delete animation clip")->Actor.Clips.erase(Clips.begin() + AnimClipIndex);
		AnimClipIndex = -1;
		AnimKeys.clear();
		SetStatus(false, "The clip is deleted; blocks that played it no longer resolve.");
		return;
	}
	if (Stock)
	{
		const float Start = Target->Span.Start + Stock->Start, Duration = Runtime ? Runtime->BlockDuration(*Stock) : 0.0f;
		float Local = std::clamp(Core.Time - Start, 0.0f, std::max(Duration, 0.0f));
		ImGui::SameLine();
		if (ImGui::Button("Make editable"))
		{
			ImportClip(Core, AnimObject, AnimStockLayer, static_cast<u32>(AnimStockBlock));
			return;
		}
		ImGui::SetItemTooltip("Turns it into a clip with keys on every bone; its block then plays that clip. One undo brings the stock animation back");
		ImGui::SameLine();
		ImGui::TextDisabled("%.2f / %.2f s", Local, Duration);
		DrawAnimatorKeys(Core, *Target, -1, Stock);
		return;
	}
	if (!HasClip)
	{
		ImGui::TextDisabled(IsHands ? "Pick a weapon animation in the list above, or Clip -> New clip." : "This actor has no animation clip yet: double-click one of its animation blocks in Scene, or Clip -> New clip.");
		return;
	}
	const char* State = IsAnimFollowing ? "" : IsAnimLinked ? "  |  the scene playhead is off this clip: shown on its own" : "  |  shown on its own";
	ImGui::SameLine();
	ImGui::TextDisabled("%.2f / %.2f s%s", AnimTime, Clips[AnimClipIndex].Length, State);
	DrawAnimatorKeys(Core, *Target, ClipBase + static_cast<s32>(AnimLayer) * ClipSpan + AnimClipIndex, nullptr);
}

void Editor::DrawAnimatorKeys(Player& Core, const Object& Target, s32 Block, const ActorBlock* Stock)
{
	constexpr Param Kinds[3] = { Param::BoneRotation, Param::BonePosition, Param::BoneScale };
	constexpr const char* KindNames[3] = { "Rotation", "Position", "Scale" };
	constexpr const char* ChannelNames[9] = { "Rotation X", "Rotation Y", "Rotation Z", "Position X", "Position Y", "Position Z", "Scale X", "Scale Y", "Scale Z" };
	constexpr u32 AxisColors[3] = { IM_COL32(230, 80, 80, 255), IM_COL32(90, 200, 90, 255), IM_COL32(90, 140, 255, 255) };
	const AnimClip* Clip = Block >= ClipBase ? &Target.Actor.Clips[ClipIndex(Block)] : nullptr;
	const ClipTrack* Track = Clip ? &TrackOf(Target, Block) : nullptr;
	const ActorRuntime* Runtime = Core.FindActor(Target.Id);
	const ImGuiIO& Input = ImGui::GetIO();
	const float Rate = static_cast<float>(Core.Data.Header.FrameRate);
	const float Begin = Stock ? Target.Span.Start + Stock->Start : 0.0f;
	const float Length = std::max(Clip ? Clip->Length : Runtime && Stock ? Runtime->BlockDuration(*Stock) : 0.0f, MinLength);
	const float Now = Clip ? AnimTime : std::clamp(Core.Time - Begin, 0.0f, Length);
	const auto Seek = [&](float Time) { Clip ? SeekAnimator(Core, Target, Time) : Core.Seek(Begin + std::clamp(Time, 0.0f, Length)); };
	const u32 Bones = Clip ? static_cast<u32>(Track->Bones.size()) : Runtime ? Runtime->BoneCount() : 0;
	const auto NameOf = [&](u32 Bone) { return Clip ? Track->Bones[Bone].Name.c_str() : Runtime->BoneName(static_cast<u16>(Bone)); };
	const auto JointOf = [&](u32 Bone) { return !Clip ? static_cast<u16>(Bone) : Runtime ? Runtime->FindBone(Track->Bones[Bone].Name) : BI_NONE; };
	const auto IsChosen = [&](u32 Bone)
	{
		const u16 Joint = JointOf(Bone);
		return Joint != BI_NONE && PoseObject == Target.Id && std::find(SelectedBones.begin(), SelectedBones.end(), Joint) != SelectedBones.end();
	};
	const float Row = ImGui::GetFrameHeight(), Names = ImGui::GetFontSize() * 13.0f, Pad = ImGui::GetFontSize() * 0.6f, Half = Row * 0.26f;
	const ImVec2 Top = ImGui::GetCursorScreenPos();
	const float Width = std::max(ImGui::GetContentRegionAvail().x - Names, 64.0f), Left = Top.x + Names;
	AnimZoom = std::clamp(AnimZoom, 1.0f, 64.0f);
	AnimScroll = std::clamp(AnimScroll, 0.0f, Length - Length / AnimZoom);
	AnimLaneKind = std::min<u8>(AnimLaneKind, 2);
	const float Scale = (Width - 2.0f * Pad) / Length * AnimZoom;
	const auto XAt = [&](float Time) { return Left + Pad + (Time - AnimScroll) * Scale; };
	const auto RawAt = [&](float X) { return (X - Left - Pad) / Scale + AnimScroll; };
	const auto TimeAt = [&](float X) { return std::round(RawAt(X) * Rate) / Rate; };
	AnimSnaps.clear();
	if (Track)
	{
		AnimatorUi::KeyTimes(*Track, AnimatorUi::AllBones, AnimSnaps);
	}
	const auto Pick = [&](float X)
	{
		const float Raw = RawAt(X);
		const auto Next = std::lower_bound(AnimSnaps.begin(), AnimSnaps.end(), Raw);
		float Best = TimeAt(X), Gap = 5.0f / Scale;
		for (auto Near : { Next, Next == AnimSnaps.begin() ? Next : Next - 1 })
		{
			if (Near != AnimSnaps.end() && std::fabs(*Near - Raw) <= Gap)
			{
				Gap = std::fabs(*Near - Raw);
				Best = *Near;
			}
		}
		return Best;
	};
	const u32 HeadColor = IM_COL32(255, 90, 90, 255), PickColor = IM_COL32(255, 200, 60, 255), Faint = ImGui::GetColorU32(ImGuiCol_TextDisabled);
	ImDrawList* List = ImGui::GetWindowDrawList();
	ImGui::SetCursorScreenPos(ImVec2(Left, Top.y));
	ImGui::InvisibleButton("##Ruler", ImVec2(Width, Row));
	if (ImGui::IsItemActive())
	{
		Seek(Pick(Input.MousePos.x));
	}
	const float* Step = std::find_if(std::begin(AnimatorUi::Steps), std::end(AnimatorUi::Steps) - 1, [&](float Item) { return Item * Scale >= ImGui::GetFontSize() * 4.0f; });
	List->PushClipRect(ImVec2(Left, Top.y), ImVec2(Left + Width, Top.y + Row), true);
	List->AddRectFilled(ImVec2(Left, Top.y), ImVec2(Left + Width, Top.y + Row), ImGui::GetColorU32(ImGuiCol_FrameBg));
	for (float Tick = std::floor(AnimScroll / *Step) * *Step; Tick <= Length + AnimatorUi::SameTime && XAt(Tick) < Left + Width; Tick += *Step)
	{
		string16 Label;
		xr_sprintf(Label, "%.2f", Tick);
		List->AddLine(ImVec2(XAt(Tick), Top.y + Row * 0.6f), ImVec2(XAt(Tick), Top.y + Row), Faint);
		List->AddText(ImVec2(XAt(Tick) + 3.0f, Top.y), Faint, Label);
	}
	List->AddLine(ImVec2(XAt(Length), Top.y), ImVec2(XAt(Length), Top.y + Row), ImGui::GetColorU32(ImGuiCol_Text));
	List->AddTriangleFilled(ImVec2(XAt(Now) - Half, Top.y), ImVec2(XAt(Now) + Half, Top.y), ImVec2(XAt(Now), Top.y + Row * 0.6f), HeadColor);
	List->PopClipRect();
	AnimOpen.resize(Bones, 0);
	AnimRows.clear();
	AnimRows.push_back({});
	for (u32 Bone = 0; Bone < Bones; ++Bone)
	{
		AnimRows.push_back({ static_cast<u16>(Bone), -1 });
		for (s8 Channel = 0; AnimOpen[Bone] && Channel < 9; ++Channel)
		{
			AnimRows.push_back({ static_cast<u16>(Bone), Channel });
		}
	}
	const u32 Count = static_cast<u32>(AnimRows.size());
	const float Rest = ImGui::GetContentRegionAvail().y - Row;
	const float TreeTall = Clip ? std::max(std::floor(Rest * 0.5f), Row * 3.0f) : 0.0f;
	ImGui::SetCursorScreenPos(ImVec2(Top.x, Top.y + Row));
	ImGui::BeginChild("##AnimRows", ImVec2(0.0f, TreeTall), ImGuiChildFlags_None, ImGuiWindowFlags_NoScrollWithMouse);
	const ImVec2 Origin = ImGui::GetCursorScreenPos();
	ImGui::InvisibleButton("##Rows", ImVec2(Names + Width, Row * static_cast<float>(Count)), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);
	const bool IsHovered = ImGui::IsItemHovered(), IsActive = ImGui::IsItemActive();
	const s32 Over = IsHovered ? std::clamp(static_cast<s32>((Input.MousePos.y - Origin.y) / Row), 0, static_cast<s32>(Count) - 1) : -1;
	float HitTime = -1.0f;
	const bool IsBoxed = Clip && IsAnimBoxing && IsActive && Input.MouseDragMaxDistanceSqr[ImGuiMouseButton_Left] >= 16.0f;
	const ImRect Box(ImMin(AnimBoxStart, Input.MousePos), ImMax(AnimBoxStart, Input.MousePos));
	AnimBoxKeys.clear();
	List = ImGui::GetWindowDrawList();
	for (u32 Index = 0; Index < Count; ++Index)
	{
		const float Y = Origin.y + Row * static_cast<float>(Index);
		if (!ImGui::IsRectVisible(ImVec2(Top.x, Y), ImVec2(Left + Width, Y + Row)))
		{
			continue;
		}
		const AnimRow Line = AnimRows[Index];
		const bool IsBone = Line.Bone != u16(-1), IsLeaf = Line.Channel >= 0, IsLit = IsBone && IsChosen(Line.Bone);
		const u8 Layer = IsBone ? static_cast<u8>(Line.Bone) : AnimatorUi::AllBones;
		if (IsLit || (Index & 1))
		{
			List->AddRectFilled(ImVec2(Top.x, Y), ImVec2(Left + Width, Y + Row), ImGui::GetColorU32(IsLit ? ImGuiCol_Header : ImGuiCol_TableRowBgAlt, IsLit && IsLeaf ? 0.5f : 1.0f));
		}
		const float Inset = IsLeaf ? Row * 1.6f : IsBone ? Row : 4.0f, Mid = Y + Row * 0.5f;
		if (IsBone && !IsLeaf)
		{
			const float Edge = Top.x + Row * 0.5f, Size = Row * 0.18f;
			AnimOpen[Line.Bone] ? List->AddTriangleFilled(ImVec2(Edge - Size, Mid - Size * 0.6f), ImVec2(Edge + Size, Mid - Size * 0.6f), ImVec2(Edge, Mid + Size), Faint) : List->AddTriangleFilled(ImVec2(Edge - Size * 0.6f, Mid - Size), ImVec2(Edge + Size, Mid), ImVec2(Edge - Size * 0.6f, Mid + Size), Faint);
		}
		const bool IsSolo = IsLeaf && IsLit && AnimLaneKind == Line.Channel / 3 && (AnimSolo < 0 || AnimSolo == Line.Channel % 3);
		const u32 Ink = IsLeaf ? (IsSolo ? AxisColors[Line.Channel % 3] : Faint) : !Clip || (IsBone && JointOf(Line.Bone) == BI_NONE) ? Faint : ImGui::GetColorU32(ImGuiCol_Text);
		List->PushClipRect(ImVec2(Top.x, Y), ImVec2(Left - 4.0f, Y + Row), true);
		List->AddText(ImVec2(Top.x + Inset, Y + ImGui::GetStyle().FramePadding.y), Ink, IsLeaf ? ChannelNames[Line.Channel] : IsBone ? NameOf(Line.Bone) : "Summary");
		List->PopClipRect();
		if (!Clip)
		{
			continue;
		}
		AnimTimes.clear();
		const auto& Lanes = Track->Lanes;
		const Lane* Track = IsLeaf ? Curves::FindLane(Lanes, Kinds[Line.Channel / 3], Layer) : nullptr;
		for (u32 At = 0; Track && At < Track->Curves[Line.Channel % 3].Keys.size(); ++At)
		{
			AnimTimes.push_back(Track->Curves[Line.Channel % 3].Keys[At].Time);
		}
		if (!IsLeaf)
		{
			AnimatorUi::KeyTimes(*Clip, Layer, AnimTimes);
		}
		List->PushClipRect(ImVec2(Left, Y), ImVec2(Left + Width, Y + Row), true);
		float Column = -FLT_MAX;
		u32 Stack = 0;
		for (float Time : AnimTimes)
		{
			const float X = XAt(Time);
			if (X < Left - Half || X > Left + Width + Half)
			{
				continue;
			}
			Stack = X - Column < Half * 1.5f ? (Stack + 1) % 3 : 0;
			Column = Stack ? Column : X;
			const float Size = (IsLeaf ? Half * 0.8f : Half) * (Stack ? 0.7f : 1.0f), High = Mid + (Stack == 1 ? -0.3f : Stack == 2 ? 0.3f : 0.0f) * Row;
			const bool IsPicked = std::any_of(AnimKeys.begin(), AnimKeys.end(), [&](const AnimKey& Item) { return (Item.Bone == AnimatorUi::AllBones || Item.Bone == Layer) && (Item.Channel < 0 || Item.Channel == Line.Channel) && std::fabs(Item.Time - Time) < AnimatorUi::SameTime; });
			List->AddQuadFilled(ImVec2(X - Size, High), ImVec2(X, High - Size), ImVec2(X + Size, High), ImVec2(X, High + Size), IsPicked ? PickColor : IsLeaf ? AxisColors[Line.Channel % 3] : ImGui::GetColorU32(ImGuiCol_Text));
			const bool IsNear = Over == static_cast<s32>(Index) && std::fabs(Input.MousePos.x - X) <= Half + 2.0f;
			HitTime = IsNear && (HitTime < 0.0f || std::fabs(Input.MousePos.x - X) < std::fabs(Input.MousePos.x - XAt(HitTime))) ? Time : HitTime;
			if (IsBoxed && X >= Box.Min.x && X <= Box.Max.x && Mid >= Box.Min.y && Mid <= Box.Max.y)
			{
				AnimBoxKeys.push_back({ Layer, Time, Line.Channel });
			}
		}
		List->PopClipRect();
	}
	List->PushClipRect(ImVec2(Left, Origin.y), ImVec2(Left + Width, Origin.y + ImGui::GetWindowHeight() + ImGui::GetScrollMaxY()), true);
	List->AddLine(ImVec2(XAt(Now), Origin.y), ImVec2(XAt(Now), Origin.y + std::max(Row * static_cast<float>(Count), ImGui::GetWindowHeight())), HeadColor);
	if (IsBoxed)
	{
		List->AddRectFilled(Box.Min, Box.Max, ImGui::GetColorU32(ImGuiCol_Header, 0.4f));
		List->AddRect(Box.Min, Box.Max, ImGui::GetColorU32(ImGuiCol_Text, 0.6f));
		AnimKeys = AnimBaseKeys;
		for (const AnimKey& Item : AnimBoxKeys)
		{
			if (std::find(AnimKeys.begin(), AnimKeys.end(), Item) == AnimKeys.end())
			{
				AnimKeys.push_back(Item);
			}
		}
	}
	List->PopClipRect();
	IsAnimBoxing &= ImGui::IsMouseDown(ImGuiMouseButton_Left);
	const AnimRow Under = Over >= 0 ? AnimRows[Over] : AnimRow {};
	const bool HasBone = Under.Bone != u16(-1), IsOnKey = HitTime >= 0.0f, IsMenu = IsHovered && ImGui::IsMouseClicked(ImGuiMouseButton_Right), IsAdding = Input.KeyCtrl || Input.KeyShift;
	const bool IsLabel = Input.MousePos.x < Left;
	const AnimKey Hit { HasBone ? static_cast<u8>(Under.Bone) : AnimatorUi::AllBones, HitTime, Under.Channel };
	if (IsHovered && (IsMenu || ImGui::IsMouseClicked(ImGuiMouseButton_Left)))
	{
		const auto Found = std::find_if(AnimKeys.begin(), AnimKeys.end(), [&Hit](const AnimKey& Item) { return Item.Bone == Hit.Bone && Item.Channel == Hit.Channel && std::fabs(Item.Time - Hit.Time) < AnimatorUi::SameTime; });
		const bool IsHeld = Found != AnimKeys.end();
		if (IsOnKey && IsHeld && Input.KeyCtrl && !IsMenu)
		{
			AnimKeys.erase(Found);
		}
		else if (IsOnKey && !IsHeld)
		{
			IsAdding ? (void)0 : AnimKeys.clear();
			AnimKeys.push_back(Hit);
		}
		else if (!IsOnKey && !IsAdding && !IsMenu)
		{
			AnimKeys.clear();
		}
		const bool IsFold = HasBone && Under.Channel < 0 && IsLabel && Input.MousePos.x < Top.x + Row && !IsMenu;
		const u16 Joint = HasBone && !IsFold && PoseObject == Target.Id ? JointOf(Under.Bone) : BI_NONE;
		if (IsFold)
		{
			AnimOpen[Under.Bone] = !AnimOpen[Under.Bone];
		}
		if (Joint != BI_NONE && !IsAdding)
		{
			SelectedBones.assign(1, Joint);
		}
		else if (Joint != BI_NONE && std::find(SelectedBones.begin(), SelectedBones.end(), Joint) == SelectedBones.end())
		{
			SelectedBones.push_back(Joint);
		}
		if (Joint != BI_NONE && IsLabel)
		{
			AnimLaneKind = Under.Channel >= 0 ? static_cast<u8>(Under.Channel / 3) : AnimLaneKind;
			AnimSolo = Under.Channel >= 0 ? static_cast<s8>(Under.Channel % 3) : s8(-1);
		}
		if (IsMenu && Clip)
		{
			AnimMenu = Hit;
			ImGui::OpenPopup("##AnimKeyMenu");
		}
		else if (!IsMenu)
		{
			IsAnimDragging = Clip && IsOnKey && !Input.KeyCtrl;
			AnimGrab = IsOnKey ? TimeAt(Input.MousePos.x) : Pick(Input.MousePos.x);
			AnimShift = 0.0f;
			AnimBaseKeys = AnimKeys;
			if (IsAnimDragging)
			{
				AnimBase = Track->Lanes;
			}
			IsAnimBoxing = Clip && !IsOnKey && !IsLabel;
			AnimBoxStart = Input.MousePos;
			if (!IsAdding && !IsLabel)
			{
				Seek(IsOnKey ? HitTime : AnimGrab);
			}
		}
	}
	const bool IsDragging = IsActive && ImGui::IsMouseDragging(ImGuiMouseButton_Left);
	if (Clip && IsHovered && ImGui::IsMouseDoubleClicked(ImGuiMouseButton_Left) && !IsOnKey && HasBone && !IsLabel && PoseObject == Target.Id && !SelectedBones.empty())
	{
		IsAnimDragging = false;
		KeyBones(Core);
	}
	else if (Clip && IsDragging && IsAnimDragging && !AnimBaseKeys.empty())
	{
		float First = FLT_MAX, Last = -FLT_MAX;
		for (const AnimKey& Item : AnimBaseKeys)
		{
			First = std::min(First, Item.Time);
			Last = std::max(Last, Item.Time);
		}
		const float Shift = std::clamp(TimeAt(Input.MousePos.x) - AnimGrab, -First, std::max(Clip->Length - Last, -First));
		if (std::fabs(Shift - AnimShift) > AnimatorUi::SameTime)
		{
			AnimShift = Shift;
			MoveAnimKeys(Core, Shift);
		}
	}
	else if (!Clip && IsDragging && !IsLabel)
	{
		Seek(Pick(Input.MousePos.x));
	}
	IsAnimDragging &= ImGui::IsMouseDown(ImGuiMouseButton_Left);
	bool IsStale = false;
	if (ImGui::BeginPopup("##AnimKeyMenu"))
	{
		const bool HasKeys = !AnimKeys.empty();
		if (ImGui::BeginMenu("Curve", HasKeys))
		{
			for (u32 Mode = 0; Mode < 6; ++Mode)
			{
				if (ImGui::MenuItem(AnimatorUi::Modes[Mode]))
				{
					SetAnimMode(Core, static_cast<Interp>(Mode));
				}
			}
			ImGui::EndMenu();
		}
		if (ImGui::MenuItem("Fit curve", nullptr, false, AnimKeys.size() >= 2))
		{
			FitAnimKeys(Core);
		}
		ImGui::SetItemTooltip("Replaces the keys from the first to the last selected one with as few Bezier keys as keep the motion within the fit tolerance (Clip menu)");
		if (ImGui::MenuItem("Delete keys", "Del", false, HasKeys))
		{
			DeleteAnimKeys(Core);
		}
		if (AnimMenu.Bone != AnimatorUi::AllBones && ImGui::MenuItem("Remove bone from clip"))
		{
			RemoveAnimBone(Core, AnimMenu.Bone);
			AnimOpen.clear();
			IsStale = true;
		}
		ImGui::EndPopup();
	}
	const auto Wheel = [&]()
	{
		const float Offset = Input.MousePos.x - Left - Pad;
		if (Input.KeyCtrl)
		{
			AnimScroll -= Input.MouseWheel * Length / AnimZoom * 0.1f;
		}
		else if (!Input.KeyShift && Input.MousePos.x >= Left)
		{
			const float Pivot = Offset / Scale + AnimScroll;
			AnimZoom = std::clamp(AnimZoom * (Input.MouseWheel > 0.0f ? 1.25f : 0.8f), 1.0f, 64.0f);
			AnimScroll = Pivot - Offset / ((Width - 2.0f * Pad) / Length * AnimZoom);
		}
	};
	if (ImGui::IsWindowHovered() && Input.MouseWheel != 0.0f)
	{
		Wheel();
		if (!Input.KeyCtrl && (Input.KeyShift || IsLabel))
		{
			ImGui::SetScrollY(ImGui::GetScrollY() - Input.MouseWheel * Row * 3.0f);
		}
	}
	ImGui::EndChild();
	const ImVec2 Pane = ImGui::GetCursorScreenPos();
	const float Tall = ImGui::GetContentRegionAvail().y;
	if (IsStale || !Clip || Tall < Row * 3.0f || static_cast<s32>(ClipIndex(Block)) != AnimClipIndex)
	{
		AnimCurveKey = -1;
		AnimTipSide = 0;
		return;
	}
	for (u8 Choice = 0; Choice < 3; ++Choice)
	{
		ImGui::SetCursorScreenPos(ImVec2(Top.x + 4.0f, Pane.y + 2.0f + Row * static_cast<float>(Choice)));
		if (ImGui::RadioButton(KindNames[Choice], AnimLaneKind == Choice))
		{
			AnimLaneKind = Choice;
			AnimSolo = -1;
		}
	}
	ImGui::SetCursorScreenPos(ImVec2(Left, Pane.y));
	ImGui::InvisibleButton("##Curves", ImVec2(Width, Tall));
	const bool IsOver = ImGui::IsItemHovered(), IsPressed = ImGui::IsItemActive();
	const Param Kind = Kinds[AnimLaneKind];
	const bool IsFrozen = AnimCurveKey >= 0 || AnimTipSide;
	float Lo = FLT_MAX, Hi = -FLT_MAX;
	u32 Shown = 0;
	for (u32 Bone = 0; Bone < Bones; ++Bone)
	{
		if (!IsChosen(Bone))
		{
			continue;
		}
		++Shown;
		const PoseBone& Entry = Track->Bones[Bone];
		const auto& Lanes = Track->Lanes;
		const Fvector& Still = AnimLaneKind == 0 ? Entry.Rotation : AnimLaneKind == 1 ? Entry.Offset : Entry.Scale;
		const Lane* Track = Curves::FindLane(Lanes, Kind, static_cast<u8>(Bone));
		for (u32 Axis = 0; Axis < 3; ++Axis)
		{
			Lo = std::min(Lo, Still[Axis]);
			Hi = std::max(Hi, Still[Axis]);
			for (u32 At = 0; Track && At < Track->Curves[Axis].Keys.size(); ++At)
			{
				Lo = std::min(Lo, Track->Curves[Axis].Keys[At].Value);
				Hi = std::max(Hi, Track->Curves[Axis].Keys[At].Value);
			}
		}
	}
	List = ImGui::GetWindowDrawList();
	List->AddRectFilled(ImVec2(Top.x, Pane.y), ImVec2(Left + Width, Pane.y + Tall), ImGui::GetColorU32(ImGuiCol_FrameBg));
	if (!Shown)
	{
		List->AddText(ImVec2(Left + Pad, Pane.y + Row), Faint, "Select a bone to see its curves.");
		AnimCurveKey = -1;
		AnimTipSide = 0;
		return;
	}
	const float Room = std::max(Hi - Lo, AnimLaneKind == 0 ? 2.0f : 0.02f) * 0.15f;
	Lo = IsFrozen ? AnimLaneLo : Lo - Room;
	Hi = IsFrozen ? AnimLaneHi : Hi + Room;
	const float Bottom = Pane.y + Tall;
	const auto YAt = [&](float Value) { return Bottom - (Value - Lo) / (Hi - Lo) * Tall; };
	string32 Bound;
	xr_sprintf(Bound, "%.2f", Hi);
	List->AddText(ImVec2(Left - ImGui::CalcTextSize(Bound).x - 6.0f, Pane.y), Faint, Bound);
	xr_sprintf(Bound, "%.2f", Lo);
	List->AddText(ImVec2(Left - ImGui::CalcTextSize(Bound).x - 6.0f, Bottom - ImGui::GetFontSize()), Faint, Bound);
	s32 HitKey = -1;
	float HitScore = 7.0f, TipScore = 7.0f, HitMoment = 0.0f;
	u8 HitAxis = 0, HitBone = 0, HitTip = 0;
	List->PushClipRect(ImVec2(Left, Pane.y), ImVec2(Left + Width, Bottom), true);
	for (u32 Bone = 0; Bone < Bones; ++Bone)
	{
		if (!IsChosen(Bone))
		{
			continue;
		}
		const PoseBone& Entry = Track->Bones[Bone];
		const auto& Lanes = Track->Lanes;
		const Fvector& Still = AnimLaneKind == 0 ? Entry.Rotation : AnimLaneKind == 1 ? Entry.Offset : Entry.Scale;
		const Lane* Track = Curves::FindLane(Lanes, Kind, static_cast<u8>(Bone));
		for (u8 Axis = 0; Axis < 3; ++Axis)
		{
			if (AnimSolo >= 0 && AnimSolo != Axis)
			{
				continue;
			}
			const Curve* Part = Track && !Track->Curves[Axis].Keys.empty() ? &Track->Curves[Axis] : nullptr;
			for (float X = Left; X <= Left + Width; X += 3.0f)
			{
				List->PathLineTo(ImVec2(X, YAt(Part ? Curves::Value(*Part, RawAt(X), Still[Axis]) : Still[Axis])));
			}
			List->PathStroke(AxisColors[Axis], 0, Part ? 1.5f : 1.0f);
			for (u32 At = 0; Part && At < Part->Keys.size(); ++At)
			{
				const ImVec2 Spot(XAt(Part->Keys[At].Time), YAt(Part->Keys[At].Value));
				const bool IsPicked = AnimPickBone == Bone && AnimPickAxis == Axis && AnimPickKey == static_cast<s32>(At);
				for (u8 Side = 1; IsPicked && Side <= 2; ++Side)
				{
					if (Side == 2 ? At + 1 >= Part->Keys.size() : At == 0)
					{
						continue;
					}
					const ImVec2 Reach = AnimatorUi::Arm(*Part, At, Side == 2, Still[Axis]);
					const ImVec2 Tip(XAt(Part->Keys[At].Time + Reach.x), YAt(Part->Keys[At].Value + Reach.y));
					const float Gap = std::fabs(Tip.x - Input.MousePos.x) + std::fabs(Tip.y - Input.MousePos.y);
					List->AddLine(Spot, Tip, ImGui::GetColorU32(AnimTipSide == Side ? ImGuiCol_Text : ImGuiCol_TextDisabled));
					List->AddCircleFilled(Tip, AnimTipSide == Side ? 4.0f : 3.0f, ImGui::GetColorU32(AnimTipSide == Side ? ImGuiCol_Text : ImGuiCol_TextDisabled));
					if (IsOver && !AnimTipSide && Gap < TipScore)
					{
						TipScore = Gap;
						HitTip = Side;
					}
				}
				List->AddRectFilled(ImVec2(Spot.x - 3.0f, Spot.y - 3.0f), ImVec2(Spot.x + 3.0f, Spot.y + 3.0f), IsPicked ? PickColor : AxisColors[Axis]);
				const float Score = std::fabs(Spot.x - Input.MousePos.x) + std::fabs(Spot.y - Input.MousePos.y);
				if (IsOver && Score < HitScore)
				{
					HitScore = Score;
					HitKey = static_cast<s32>(At);
					HitAxis = Axis;
					HitBone = static_cast<u8>(Bone);
					HitMoment = Part->Keys[At].Time;
				}
			}
		}
	}
	List->AddLine(ImVec2(XAt(Now), Pane.y), ImVec2(XAt(Now), Bottom), HeadColor);
	List->PopClipRect();
	if (IsOver && ImGui::IsMouseClicked(ImGuiMouseButton_Left))
	{
		AnimLaneLo = Lo;
		AnimLaneHi = Hi;
		if (HitTip)
		{
			AnimTipSide = HitTip;
		}
		else if (HitKey >= 0)
		{
			AnimCurveKey = AnimPickKey = HitKey;
			AnimCurveBone = AnimPickBone = HitBone;
			AnimCurveAxis = AnimPickAxis = HitAxis;
			Seek(HitMoment);
		}
		else
		{
			Seek(Pick(Input.MousePos.x));
		}
	}
	const bool IsPulled = IsPressed && ImGui::IsMouseDragging(ImGuiMouseButton_Left);
	const u8 Held = AnimTipSide ? AnimPickBone : AnimCurveBone, Along = AnimTipSide ? AnimPickAxis : AnimCurveAxis;
	if ((AnimTipSide || AnimCurveKey >= 0) && IsPulled && Held < Track->Bones.size())
	{
		ClipTrack* Edited = EditClip(Core, AnimTipSide ? "Move tangent" : "Move curve key");
		const Lane* Track = Edited ? Curves::FindLane(Edited->Lanes, Kind, Held) : nullptr;
		Curve* Part = Track ? const_cast<Curve*>(&Track->Curves[Along]) : nullptr;
		const u32 At = static_cast<u32>(std::max(AnimTipSide ? AnimPickKey : AnimCurveKey, 0)), Keys = Part ? static_cast<u32>(Part->Keys.size()) : 0;
		const float Level = Lo + (Bottom - Input.MousePos.y) / Tall * (Hi - Lo), Frame = 1.0f / Rate;
		const bool IsOut = AnimTipSide == 2;
		if (AnimTipSide && At < Keys && (IsOut ? At + 1 < Keys : At > 0))
		{
			const u32 Head = IsOut ? At : At - 1;
			Curves::SetMode(*Part, Head, Interp::Bezier);
			Key& Item = Part->Keys[At];
			const float Span = Part->Keys[Head + 1].Time - Part->Keys[Head].Time, Sign = IsOut ? 1.0f : -1.0f, Rise = Level - Item.Value;
			const float Reach = Sign * std::clamp(Sign * (RawAt(Input.MousePos.x) - Item.Time), 0.01f * Span, Span);
			(IsOut ? Item.OutTime : Item.InTime) = Reach;
			(IsOut ? Item.OutValue : Item.InValue) = Rise;
			Item.IsBroken |= Input.KeyAlt;
			if (!Item.IsBroken)
			{
				(IsOut ? Item.InValue : Item.OutValue) = (IsOut ? Item.InTime : Item.OutTime) * Rise / Reach;
			}
		}
		else if (!AnimTipSide && At < Keys)
		{
			const ParamInfo& Meta = Info(Kind);
			const float Least = At ? Part->Keys[At - 1].Time + Frame : 0.0f, Most = At + 1 < Keys ? Part->Keys[At + 1].Time - Frame : Clip->Length;
			Part->Keys[At].Time = std::clamp(TimeAt(Input.MousePos.x), Least, std::max(Least, Most));
			Part->Keys[At].Value = std::clamp(Level, Meta.Min, Meta.Max);
		}
	}
	else if (!AnimTipSide && AnimCurveKey < 0 && IsPulled)
	{
		Seek(Pick(Input.MousePos.x));
	}
	if (IsOver && Input.MouseWheel != 0.0f)
	{
		Wheel();
	}
	AnimCurveKey = ImGui::IsMouseDown(ImGuiMouseButton_Left) ? AnimCurveKey : -1;
	AnimTipSide = ImGui::IsMouseDown(ImGuiMouseButton_Left) ? AnimTipSide : 0;
}
}
