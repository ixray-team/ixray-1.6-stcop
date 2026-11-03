#include "stdafx.h"
#include "ActorEditor.h"

// Pixel radius used to pick a notify key on a timeline
constexpr float KeyPickRadius = 6.0f;

static float RoundToTwoDecimals(float Value)
{
	return std::round(Value * 100.0f) / 100.0f;
}

static bool FindKeyAt(const NotifyTrack& Track, float Length, float LocalX, float Width, float& OutTime)
{
	float BestDistance = KeyPickRadius;
	bool Found = false;
	for (const auto& [Time, Notify] : Track.Notifies)
	{
		const float Distance = std::abs(Time / Length * Width - LocalX);
		if (Distance <= BestDistance)
		{
			BestDistance = Distance;
			OutTime = Time;
			Found = true;
		}
	}
	return Found;
}

static bool MoveKey(NotifyTrack& Track, float From, float To)
{
	if (From == To || Track.Notifies.contains(To))
	{
		return false;
	}

	auto Node = Track.Notifies.extract(From);
	if (Node.empty())
	{
		return false;
	}

	Node.key() = To;
	Track.Notifies.insert(std::move(Node));
	return true;
}

// Draws a timeline with diamond keys; the item is an invisible button for interaction
static void DrawKeyTimeline(const char* Id, const xr_vector<float>& Times, float Length, float Playhead, const float* SelectedTime, const ImVec2& Size)
{
	const ImVec2 Min = ImGui::GetCursorScreenPos();
	const ImVec2 Max(Min.x + Size.x, Min.y + Size.y);
	ImGui::InvisibleButton(Id, ImVec2(std::max(Size.x, 1.0f), Size.y), ImGuiButtonFlags_MouseButtonLeft | ImGuiButtonFlags_MouseButtonRight);

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	const ImGuiCol BackColor = ImGui::IsItemActive() ? ImGuiCol_FrameBgActive : (ImGui::IsItemHovered() ? ImGuiCol_FrameBgHovered : ImGuiCol_FrameBg);
	DrawList->AddRectFilled(Min, Max, ImGui::GetColorU32(BackColor), ImGui::GetStyle().FrameRounding);

	if (Length > 0.0f)
	{
		const float Radius = std::min(5.0f, Size.y * 0.4f);
		const float CenterY = (Min.y + Max.y) * 0.5f;
		for (float Time : Times)
		{
			const float X = Min.x + std::clamp(Time / Length, 0.0f, 1.0f) * Size.x;
			const bool Selected = SelectedTime && *SelectedTime == Time;
			const float R = Selected ? Radius + 1.0f : Radius;
			const ImVec2 Top(X, CenterY - R), Right(X + R, CenterY), Bottom(X, CenterY + R), Left(X - R, CenterY);
			DrawList->AddQuadFilled(Top, Right, Bottom, Left, ImGui::GetColorU32(Selected ? ImGuiCol_CheckMark : ImGuiCol_PlotHistogram));
			if (Selected)
			{
				DrawList->AddQuad(Top, Right, Bottom, Left, ImGui::GetColorU32(ImGuiCol_Text));
			}
		}
	}

	const float PlayheadX = Min.x + Playhead * Size.x;
	DrawList->AddLine(ImVec2(PlayheadX, Min.y), ImVec2(PlayheadX, Max.y), ImGui::GetColorU32(ImGuiCol_Text), 1.0f);
}

static void CollectTimes(const NotifyTrack& Track, xr_vector<float>& Out)
{
	for (const auto& [Time, Notify] : Track.Notifies)
	{
		Out.push_back(Time);
	}
}

UIKeyForm::UIKeyForm():m_AutoChange(true), m_TimeFactor(1), m_Position(0), m_currentEditMotion(nullptr)
{
}

UIKeyForm::~UIKeyForm()
{
}

void UIKeyForm::Draw()
{
	auto PrevCurrentMotion = m_currentEditMotion;
	m_currentEditMotion = ATools->GetCurrentMotion();
	if (!m_currentEditMotion || PrevCurrentMotion != m_currentEditMotion)
	{
		ResetNotifySelection();
	}

	bool bMarksPresent12 = (m_currentEditMotion && m_currentEditMotion->marks.size() >= 2);
	bool bMarksPresent34 = (m_currentEditMotion && m_currentEditMotion->marks.size() == 4);

	bool Mark1 = bMarksPresent12 || static_cast<CLevelPreferences*>(EPrefs)->ActorAlwaysShowKeyBar12 || static_cast<CLevelPreferences*>(EPrefs)->ActorAlwaysShowKeyBar34;
	bool Mark2 = bMarksPresent12 || static_cast<CLevelPreferences*>(EPrefs)->ActorAlwaysShowKeyBar12 || static_cast<CLevelPreferences*>(EPrefs)->ActorAlwaysShowKeyBar34;
	bool Mark3 = bMarksPresent34 || static_cast<CLevelPreferences*>(EPrefs)->ActorAlwaysShowKeyBar34;
	bool Mark4 = bMarksPresent34 || static_cast<CLevelPreferences*>(EPrefs)->ActorAlwaysShowKeyBar34;

	ImGui::Begin("KeyForm");
	{
		float a, b, c;
		ATools->GetStatTime(a, b, c);
		if (AutoChange())
		{
			m_Position = c;
		}

		// Toolbar: motion info, playback follow, LOD and speed
		ImGui::Checkbox("Follow", &m_AutoChange);
		ImGui::SetItemTooltip("Sync the time slider with motion playback");
		ImGui::SameLine();
		if (m_currentEditMotion)
		{
			ImGui::Text("%s", m_currentEditMotion->Name());
			ImGui::SameLine();
			ImGui::TextDisabled("%.3f / %.3f s", c - a, b - a);
		}
		else
		{
			ImGui::TextDisabled("No motion selected");
		}

		const float SliderWidth = 110.0f;
		const float RightWidth = SliderWidth * 2 + ImGui::CalcTextSize("LOD").x + ImGui::CalcTextSize("Speed").x + ImGui::GetStyle().ItemSpacing.x * 4;
		ImGui::SameLine(std::max(ImGui::GetCursorPosX(), ImGui::GetWindowContentRegionMax().x - RightWidth));
		ImGui::AlignTextToFramePadding();
		ImGui::TextUnformatted("LOD");
		ImGui::SameLine();
		ImGui::SetNextItemWidth(SliderWidth);
		ImGui::SliderFloat("##lod", &ATools->m_RenderObject.m_fLOD, 0.0f, 1.0f, "%.2f");
		ImGui::SameLine();
		ImGui::TextUnformatted("Speed");
		ImGui::SameLine();
		ImGui::SetNextItemWidth(SliderWidth);
		if (ImGui::SliderFloat("##speed", &m_TimeFactor, 0.0f, 1.0f, "x%.2f"))
		{
			EDevice->time_factor(m_TimeFactor);
		}
		if (ImGui::IsItemClicked(ImGuiMouseButton_Right))
		{
			m_TimeFactor = 1.0f;
			EDevice->time_factor(m_TimeFactor);
		}
		ImGui::SetItemTooltip("Playback speed (right click to reset)");

		const ImGuiStyle& Style = ImGui::GetStyle();
		const float ActionsWidth = ImGui::CalcTextSize("BeginEndDelete").x + Style.FramePadding.x * 6 + Style.ItemSpacing.x * 2;
		const bool MarkEnabled[4] = { Mark1, Mark2, Mark3, Mark4 };
		const char* MarkNames[4] = { "Left 1", "Right 1", "Left 2", "Right 2" };
		const float PlayheadT = (b > a) ? std::clamp((c - a) / (b - a), 0.0f, 1.0f) : 0.0f;
		const ImGuiTableFlags TableFlags = ImGuiTableFlags_BordersInnerH | ImGuiTableFlags_RowBg | ImGuiTableFlags_SizingFixedFit;
		if (ImGui::BeginTable("##marks", 3, TableFlags))
		{
			ImGui::TableSetupColumn("Track", ImGuiTableColumnFlags_WidthFixed, 130.0f);
			ImGui::TableSetupColumn("Timeline", ImGuiTableColumnFlags_WidthStretch);
			ImGui::TableSetupColumn("Actions", ImGuiTableColumnFlags_WidthFixed, ActionsWidth);

			// Time row
			ImGui::TableNextRow();
			ImGui::TableNextColumn();
			ImGui::AlignTextToFramePadding();
			ImGui::TextUnformatted("Time");
			ImGui::TableNextColumn();
			ImGui::SetNextItemWidth(-1);
			if (!m_currentEditMotion)
			{
				ImGui::BeginDisabled();
			}
			if (ImGui::SliderFloat("##key1", &m_Position, a, b, "%.4f"))
			{
				m_AutoChange = false;
			}
			if (!m_currentEditMotion)
			{
				ImGui::EndDisabled();
			}
			ImGui::TableNextColumn();

			for (int Id = 0; Id < 4; ++Id)
			{
				const bool HasChannel = m_currentEditMotion && (int)m_currentEditMotion->marks.size() > Id;
				const bool Enabled = MarkEnabled[Id] && HasChannel;
				ImGui::PushID(Id);
				ImGui::TableNextRow();

				ImGui::TableNextColumn();
				ImGui::AlignTextToFramePadding();
				if (Enabled)
				{
					ImGui::TextUnformatted(MarkNames[Id]);
				}
				else
				{
					ImGui::TextDisabled("%s", MarkNames[Id]);
				}

				ImGui::TableNextColumn();
				DrawMarkTrack(Id, Enabled, PlayheadT, ImVec2(ImGui::GetContentRegionAvail().x, ImGui::GetFrameHeight()));

				ImGui::TableNextColumn();
				if (!HasChannel)
				{
					// Channels come in pairs: Left/Right 1 and Left/Right 2
					const bool FirstPair = Id < 2;
					const bool CanAdd = m_currentEditMotion && (FirstPair || m_currentEditMotion->marks.size() >= 2);
					if (Id % 2 == 0)
					{
						if (!CanAdd)
						{
							ImGui::BeginDisabled();
						}
						if (ImGui::Button(FirstPair ? "Add channels 1" : "Add channels 2"))
						{
							ATools->AddMarksChannel(FirstPair);
						}
						ImGui::SetItemTooltip(FirstPair ? "Create Left 1 / Right 1 mark channels for this motion" : "Create Left 2 / Right 2 mark channels (requires channels 1)");
						if (!CanAdd)
						{
							ImGui::EndDisabled();
						}
					}
					ImGui::PopID();
					continue;
				}
				if (!Enabled)
				{
					ImGui::BeginDisabled();
				}
				if (ImGui::Button("Begin"))
				{
					SetMark(Id, 1);
				}
				ImGui::SetItemTooltip("Start an interval at the current time (or move the start of the current one)");
				ImGui::SameLine();
				if (ImGui::Button("End"))
				{
					SetMark(Id, 2);
				}
				ImGui::SetItemTooltip("End the current interval at the current time");
				ImGui::SameLine();
				if (ImGui::Button("Delete"))
				{
					SetMark(Id, 3);
				}
				ImGui::SetItemTooltip("Delete the interval under the current time");
				if (!Enabled)
				{
					ImGui::EndDisabled();
				}
				ImGui::PopID();
			}
			ImGui::EndTable();
		}
		ImGui::Separator();

		if (m_currentEditMotion)
		{
			DrawNotifies(ActionsWidth, PlayheadT);
			DrawNotifyEditor();
		}
	}
	ImGui::End();
}

inline bool interval_comparer(const motion_marks::interval& i1, const motion_marks::interval& i2)
{
	return (i1.first < i2.first);
}
void UIKeyForm::SetMark(int id, int action)
{
	if (!m_currentEditMotion)
		return;

	if (m_currentEditMotion->marks.size() == 0)
		return;

	motion_marks& M = m_currentEditMotion->marks[id];
	float a, b, c;
	ATools->GetStatTime(a, b, c);
	float cur_time = c - a;

	motion_marks::ITERATOR it = M.intervals.begin();
	motion_marks::ITERATOR it_e = M.intervals.end();

	if (action == 3)
	{ //del current

		for (; it != it_e; ++it)
		{
			motion_marks::interval& iv = *it;
			if (iv.first<cur_time && iv.second>cur_time)
			{
				M.intervals.erase(it);
				break;
			}
		}
	}
	else if (action == 2)
	{//up
		for (; it != it_e; ++it)
		{
			motion_marks::interval& iv = *it;
			if (iv.first<cur_time && iv.second>cur_time)
			{
				iv.second = cur_time;
				break;
			}
		}
	}
	else if (action == 1)
	{//down
		for (; it != it_e; ++it)
		{
			motion_marks::interval& iv = *it;
			if (iv.first<cur_time && iv.second>cur_time)
			{
				iv.first = cur_time;
				break;
			}
		}
		if (it == it_e)
		{//insert new
			M.intervals.push_back(motion_marks::interval(cur_time, b - a));
		}
	}

	std::sort(M.intervals.begin(), M.intervals.end(), interval_comparer);
}

void UIKeyForm::DrawMarkTrack(int id, bool enabled, float playhead, const ImVec2& size)
{
	const ImVec2 Min = ImGui::GetCursorScreenPos();
	const ImVec2 Max(Min.x + size.x, Min.y + size.y);
	ImGui::Dummy(size);

	ImDrawList* DrawList = ImGui::GetWindowDrawList();
	DrawList->AddRectFilled(Min, Max, ImGui::GetColorU32(ImGuiCol_FrameBg), ImGui::GetStyle().FrameRounding);
	if (!enabled)
	{
		return;
	}

	float a, b, c;
	ATools->GetStatTime(a, b, c);
	const float MotionLength = b - a;
	if (m_currentEditMotion && MotionLength > 0 && id < (int)m_currentEditMotion->marks.size())
	{
		const ImU32 IntervalColor = ImGui::GetColorU32(ImGuiCol_PlotHistogram);
		for (const motion_marks::interval& Interval : m_currentEditMotion->marks[id].intervals)
		{
			const float X0 = Min.x + std::clamp(Interval.first / MotionLength, 0.0f, 1.0f) * size.x;
			const float X1 = Min.x + std::clamp(Interval.second / MotionLength, 0.0f, 1.0f) * size.x;
			DrawList->AddRectFilled(ImVec2(X0, Min.y + 2), ImVec2(std::max(X1, X0 + 1), Max.y - 2), IntervalColor, 2.0f);
		}
	}

	const float PlayheadX = Min.x + playhead * size.x;
	DrawList->AddLine(ImVec2(PlayheadX, Min.y), ImVec2(PlayheadX, Max.y), ImGui::GetColorU32(ImGuiCol_Text), 1.0f);
}

void UIKeyForm::ResetNotifySelection()
{
	SelectedBone = shared_str();
	SelectedTrack = -1;
	SelectedTime = 0.0f;
	DraggingKey = false;
}

void UIKeyForm::SelectNotify(const shared_str& Bone, int Track, float Time)
{
	SelectedBone = Bone;
	SelectedTrack = Track;
	SelectedTime = Time;
}

NotifyTrack* UIKeyForm::GetSelectedTrack()
{
	if (!m_currentEditMotion || SelectedTrack < 0)
	{
		return nullptr;
	}

	auto& Tracks = m_currentEditMotion->notify.NotifyTracks;
	auto BoneIt = Tracks.find(SelectedBone);
	if (BoneIt == Tracks.end() || SelectedTrack >= (int)BoneIt->second.size())
	{
		return nullptr;
	}
	return &BoneIt->second[SelectedTrack];
}

anim_notify* UIKeyForm::GetSelectedNotify()
{
	NotifyTrack* Track = GetSelectedTrack();
	if (!Track)
	{
		return nullptr;
	}

	auto It = Track->Notifies.find(SelectedTime);
	return It == Track->Notifies.end() ? nullptr : &It->second;
}

void UIKeyForm::DrawAddBonePopup(editor_notify_data& NotifyData)
{
	if (!ImGui::BeginPopup("##add_notify_bone"))
	{
		return;
	}

	if (ImGui::IsWindowAppearing())
	{
		ImGui::SetKeyboardFocusHere();
	}
	ImGui::SetNextItemWidth(240);
	if (ImGui::InputTextWithHint("##filter", "Search bone...", BoneFilter.InputBuf, IM_ARRAYSIZE(BoneFilter.InputBuf)))
	{
		BoneFilter.Build();
	}

	if (ImGui::BeginListBox("##bones", ImVec2(240, 260)))
	{
		for (const auto& Bone : ATools->CurrentObject()->m_Bones)
		{
			const char* Name = Bone->name.c_str();
			if (!BoneFilter.PassFilter(Name))
			{
				continue;
			}

			const bool Exists = NotifyData.NotifyTracks.contains(Bone->name);
			if (ImGui::Selectable(Name, Exists, Exists ? ImGuiSelectableFlags_Disabled : 0))
			{
				NotifyData.NotifyTracks[Bone->name].emplace_back();
				OpenBone = Bone->name;
			}
		}
		ImGui::EndListBox();
	}
	ImGui::EndPopup();
}

void UIKeyForm::DrawNotifies(float ActionsWidth, float Playhead)
{
	editor_notify_data& NotifyData = m_currentEditMotion->notify;
	if (NotifyData.NotifyToRemove)
	{
		NotifyData.NotifyTracks[std::get<0>(*NotifyData.NotifyToRemove)][std::get<2>(*NotifyData.NotifyToRemove)].Notifies.erase(std::get<1>(*NotifyData.NotifyToRemove));
		NotifyData.NotifyToRemove = nullptr;
	}

	if (!ImGui::IsMouseDown(ImGuiMouseButton_Left))
	{
		DraggingKey = false;
	}

	float a, b, c;
	ATools->GetStatTime(a, b, c);
	const float Length = b - a;
	const float CurrentTime = std::clamp(m_Position - a, 0.0f, std::max(Length, 0.0f));
	const float HalfButton = (ActionsWidth - ImGui::GetStyle().ItemSpacing.x) * 0.5f;
	const ImVec2 ButtonSize(HalfButton, 0);

	const ImGuiTableFlags TableFlags = ImGuiTableFlags_BordersInnerH | ImGuiTableFlags_RowBg | ImGuiTableFlags_SizingFixedFit;
	if (!ImGui::BeginTable("##notifies", 3, TableFlags))
	{
		return;
	}
	ImGui::TableSetupColumn("Track", ImGuiTableColumnFlags_WidthFixed, 130.0f);
	ImGui::TableSetupColumn("Timeline", ImGuiTableColumnFlags_WidthStretch);
	ImGui::TableSetupColumn("Actions", ImGuiTableColumnFlags_WidthFixed, ActionsWidth);

	xr_vector<float> Times;
	auto TimelineSize = [] { return ImVec2(ImGui::GetContentRegionAvail().x, ImGui::GetFrameHeight()); };

	// Summary row with all notifies of the motion
	ImGui::TableNextRow();
	ImGui::TableNextColumn();
	ImGui::SetNextItemOpen(true, ImGuiCond_Once);
	ImGui::AlignTextToFramePadding();
	const bool RootOpened = ImGui::TreeNodeEx("Notifies", ImGuiTreeNodeFlags_SpanAvailWidth);
	ImGui::TableNextColumn();
	for (const auto& [BoneName, Tracks] : NotifyData.NotifyTracks)
	{
		for (const NotifyTrack& Track : Tracks)
		{
			CollectTimes(Track, Times);
		}
	}
	DrawKeyTimeline("##all", Times, Length, Playhead, nullptr, TimelineSize());
	ImGui::SetItemTooltip("All notifies of the motion");
	ImGui::TableNextColumn();
	if (ImGui::Button("Add bone...", ImVec2(ActionsWidth, 0)))
	{
		BoneFilter.Clear();
		ImGui::OpenPopup("##add_notify_bone");
	}
	ImGui::SetItemTooltip("Add a bone with a notify track");
	DrawAddBonePopup(NotifyData);

	if (RootOpened)
	{
		shared_str BoneToRemove;
		int TrackToRemove = -1;

		for (auto& [BoneName, Tracks] : NotifyData.NotifyTracks)
		{
			ImGui::PushID(BoneName.c_str());
			ImGui::TableNextRow();
			ImGui::TableNextColumn();
			if (OpenBone == BoneName)
			{
				ImGui::SetNextItemOpen(true);
				OpenBone = shared_str();
			}
			ImGui::AlignTextToFramePadding();
			const bool BoneOpened = ImGui::TreeNodeEx(BoneName.c_str(), ImGuiTreeNodeFlags_SpanAvailWidth);

			ImGui::TableNextColumn();
			Times.clear();
			for (const NotifyTrack& Track : Tracks)
			{
				CollectTimes(Track, Times);
			}
			DrawKeyTimeline("##bone", Times, Length, Playhead, nullptr, TimelineSize());

			ImGui::TableNextColumn();
			if (ImGui::Button("+ Track", ButtonSize))
			{
				Tracks.emplace_back();
				OpenBone = BoneName;
			}
			ImGui::SetItemTooltip("Add a notify track to this bone");
			ImGui::SameLine();
			if (ImGui::Button("Remove", ButtonSize))
			{
				BoneToRemove = BoneName;
			}
			ImGui::SetItemTooltip("Remove the bone with all its notifies");

			if (BoneOpened)
			{
				for (int TrackId = 0; TrackId < (int)Tracks.size(); ++TrackId)
				{
					NotifyTrack& Track = Tracks[TrackId];
					ImGui::PushID(TrackId);
					ImGui::TableNextRow();

					ImGui::TableNextColumn();
					ImGui::AlignTextToFramePadding();
					ImGui::Text("Track %d", TrackId);
					ImGui::SameLine();
					ImGui::TextDisabled("(%d)", (int)Track.Notifies.size());

					ImGui::TableNextColumn();
					Times.clear();
					CollectTimes(Track, Times);
					const bool TrackSelected = SelectedBone == BoneName && SelectedTrack == TrackId;
					DrawKeyTimeline("##track", Times, Length, Playhead, TrackSelected ? &SelectedTime : nullptr, TimelineSize());

					const float Width = ImGui::GetItemRectSize().x;
					const float LocalX = ImGui::GetMousePos().x - ImGui::GetItemRectMin().x;
					const float MouseTime = Length > 0.0f ? RoundToTwoDecimals(std::clamp(LocalX / Width, 0.0f, 1.0f) * Length) : 0.0f;
					if (ImGui::IsItemHovered() || ImGui::IsItemActive())
					{
						ImGui::SetTooltip("%.2f s\nLMB: select or add, drag to move\nRMB: delete", MouseTime);
					}

					if (Length > 0.0f && ImGui::IsItemClicked(ImGuiMouseButton_Left))
					{
						float KeyTime;
						if (!FindKeyAt(Track, Length, LocalX, Width, KeyTime))
						{
							KeyTime = MouseTime;
							Track.Notifies.try_emplace(KeyTime);
						}
						SelectNotify(BoneName, TrackId, KeyTime);
						DraggingKey = true;
					}
					else if (Length > 0.0f && ImGui::IsItemClicked(ImGuiMouseButton_Right))
					{
						float KeyTime;
						if (FindKeyAt(Track, Length, LocalX, Width, KeyTime))
						{
							Track.Notifies.erase(KeyTime);
							if (TrackSelected && SelectedTime == KeyTime)
							{
								ResetNotifySelection();
							}
						}
					}
					else if (DraggingKey && ImGui::IsItemActive() && ImGui::IsMouseDragging(ImGuiMouseButton_Left) && SelectedBone == BoneName && SelectedTrack == TrackId)
					{
						if (MoveKey(Track, SelectedTime, MouseTime))
						{
							SelectedTime = MouseTime;
						}
					}

					ImGui::TableNextColumn();
					if (ImGui::Button("+ Key", ButtonSize))
					{
						const float Time = RoundToTwoDecimals(CurrentTime);
						Track.Notifies.try_emplace(Time);
						SelectNotify(BoneName, TrackId, Time);
					}
					ImGui::SetItemTooltip("Add a notify at the current time");
					ImGui::SameLine();
					if (ImGui::Button("Delete", ButtonSize))
					{
						BoneToRemove = BoneName;
						TrackToRemove = TrackId;
					}
					ImGui::SetItemTooltip("Delete this track");
					ImGui::PopID();
				}

				if (Tracks.empty())
				{
					ImGui::TableNextRow();
					ImGui::TableNextColumn();
					ImGui::TableNextColumn();
					ImGui::TextDisabled("No tracks. Press \"+ Track\" to add one.");
				}
				ImGui::TreePop();
			}
			ImGui::PopID();
		}

		if (NotifyData.NotifyTracks.empty())
		{
			ImGui::TableNextRow();
			ImGui::TableNextColumn();
			ImGui::TableNextColumn();
			ImGui::TextDisabled("No notifies. Press \"Add bone...\" to start.");
		}

		auto BoneIt = NotifyData.NotifyTracks.find(BoneToRemove);
		if (BoneIt != NotifyData.NotifyTracks.end())
		{
			if (TrackToRemove < 0)
			{
				NotifyData.NotifyTracks.erase(BoneIt);
			}
			else
			{
				BoneIt->second.erase(BoneIt->second.begin() + TrackToRemove);
			}

			if (SelectedBone == BoneToRemove)
			{
				ResetNotifySelection();
			}
		}
		ImGui::TreePop();
	}
	ImGui::EndTable();
}

void UIKeyForm::DrawNotifyEditor()
{
	anim_notify* Notify = GetSelectedNotify();
	if (!Notify)
	{
		ImGui::TextDisabled("Select a notify key to edit it.");
		return;
	}

	float a, b, c;
	ATools->GetStatTime(a, b, c);
	const float Length = std::max(b - a, 0.0f);
	const float LabelWidth = 130.0f;

	ImGui::SeparatorText("Selected notify");
	ImGui::Text("%s / Track %d", SelectedBone.c_str(), SelectedTrack);

	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted("Time");
	ImGui::SameLine(LabelWidth);
	ImGui::SetNextItemWidth(200);
	float NewTime = SelectedTime;
	if (ImGui::DragFloat("##time", &NewTime, 0.01f, 0.0f, Length, "%.2f s"))
	{
		NewTime = RoundToTwoDecimals(std::clamp(NewTime, 0.0f, Length));
		if (MoveKey(*GetSelectedTrack(), SelectedTime, NewTime))
		{
			SelectedTime = NewTime;
		}
	}
	ImGui::SameLine();
	if (ImGui::Button("Go to"))
	{
		m_AutoChange = false;
		m_Position = a + SelectedTime;
	}
	ImGui::SetItemTooltip("Move the time slider to this notify");

	// The time move above may have reallocated the key
	Notify = GetSelectedNotify();

	ImGui::AlignTextToFramePadding();
	ImGui::TextUnformatted("External ref");
	ImGui::SameLine(LabelWidth);
	ImGui::SetNextItemWidth(-1);
	char Buffer[256] = {};
	xr_strcpy(Buffer, Notify->ExternalRef.size() ? Notify->ExternalRef.c_str() : "");
	if (ImGui::InputText("##external_ref", Buffer, sizeof(Buffer)))
	{
		Notify->ExternalRef = Buffer;
	}

	if (ImGui::Button("Delete notify"))
	{
		GetSelectedTrack()->Notifies.erase(SelectedTime);
		ResetNotifySelection();
	}
}
