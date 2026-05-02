#pragma once

class CUIXml;

namespace PdaConfig
{
constexpr const char* TabAliasesSection = "pda_tab_aliases";
constexpr const char* MapSubdialogWindowName = "map_wnd";
} // namespace PdaConfig

namespace PdaXml
{
constexpr const char* Main = "pda.xml";
constexpr const char* Map = "pda_map.xml";
constexpr const char* Ranking = "pda_ranking.xml";
constexpr const char* FactionWar = "pda_fraction_war.xml";
constexpr const char* ContactsNew = "pda_contacts_new.xml";
constexpr const char* ContactsBackground = "background";
constexpr const char* ContactsLeftFrame = "left_frame_window";
constexpr const char* ContactsRightFrame = "right_frame_window";
constexpr const char* ContactsDetailList = "detail_list";
constexpr const char* LogsLeftFrameLine = "left_frame_line";
constexpr const char* LogsRightFrameLine = "right_frame_line";
constexpr const char* ContactsDialog = "pda_dialog";
constexpr const char* DialogMain = "main";
constexpr const char* DialogAnswersList = "answers_list";
constexpr const char* DialogQuestionsList = "questions_list";
constexpr const char* DialogQuestionItem = "question_item";
constexpr float ContactsFrameSizeTolerance = 2.f;
} // namespace PdaXml

namespace PdaSectionId
{
constexpr const char* Tasks = "eptTasks";
// Tab variant that auto-opens UITaskListWnd side panel when activated.
constexpr const char* TaskList = "eptTaskList";
constexpr const char* Quests = "eptQuests";
constexpr const char* FractionWar = "eptFractionWar";
constexpr const char* Contacts = "eptContacts";
constexpr const char* Ranking = "eptRanking";
constexpr const char* RankingGlobal = "eptRankingGlobal";
constexpr const char* Logs = "eptLogs";
constexpr const char* Encyclopedia = "eptEncyclopedia";
constexpr const char* ActorStatistic = "eptActorStatistic";
constexpr const char* Diary = "eptDiary";
constexpr const char* Map = "eptMap";

const char* Resolve(const char* defaultId);
bool Equals(const shared_str& sectionId, const char* defaultId);
} // namespace PdaSectionId

namespace PdaActorInfo
{
constexpr const char* Show = "ui_pda";
constexpr const char* Hide = "ui_pda_hide";
} // namespace PdaActorInfo

namespace PdaLegacyTabId
{
constexpr const char* Legacy0 = "0";
constexpr const char* Legacy1 = "1";
constexpr const char* Legacy2 = "2";
constexpr const char* Legacy3 = "3";
constexpr const char* Legacy4 = "4";
constexpr const char* Legacy5 = "5";
constexpr const char* Legacy6 = "6";
} // namespace PdaLegacyTabId

namespace PdaScript
{
constexpr const char* OnSetActiveSubdialog = "OnSetActiveSubdialog";
constexpr const char* OnGetRankingsArraySize = "OnGetRankingsArraySize";
constexpr const char* OnGetPdaStatById = "OnGetPdaStatById";
constexpr const char* GetStatById = "pda.get_stat_by_id";
constexpr const char* GetStat = "pda.get_stat";
constexpr const char* GetMaxMemberCount = "pda.get_max_member_count";
constexpr const char* GetMaxResource = "pda.get_max_resource";
constexpr const char* GetMaxPower = "pda.get_max_power";
constexpr const char* GetValuableArtifactIcon = "pda.get_valuable_artifact_icon";
constexpr const char* GetMonsterBack = "pda.get_monster_back";
constexpr const char* GetMonsterIcon = "pda.get_monster_icon";
constexpr const char* GetFavoriteWeapon = "pda.get_favorite_weapon";
} // namespace PdaScript

namespace PdaRankingStatId
{
constexpr const char* MoneyEarned = "money_earned";
constexpr const char* MoneySpent = "money_spent";
constexpr const char* HelpWounded = "help_wounded";
constexpr const char* Headshots = "headshots";
constexpr const char* Deaths = "deaths";
constexpr const char* Distance = "distance";
} // namespace PdaRankingStatId

namespace PdaNavButton
{
constexpr const char* Legend = "btn_nav_legend";
constexpr const char* ZoomIn = "btn_nav_zoom_in";
constexpr const char* Center = "btn_nav_center";
constexpr const char* ZoomOut = "btn_nav_zoom_out";
constexpr const char* ZoomReset = "btn_nav_zmreset";
constexpr const char* Up = "btn_nav_up";
constexpr const char* Down = "btn_nav_down";
constexpr const char* Left = "btn_nav_left";
constexpr const char* Right = "btn_nav_right";
constexpr const char* PersonalSpot = "btn_personal_spot";
// Optional btn_nav_* attribute: personal_spot_rmb="1" enables LMB button + RMB map placement.
constexpr const char* PersonalSpotRmbAttrib = "personal_spot_rmb";
constexpr const char* PersonalSpotRmbHintAttrib = "hint_rmb";
// Optional btn_nav_9+ inside btn_nav_parent; window_name must match (not legacy btn_task_focus).
constexpr const char* TaskFocus = "btn_nav_task_focus";
} // namespace PdaNavButton

namespace PdaMapSpot
{
constexpr const char* Treasure = "treasure";
constexpr const char* PrimaryObject = "primary_object";
constexpr const char* SecondaryTask = "secondary_task_location";
constexpr const char* SecondaryTaskComplexTimer = "secondary_task_location_complex_timer";

constexpr const char* Trader = "ui_pda2_trader_location";
constexpr const char* Mechanic = "ui_pda2_mechanic_location";
constexpr const char* Scout = "ui_pda2_scout_location";
constexpr const char* QuestNpc = "ui_pda2_quest_npc_location";
constexpr const char* Medic = "ui_pda2_medic_location";
constexpr const char* ActorBox = "ui_pda2_actor_box_location";
constexpr const char* ActorSleep = "ui_pda2_actor_sleep_location";
} // namespace PdaMapSpot

// pda_tasks.xml node paths (legacy map header vs in-panel task list).
namespace PdaTaskXml
{
constexpr const char* PanelStorylineItem = "second_task_wnd:storyline_task_item";
constexpr const char* PanelFilterTabs = "second_task_wnd:task_filter_tabs";
constexpr const char* PanelStorylineItemRel = "storyline_task_item";
constexpr const char* PanelStorylineFocusRel = "storyline_task_item:btn_task_focus";
constexpr const char* PanelFilterTabsRel = "task_filter_tabs";
constexpr const char* LegacyStorylineItem = "storyline_task_item";
constexpr const char* LegacyTaskFocus = "btn_task_focus";
constexpr const char* TaskItemFocus = "second_task_wnd:task_item:btn_focus";
constexpr const char* TaskItemTaskFocus = "second_task_wnd:task_item:btn_task_focus";
} // namespace PdaTaskXml

struct STaskWndFeatures
{
    bool panelStoryline = false;
    bool filterTabs = false;
    bool legacyHeader = false;
};

STaskWndFeatures DetectTaskWndFeatures(CUIXml& xml);

struct SPdaContactsLayoutInfo
{
	bool hasDialogNode = false;
	bool hasDialogMain = false;
	bool hasAnswersList = false;
	bool hasQuestionsList = false;
	bool hasDialogFonts = false;
	bool hasBackground = false;
	bool frameSizeMismatch = false;
	float rightFrameWidth = 0.f;
	float rightFrameHeight = 0.f;
	float dialogMainWidth = 0.f;
	float dialogMainHeight = 0.f;
};

SPdaContactsLayoutInfo InspectPdaContactsLayout(CUIXml& xml);
bool IsPdaContactsLayoutValid(const SPdaContactsLayoutInfo& info);
void LogPdaContactsLayoutIssues(const SPdaContactsLayoutInfo& info, const char* xmlFileName);

