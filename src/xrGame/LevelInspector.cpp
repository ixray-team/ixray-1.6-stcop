#include "StdAfx.h"
#include "LevelInspector.h"
#include "GamePersistent.h"
#include "Level.h"
#include "Actor.h"
#include "../xrEngine/xr_collide_form.h"
#include "space_restrictor.h"
#include "../xrEngine/xr_ioc_cmd.h"
#include "../xrEngine/xr_input.h"
#include "ClimableObject.h"
#include "Inventory.h"
#include "attachment_owner.h"
#include "InventoryOwner.h"
#include "PHMovementControl.h"
#include "CharacterPhysicsSupport.h"
#include "entity_alive.h"
#include "ai/stalker/ai_stalker.h"
#include "movement_manager.h"
#include "detail_path_manager.h"
#include "memory_manager.h"
#include "alife_update_manager.h"
#include "alife_simulator.h"
#include "ai_object_location_impl.h"
#include "abstract_path_manager.h"
#include "level_changer.h"
#include "smart_cover_object.h"
#include "cover_manager.h"
#include "smart_cover.h"
#include "smart_cover_description.h"
#include "smart_cover_loophole.h"
#include "visual_memory_manager.h"
#include "HUDManager.h"
#include "ai_space.h"
#include "patrol_path.h"
#include "patrol_path_storage.h"
#include "player_hud.h"
#include <magic_enum/magic_enum.hpp>
Fvector aabb_selection_vertices[]
{
	{-0.505f, -0.505f, -0.505f},
	{ 0.505f, -0.505f, -0.505f},
	{-0.505f,  0.505f, -0.505f},
	{ 0.505f,  0.505f, -0.505f},
	{-0.505f, -0.505f,  0.505f},
	{ 0.505f, -0.505f,  0.505f},
	{-0.505f,  0.505f,  0.505f},
	{ 0.505f,  0.505f,  0.505f},

	{-0.255f, -0.505f, -0.505f},
	{ 0.255f, -0.505f, -0.505f},
	{-0.255f,  0.505f, -0.505f},
	{ 0.255f,  0.505f, -0.505f},
	{-0.255f, -0.505f,  0.505f},
	{ 0.255f, -0.505f,  0.505f},
	{-0.255f,  0.505f,  0.505f},
	{ 0.255f,  0.505f,  0.505f},

	{-0.505f, -0.255f, -0.505f},
	{ 0.505f, -0.255f, -0.505f},
	{-0.505f,  0.255f, -0.505f},
	{ 0.505f,  0.255f, -0.505f},
	{-0.505f, -0.255f,  0.505f},
	{ 0.505f, -0.255f,  0.505f},
	{-0.505f,  0.255f,  0.505f},
	{ 0.505f,  0.255f,  0.505f},

	{-0.505f, -0.505f, -0.255f},
	{ 0.505f, -0.505f, -0.255f},
	{-0.505f,  0.505f, -0.255f},
	{ 0.505f,  0.505f, -0.255f},
	{-0.505f, -0.505f,  0.255f},
	{ 0.505f, -0.505f,  0.255f},
	{-0.505f,  0.505f,  0.255f},
	{ 0.505f,  0.505f,  0.255f}
};

LevelInspector::lindex aabb_selection_lindices[]
{
	{0, 8}, {0, 16}, {0, 24},
	{1, 9}, {1, 17}, {1, 25},
	{2, 10}, {2, 18}, {2, 26},
	{3, 11}, {3, 19}, {3, 27},
	{4, 12}, {4, 20}, {4, 28},
	{5, 13}, {5, 21}, {5, 29},
	{6, 14}, {6, 22}, {6, 30},
	{7, 15}, {7, 23}, {7, 31}
};
constexpr int aabb_lvertices_max = std::size(aabb_selection_lindices) * 2;

Fvector obb_vertices[8]
{
	{-1.f, -1.f, -1.f},
	{-1.f, 1.f, -1.f},
	{1.f, 1.f, -1.f},
	{1.f, -1.f, -1.f},
	{-1.f, -1.f, 1.f},
	{-1.f, 1.f, 1.f},
	{1.f, 1.f, 1.f},
	{1.f, -1.f, 1.f}
};

LevelInspector::lindex obb_lindices[12]
{
	{0,1}, {1,2},
	{2,3}, {3,0},
	{4,5}, {5,6},
	{6,7}, {7,4},
	{1,5}, {2,6},
	{3,7}, {0,4}
};
constexpr int obb_lvertices_max = std::size(obb_lindices) * 3;

LevelInspector::tindex obb_tindices[12]
{
	{0,1,2}, {0,2,3},
	{5,4,7}, {5,7,6},
	{1,5,6}, {1,6,2},
	{4,0,3}, {4,3,7},
	{3,2,6}, {3,6,7},
	{4,5,1}, {4,1,0}
};
constexpr int obb_tvertices_max = std::size(obb_tindices) * 3;

Fvector sphere_vertices[114]
{
	{0.f,0.f,1.f}, {0.f,0.3827f,0.9239f}, {-0.1464f,0.3536f,0.9239f},
	{-0.2706f,0.2706f,0.9239f}, {-0.3536f,0.1464f,0.9239f}, {-0.3827f,0.f,0.9239f},
	{-0.3536f,-0.1464f,0.9239f}, {-0.2706f,-0.2706f,0.9239f}, {-0.1464f,-0.3536f,0.9239f},
	{0.f,-0.3827f,0.9239f}, {0.1464f,-0.3536f,0.9239f}, {0.2706f,-0.2706f,0.9239f},
	{0.3536f,-0.1464f,0.9239f}, {0.3827f,0.f,0.9239f}, {0.3536f,0.1464f,0.9239f},
	{0.2706f,0.2706f,0.9239f}, {0.1464f,0.3536f,0.9239f}, {0.f,0.7071f,0.7071f},
	{-0.2706f,0.6533f,0.7071f}, {-0.5f,0.5f,0.7071f}, {-0.6533f,0.2706f,0.7071f},
	{-0.7071f,0.f,0.7071f}, {-0.6533f,-0.2706f,0.7071f}, {-0.5f,-0.5f,0.7071f},
	{-0.2706f,-0.6533f,0.7071f}, {0.f,-0.7071f,0.7071f}, {0.2706f,-0.6533f,0.7071f},
	{0.5f,-0.5f,0.7071f}, {0.6533f,-0.2706f,0.7071f}, {0.7071f,0.f,0.7071f},
	{0.6533f,0.2706f,0.7071f}, {0.5f,0.5f,0.7071f}, {0.2706f,0.6533f,0.7071f},
	{0.f,0.9239f,0.3827f}, {-0.3536f,0.8536f,0.3827f}, {-0.6533f,0.6533f,0.3827f},
	{-0.8536f,0.3536f,0.3827f}, {-0.9239f,0.f,0.3827f}, {-0.8536f,-0.3536f,0.3827f},
	{-0.6533f,-0.6533f,0.3827f}, {-0.3536f,-0.8536f,0.3827f}, {0.f,-0.9239f,0.3827f},
	{0.3536f,-0.8536f,0.3827f}, {0.6533f,-0.6533f,0.3827f}, {0.8536f,-0.3536f,0.3827f},
	{0.9239f,0.f,0.3827f}, {0.8536f,0.3536f,0.3827f}, {0.6533f,0.6533f,0.3827f},
	{0.3536f,0.8536f,0.3827f}, {0.f,1.f,0.f}, {-0.3827f,0.9239f,0.f},
	{-0.7071f,0.7071f,0.f}, {-0.9239f,0.3827f,0.f}, {-1.f,0.f,0.f},
	{-0.9239f,-0.3827f,0.f}, {-0.7071f,-0.7071f,0.f}, {-0.3827f,-0.9239f,0.f},
	{0.f,-1.f,0.f}, {0.3827f,-0.9239f,0.f}, {0.7071f,-0.7071f,0.f},
	{0.9239f,-0.3827f,0.f}, {1.f,0.f,0.f}, {0.9239f,0.3827f,0.f},
	{0.7071f,0.7071f,0.f}, {0.3827f,0.9239f,0.f}, {0.f,0.9239f,-0.3827f},
	{-0.3536f,0.8536f,-0.3827f}, {-0.6533f,0.6533f,-0.3827f}, {-0.8536f,0.3536f,-0.3827f},
	{-0.9239f,0.f,-0.3827f}, {-0.8536f,-0.3536f,-0.3827f}, {-0.6533f,-0.6533f,-0.3827f},
	{-0.3536f,-0.8536f,-0.3827f}, {0.f,-0.9239f,-0.3827f}, {0.3536f,-0.8536f,-0.3827f},
	{0.6533f,-0.6533f,-0.3827f}, {0.8536f,-0.3536f,-0.3827f}, {0.9239f,0.f,-0.3827f},
	{0.8536f,0.3536f,-0.3827f}, {0.6533f,0.6533f,-0.3827f}, {0.3536f,0.8536f,-0.3827f},
	{0.f,0.7071f,-0.7071f}, {-0.2706f,0.6533f,-0.7071f}, {-0.5f,0.5f,-0.7071f},
	{-0.6533f,0.2706f,-0.7071f}, {-0.7071f,0.f,-0.7071f}, {-0.6533f,-0.2706f,-0.7071f},
	{-0.5f,-0.5f,-0.7071f}, {-0.2706f,-0.6533f,-0.7071f}, {0.f,-0.7071f,-0.7071f},
	{0.2706f,-0.6533f,-0.7071f}, {0.5f,-0.5f,-0.7071f}, {0.6533f,-0.2706f,-0.7071f},
	{0.7071f,0.f,-0.7071f}, {0.6533f,0.2706f,-0.7071f}, {0.5f,0.5f,-0.7071f},
	{0.2706f,0.6533f,-0.7071f}, {0.f,0.3827f,-0.9239f}, {-0.1464f,0.3536f,-0.9239f},
	{-0.2706f,0.2706f,-0.9239f}, {-0.3536f,0.1464f,-0.9239f}, {-0.3827f,0.f,-0.9239f},
	{-0.3536f,-0.1464f,-0.9239f}, {-0.2706f,-0.2706f,-0.9239f}, {-0.1464f,-0.3536f,-0.9239f},
	{0.f,-0.3827f,-0.9239f}, {0.1464f,-0.3536f,-0.9239f}, {0.2706f,-0.2706f,-0.9239f},
	{0.3536f,-0.1464f,-0.9239f}, {0.3827f,0.f,-0.9239f}, {0.3536f,0.1464f,-0.9239f},
	{0.2706f,0.2706f,-0.9239f}, {0.1464f,0.3536f,-0.9239f}, {0.f,0.f,-1.f}
};

LevelInspector::lindex sphere_lindices[48]
{
	{49, 50}, {50, 51}, {51, 52}, {52, 53},
	{53, 54}, {54, 55}, {55, 56}, {56, 57},
	{57, 58}, {58, 59}, {59, 60}, {60, 61},
	{61, 62}, {62, 63}, {63, 64}, {64, 49},
	{0, 1}, {1, 17}, {17, 33}, {33, 49},
	{49, 65}, {65, 81}, {81, 97}, {97, 113},
	{113, 109}, {109, 93}, {93, 77}, {77, 61},
	{61, 45}, {45, 29}, {29, 13}, {13, 0},
	{0, 5}, {5, 21}, {21, 37}, {37, 53},
	{53, 69}, {69, 85}, {85, 101}, {101, 113},
	{113, 105}, {105, 89}, {89, 73}, {73, 57},
	{57, 41}, {41, 25}, {25, 9}, {9, 0}
};
constexpr int sphere_lvertices_max = std::size(sphere_lindices) * 2;

LevelInspector::tindex sphere_tindices[224]
{
	{0, 1, 2}, {0, 2, 3}, {0, 3, 4}, {0, 4, 5}, {0, 5, 6}, {0, 6, 7},
	{0, 7, 8}, {0, 8, 9}, {0, 9, 10}, {0, 10, 11}, {0, 11, 12}, {0, 12, 13},
	{0, 13, 14}, {0, 14, 15}, {0, 15, 16}, {0, 16, 1},

	{1, 17, 18}, {1, 18, 2}, {2, 18, 19}, {2, 19, 3}, {3, 19, 20}, {3, 20, 4},
	{4, 20, 21}, {4, 21, 5}, {5, 21, 22}, {5, 22, 6}, {6, 22, 23}, {6, 23, 7},
	{7, 23, 24}, {7, 24, 8}, {8, 24, 25}, {8, 25, 9}, {9, 25, 26}, {9, 26, 10},
	{10, 26, 27}, {10, 27, 11}, {11, 27, 28}, {11, 28, 12}, {12, 28, 29}, {12, 29, 13},
	{13, 29, 30}, {13, 30, 14}, {14, 30, 31}, {14, 31, 15}, {15, 31, 32}, {15, 32, 16},
	{16, 32, 17}, {16, 17, 1},

	{17, 33, 34}, {17, 34, 18}, {18, 34, 35}, {18, 35, 19}, {19, 35, 36}, {19, 36, 20},
	{20, 36, 37}, {20, 37, 21}, {21, 37, 38}, {21, 38, 22}, {22, 38, 39}, {22, 39, 23},
	{23, 39, 40}, {23, 40, 24}, {24, 40, 41}, {24, 41, 25}, {25, 41, 42}, {25, 42, 26},
	{26, 42, 43}, {26, 43, 27}, {27, 43, 44}, {27, 44, 28}, {28, 44, 45}, {28, 45, 29},
	{29, 45, 46}, {29, 46, 30}, {30, 46, 47}, {30, 47, 31}, {31, 47, 48}, {31, 48, 32},
	{32, 48, 33}, {32, 33, 17},

	{33, 49, 50}, {33, 50, 34}, {34, 50, 51}, {34, 51, 35}, {35, 51, 52}, {35, 52, 36},
	{36, 52, 53}, {36, 53, 37}, {37, 53, 54}, {37, 54, 38}, {38, 54, 55}, {38, 55, 39},
	{39, 55, 56}, {39, 56, 40}, {40, 56, 57}, {40, 57, 41}, {41, 57, 58}, {41, 58, 42},
	{42, 58, 59}, {42, 59, 43}, {43, 59, 60}, {43, 60, 44}, {44, 60, 61}, {44, 61, 45},
	{45, 61, 62}, {45, 62, 46}, {46, 62, 63}, {46, 63, 47}, {47, 63, 64}, {47, 64, 48},
	{48, 64, 49}, {48, 49, 33},

	{49, 65, 66}, {49, 66, 50}, {50, 66, 67}, {50, 67, 51}, {51, 67, 68}, {51, 68, 52},
	{52, 68, 69}, {52, 69, 53}, {53, 69, 70}, {53, 70, 54}, {54, 70, 71}, {54, 71, 55},
	{55, 71, 72}, {55, 72, 56}, {56, 72, 73}, {56, 73, 57}, {57, 73, 74}, {57, 74, 58},
	{58, 74, 75}, {58, 75, 59}, {59, 75, 76}, {59, 76, 60}, {60, 76, 77}, {60, 77, 61},
	{61, 77, 78}, {61, 78, 62}, {62, 78, 79}, {62, 79, 63}, {63, 79, 80}, {63, 80, 64},
	{64, 80, 65}, {64, 65, 49},

	{65, 81, 82}, {65, 82, 66}, {66, 82, 83}, {66, 83, 67}, {67, 83, 84}, {67, 84, 68},
	{68, 84, 85}, {68, 85, 69}, {69, 85, 86}, {69, 86, 70}, {70, 86, 87}, {70, 87, 71},
	{71, 87, 88}, {71, 88, 72}, {72, 88, 89}, {72, 89, 73}, {73, 89, 90}, {73, 90, 74},
	{74, 90, 91}, {74, 91, 75}, {75, 91, 92}, {75, 92, 76}, {76, 92, 93}, {76, 93, 77},
	{77, 93, 94}, {77, 94, 78}, {78, 94, 95}, {78, 95, 79}, {79, 95, 96}, {79, 96, 80},
	{80, 96, 81}, {80, 81, 65},

	{81, 97, 98}, {81, 98, 82}, {82, 98, 99}, {82, 99, 83}, {83, 99, 100}, {83, 100, 84},
	{84, 100, 101}, {84, 101, 85}, {85, 101, 102}, {85, 102, 86}, {86, 102, 103}, {86, 103, 87},
	{87, 103, 104}, {87, 104, 88}, {88, 104, 105}, {88, 105, 89}, {89, 105, 106}, {89, 106, 90},
	{90, 106, 107}, {90, 107, 91}, {91, 107, 108}, {91, 108, 92}, {92, 108, 109}, {92, 109, 93},
	{93, 109, 110}, {93, 110, 94}, {94, 110, 111}, {94, 111, 95}, {95, 111, 112}, {95, 112, 96},
	{96, 112, 97}, {96, 97, 81},

	{113, 97, 98}, {113, 98, 99}, {113, 99, 100}, {113, 100, 101}, {113, 101, 102},
	{113, 102, 103}, {113, 103, 104}, {113, 104, 105}, {113, 105, 106}, {113, 106, 107},
	{113, 107, 108}, {113, 108, 109}, {113, 109, 110}, {113, 110, 111}, {113, 111, 112},
	{113, 112, 97}
};
constexpr int sphere_tvertices_max = std::size(sphere_tindices) * 3;

Fvector cylinder_vertices[24]
{
	{0.5f, 0.f, 0.5f },
	{0.5f, -0.f, -0.5f},
	{0.433f, -0.25f, -0.5f},
	{0.433f, -0.25f, 0.5f },
	{0.25f, -0.433f, -0.5f},
	{0.25f, -0.433f, 0.5f },
	{0.f, -0.5f, -0.5f},
	{-0.f, -0.5f, 0.5f },
	{-0.25f, -0.433f, -0.5f},
	{-0.25f, -0.433f, 0.5f },
	{-0.433f, -0.25f, -0.5f},
	{-0.433f, -0.25f, 0.5f },
	{-0.5f, -0.f, -0.5f},
	{-0.5f, 0.f, 0.5f },
	{-0.433f, 0.25f, -0.5f},
	{-0.433f, 0.25f, 0.5f },
	{-0.25f, 0.433f, -0.5f},
	{-0.25f, 0.433f, 0.5f },
	{0.f, 0.5f, -0.5f},
	{-0.f, 0.5f, 0.5f },
	{0.25f, 0.433f, -0.5f},
	{0.25f, 0.433f, 0.5f },
	{0.433f, 0.25f, -0.5f},
	{0.433f, 0.25f, 0.5f }
};

LevelInspector::lindex cylinder_lindices[36]
{
	{0, 1}, {2, 3},
	{4, 5}, {6, 7},
	{8, 9}, {10, 11},
	{12, 13}, {14, 15},
	{16, 17}, {18, 19},
	{20, 21}, {22, 23},
	{1, 2}, {2, 4},
	{4, 6}, {6, 8},
	{8, 10}, {10, 12},
	{12, 14}, {14, 16},
	{16, 18}, {18, 20},
	{20, 22}, {22, 1},
	{0, 3}, {3, 5},
	{5, 7}, {7, 9},
	{9, 11}, {11, 13},
	{13, 15}, {15, 17},
	{17, 19}, {19, 21},
	{21, 23}, {23, 0}
};
constexpr int cylinder_lvertices_max = std::size(cylinder_lindices) * 2;

LevelInspector::tindex cylinder_tindices[46]
{
	{1, 0, 2}, {0, 3, 2},
	{2, 3, 4}, {3, 5, 4},
	{4, 5, 6}, {5, 7, 6},
	{6, 7, 8}, {7, 9, 8},
	{8, 9, 10}, {9, 11, 10},
	{10, 11, 12}, {11, 13, 12},
	{12, 13, 14}, {13, 15, 14},
	{14, 15, 16}, {15, 17, 16},
	{16, 17, 18}, {17, 19, 18},
	{18, 19, 20}, {19, 21, 20},
	{20, 21, 22}, {21, 23, 22},
	{22, 23, 1}, {23, 0, 1},
	{0, 3, 23}, {3, 5, 23},
	{5, 7, 23}, {7, 9, 23},
	{9, 11, 23}, {11, 13, 23},
	{13, 15, 23}, {15, 17, 23},
	{17, 19, 23}, {19, 21, 23},
	{21, 23, 23}, {1, 2, 22},
	{2, 4, 22}, {4, 6, 22},
	{6, 8, 22}, {8, 10, 22},
	{10, 12, 22}, {12, 14, 22},
	{14, 16, 22}, {16, 18, 22},
	{18, 20, 22}, {20, 22, 22}
};
constexpr int cylinder_tvertices_max = std::size(cylinder_tindices) * 3;

Fvector cone_vertices[17]
{
	{0.f, 0.f, 0.f},
	{0.5f, 0.f, 1.f},
	{0.4619f, 0.1913f, 1.f},
	{0.3536f, 0.3536f, 1.f},
	{0.1913f, 0.4619f, 1.f},
	{-0.f, 0.5f, 1.f},
	{-0.1913f, 0.4619f, 1.f},
	{-0.3536f, 0.3536f, 1.f},
	{-0.4619f, 0.1913f, 1.f},
	{-0.5f, -0.f, 1.f},
	{-0.4619f, -0.1913f, 1.f},
	{-0.3536f, -0.3536f, 1.f},
	{-0.1913f, -0.4619f, 1.f},
	{0.f, -0.5f, 1.f},
	{0.1913f, -0.4619f, 1.f},
	{0.3536f, -0.3536f, 1.f},
	{0.4619f, -0.1913f, 1.f}
};

LevelInspector::lindex cone_lindices[32]
{
	{0, 1}, {0, 2},
	{0, 3}, {0, 4},
	{0, 5}, {0, 6},
	{0, 7}, {0, 8},
	{0, 9}, {0, 10},
	{0, 11}, {0, 12},
	{0, 13}, {0, 14},
	{0, 15}, {0, 16},
	{1, 2}, {2, 3},
	{3, 4}, {4, 5},
	{5, 6}, {6, 7},
	{7, 8}, {8, 9},
	{9, 10}, {10, 11},
	{11, 12}, {12, 13},
	{13, 14}, {14, 15},
	{15, 16}, {16, 1},
};
constexpr int cone_lvertices_max = std::size(cone_lindices) * 2;

LevelInspector::tindex cone_tindices[30]
{
	{0, 1, 2}, {0, 2, 3},
	{0, 3, 4}, {0, 4, 5},
	{0, 5, 6}, {0, 6, 7},
	{0, 7, 8}, {0, 8, 9},
	{0, 9, 10}, {0, 10, 11},
	{0, 11, 12}, {0, 12, 13},
	{0, 13, 14}, {0, 14, 15},
	{0, 15, 16}, {0, 16, 1},
	{1, 2, 3}, {1, 3, 4},
	{1, 4, 5}, {1, 5, 6},
	{1, 6, 7}, {1, 7, 8},
	{1, 8, 9}, {1, 9, 10},
	{1, 10, 11}, {1, 11, 12},
	{1, 12, 13}, {1, 13, 14},
	{1, 14, 15}, {1, 15, 16}
};
constexpr int cone_tvertices_max = std::size(cone_tindices) * 3;

Fvector graph_point_vertices[5]
{
	{0.0f, 1.0f, 0.0f},
	{0.0f, 0.0f, -0.5f},
	{0.0f, 0.0f, 0.5f},
	{-0.5f, 0.0f, 0.0f},
	{0.5f, 0.0f, 0.0f},
};

LevelInspector::lindex graph_point_lindices[8]
{ 
	{0,1}, {0,2},
	{0,3}, {0,4},
	{1,3}, {3,2},
	{2,4}, {4,1}
};
constexpr int graph_point_lvertices_max = std::size(graph_point_lindices) * 2;

LevelInspector::tindex graph_point_tindices[4]
{
	{1,3,0},
	{3,2,0},
	{2,4,0},
	{4,1,0}
};
constexpr int graph_point_tvertices_max = std::size(graph_point_tindices) * 3;

ICF void RenderSkeletonFlags(Flags32& flags, bool hud_mode)
{
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
	ImGui::CheckboxFlags("Draw joints", &flags.flags, LevelInspector::ESKELETON_INFO::ESI_BONES);
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
	ImGui::CheckboxFlags("Draw joints info", &flags.flags, LevelInspector::ESKELETON_INFO::ESI_BONES_INFO);
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
	ImGui::CheckboxFlags("Draw links", &flags.flags, LevelInspector::ESKELETON_INFO::ESI_BONES_LINKS);

	if (hud_mode)
	{
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
		ImGui::CheckboxFlags("Draw fire&shell points", &flags.flags, LevelInspector::ESKELETON_INFO::ESI_FIRE_POINTS);
	}
	else
	{
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
		ImGui::CheckboxFlags("Draw hit boxes", &flags.flags, LevelInspector::ESKELETON_INFO::ESI_HIT_SHAPES);
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
		ImGui::CheckboxFlags("Draw Actor", &flags.flags, LevelInspector::ESKELETON_INFO::ESI_ACTOR);
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
		ImGui::CheckboxFlags("Draw PH boxes", &flags.flags, LevelInspector::ESKELETON_INFO::ESI_PH_BBOX);
	}

	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
	ImGui::CheckboxFlags("Draw bounding boxes", &flags.flags, LevelInspector::ESKELETON_INFO::ESI_BBOXES);
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
	ImGui::CheckboxFlags("Draw main bounding boxes", &flags.flags, LevelInspector::ESKELETON_INFO::ESI_MAIN_BBOX);
}
ICF void setkey(u32& zbufKey, u8 idx, const char* label_name = "ZBuff")
{
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
	static bool waitingKey[2] = {false, false};

	if (waitingKey[idx])
	{
		ImGui::TextColored(ImVec4(1.0f, 0.8f, 0.0f, 1.0f), "Press any key...");
		for (u32 k = SDL_SCANCODE_A; k < SDL_SCANCODE_WAKE; ++k)
		{
			if (pInput->iGetAsyncKeyState(k))
			{
				zbufKey = SDL_Scancode(k);
				waitingKey[idx] = false;
				break;
			}
		}
	}
	else
	{
		if (ImGui::Button(*shared_str().printf("%s disable Key: %s", label_name, SDL_GetScancodeName(SDL_Scancode(zbufKey)))))
			waitingKey[idx] = true;
	}
}

ICF void fontselectioncombo(LevelInspector& LI)
{
	ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
	static xr_concurrent_vector<const char*> font_names;
	static int curr_idx = 0;

	if (font_names.empty() && !g_FontManager->Fonts.empty())
	{
		font_names.reserve(g_FontManager->Fonts.size());
		xr_parallel_foreach(g_FontManager->Fonts.begin(), g_FontManager->Fonts.end(),
		[&](const std::pair<const shared_str, CGameFont*>& pair)
		{
			if (!pSettings->section_exist(pair.first.c_str()))
				return;
			LI.m_clone_fonts_map[pair.first] = new CGameFont(pair.first.c_str());
			font_names.push_back(pair.first.c_str());
		});
	}

	if (!font_names.empty())
	{
		if (ImGui::Combo("##SelectFontCombo", &curr_idx, &font_names[0], font_names.size()))
			LI.dbg_font = LI.m_clone_fonts_map[font_names[curr_idx]];

		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
		ImGui::SliderFloat("##FontSpacing", &LI.font_spacing, 0.0f, 10.f);
		LI.dbg_font->SetLineSpacing(LI.font_spacing);
	}
}

LevelInspector::LevelInspector(BOOL hm) : hud_mode(hm)
{
	shader->create("portal");

	dbg_font = g_FontManager->Fonts[shared_str("ui_font_letterica18_russian")];
	
	if(!hud_mode)
	{
		hud_prims = new LevelInspector(TRUE);

		CImGuiManager::Instance().Subscribe("LevelInspector", CImGuiManager::ERenderPriority::eMedium, [this]()
			{
				static bool& isOpen = Engine.External.EditorStates[static_cast<u8>(EditorUI::LevelInspector)];
				if (!isOpen) return;

				ImGui::Begin("LevelInspector", &isOpen);

				ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.1f, 0.1f, 0.12f, 0.3f));
				ImGui::BeginChild("##MainFlags", ImVec2(0, ImGui::GetFrameHeightWithSpacing() * 11));

				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
				ImGui::CheckboxFlags("Draw Inspector", &m_flags.flags, ESCENE_FLAGS::ESF_DRAW);

				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
				ImGui::CheckboxFlags("Draw HUD", &m_flags.flags, ESCENE_FLAGS::ESF_DRAW_HUD);

				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
				ImGui::CheckboxFlags("Draw Objects", &m_flags.flags, ESCENE_FLAGS::ESF_DRAW_OBJECTS);

				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
				ImGui::CheckboxFlags("Draw Zones", &m_flags.flags, ESCENE_FLAGS::ESF_DRAW_ZONES);

				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
				ImGui::CheckboxFlags("Draw Selected", &m_flags.flags, ESCENE_FLAGS::ESF_DRAW_SELECTION);

				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
				ImGui::CheckboxFlags("Draw AIPaths", &m_flags.flags, ESCENE_FLAGS::ESF_DRAW_AI_PATHS);

				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
				ImGui::CheckboxFlags("Draw GameGraph", &m_flags.flags, ESCENE_FLAGS::ESF_DRAW_G_GRID);

				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
				ImGui::CheckboxFlags("Draw WayPoints", &m_flags.flags, ESCENE_FLAGS::ESF_DRAW_W_GRID);

				ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 10);
				ImGui::CheckboxFlags("Draw LevelGraph", &m_flags.flags, ESCENE_FLAGS::ESF_DRAW_L_GRID);

				setkey(zbuffer_key, 0);
				hud_prims->zbuffer_key = zbuffer_key;
				setkey(visible_currents_key, 1, "ZBuffText");

				ImGui::EndChild();
				ImGui::PopStyleColor();
				ImGui::Spacing();

				if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_HUD) && ImGui::CollapsingHeader("HUD"))
				{
					ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.12f, 0.1f, 0.1f, 0.3f));
					ImGui::BeginChild("##HUDGroup", ImVec2(0, ImGui::GetFrameHeightWithSpacing() * 6));

					RenderSkeletonFlags(hud_prims->m_skeleton_flags, true);

					ImGui::EndChild();
					ImGui::PopStyleColor();
					ImGui::Spacing();
				}

				if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_OBJECTS) && ImGui::CollapsingHeader("Objects"))
				{
					ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.1f, 0.12f, 0.1f, 0.3f));
					ImGui::BeginChild("##ObjectsGroup", ImVec2(0, ImGui::GetFrameHeightWithSpacing() * 8));

					RenderSkeletonFlags(m_skeleton_flags, false);

					ImGui::EndChild();
					ImGui::PopStyleColor();
					ImGui::Spacing();
				}

				if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_ZONES) && ImGui::CollapsingHeader("Zones"))
				{
					ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.1f, 0.12f, 0.1f, 0.3f));
					ImGui::BeginChild("##ZonesGroup", ImVec2(0, ImGui::GetFrameHeightWithSpacing() * 7));

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					ImGui::CheckboxFlags("Draw Restrictor", &m_zone_flags.flags, EZONE_INFO::EZI_RESTR);

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					ImGui::CheckboxFlags("Draw AnomalyZone", &m_zone_flags.flags, EZONE_INFO::EZI_ANOMALY_ZONE);

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					ImGui::CheckboxFlags("Draw AnomalyZoneLogic", &m_zone_flags.flags, EZONE_INFO::EZI_ANOMAL_ZONE_LOGIC);

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					ImGui::CheckboxFlags("Draw CampZone", &m_zone_flags.flags, EZONE_INFO::EZI_CAMP_ZONE);

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					ImGui::CheckboxFlags("Draw LevelChanger", &m_zone_flags.flags, EZONE_INFO::EZI_LEVEL_CHANGER);

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					ImGui::CheckboxFlags("Draw SmartCover", &m_zone_flags.flags, EZONE_INFO::EZI_SMART_COVER);

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					ImGui::CheckboxFlags("Draw SmartTerrain", &m_zone_flags.flags, EZONE_INFO::EZI_SMART_TERRAIN);

					ImGui::EndChild();
					ImGui::PopStyleColor();
					ImGui::Spacing();
				}


				if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_W_GRID) && ImGui::CollapsingHeader("WayPointsFilter"))
				{
					ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.1f, 0.1f, 0.12f, 0.3f));
					ImGui::BeginChild("##wp_flGroup", ImVec2(0, ImGui::GetFrameHeightWithSpacing() * 4));

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					if (ImGui::CheckboxFlags("Draw by prefix", &m_waypoint_flags.flags, EWAYPOINT_INFO::EWI_PREFIX))
						wp_recalc = true;

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					ImGui::BeginDisabled(!m_waypoint_flags.test(EWAYPOINT_INFO::EWI_PREFIX));
						static string64 temp_str = "zat";
						if (ImGui::InputTextWithHint("##wp_fltext", "Prefix", temp_str, sizeof(temp_str)))
							wp_recalc = true;
						wp_prefix = temp_str;
					ImGui::EndDisabled();

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					if (ImGui::CheckboxFlags("Draw by location id", &m_waypoint_flags.flags, EWAYPOINT_INFO::EWI_LICATION_ID))
						wp_recalc = true;

					ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
					if (ImGui::CheckboxFlags("Draw All", &m_waypoint_flags.flags, EWAYPOINT_INFO::EWI_ALL))
						wp_recalc = true;

					ImGui::EndChild();
					ImGui::PopStyleColor();
					ImGui::Spacing();
				}

				if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_SELECTION) && ImGui::CollapsingHeader("Selection"))
				{
					ImGui::PushStyleColor(ImGuiCol_ChildBg, ImVec4(0.1f, 0.1f, 0.12f, 0.3f));
					ImGui::BeginChild("##SelectionGroup", ImVec2(0, ImGui::GetFrameHeightWithSpacing() * 16));

					if(m_flags.test(ESCENE_FLAGS::ESF_DRAW_OBJECTS))
					{
						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Select Object", &m_selection_flags.flags, ESELECTION_FLAGS::ESLF_O);
					}

					if(m_flags.test(ESCENE_FLAGS::ESF_DRAW_ZONES))
					{
						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Select Zone", &m_selection_flags.flags, ESELECTION_FLAGS::ESLF_Z);
					}

					if(m_flags.test(ESCENE_FLAGS::ESF_DRAW_W_GRID))
					{
						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Select WayPoint", &m_selection_flags.flags, ESELECTION_FLAGS::ESLF_WP);
					}

					if(m_flags.test(ESCENE_FLAGS::ESF_DRAW_G_GRID))
					{

						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Select GraphPoint", &m_selection_flags.flags, ESELECTION_FLAGS::ESLF_GP);
					}

					if(m_flags.test(ESCENE_FLAGS::ESF_DRAW_L_GRID))
					{
						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Select AINode", &m_selection_flags.flags, ESELECTION_FLAGS::ESLF_LG);
					}

					if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_OBJECTS) || m_flags.test(ESCENE_FLAGS::ESF_DRAW_ZONES))
					{
						ImGui::Separator();

						fontselectioncombo(*this);

						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Draw Section Name", &m_selection_text_flags.flags, EOBJECT_INFO::EOI_SNAME);

						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Draw Class Name", &m_selection_text_flags.flags, EOBJECT_INFO::EOI_LCNAME);

						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Draw Name", &m_selection_text_flags.flags, EOBJECT_INFO::EOI_LNAME);

						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Draw Visual Name", &m_selection_text_flags.flags, EOBJECT_INFO::EOI_VNAME);

						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Draw Custom Data", &m_selection_text_flags.flags, EOBJECT_INFO::EOI_INI);

						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Draw Position", &m_selection_text_flags.flags, EOBJECT_INFO::EOI_POSITION);

						ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
						ImGui::CheckboxFlags("Draw GVertexID & LVertexID", &m_selection_text_flags.flags, EOBJECT_INFO::EOI_GVERTEX_LVERTEX);

						if(m_flags.test(ESCENE_FLAGS::ESF_DRAW_OBJECTS))
						{
							ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15);
							ImGui::CheckboxFlags("Draw Actor", &m_selection_text_flags.flags, EOBJECT_INFO::EOI_ACTOR);
						}
					}

					ImGui::EndChild();
					ImGui::PopStyleColor();
					ImGui::Spacing();
				}

				ImGui::End();
			});
	}
}

LevelInspector::~LevelInspector()
{
	if (!hud_mode)
		CImGuiManager::Instance().Unsubscribe("LevelInspector");
	xr_delete(hud_prims);

	for (const auto& pair : m_clone_fonts_map)
	{
		xr_delete(pair.second);
	}
}

void LevelInspector::OnRender()
{
	PROF_EVENT(__FUNCTION__);


	if (!hud_mode && !m_flags.test(ESCENE_FLAGS::ESF_DRAW))
		return;

	zbuffer_enable = !pInput->iGetAsyncKeyState(zbuffer_key);
	if (!hud_mode)
		visible_currents = pInput->iGetAsyncKeyState(visible_currents_key);

	if (hud_mode)
	{
		DrawHud();
	}
	else
	{
		if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_HUD))
			hud_prims->OnRender();
		if(m_flags.test(ESCENE_FLAGS::ESF_DRAW) && m_flags.test(ESCENE_FLAGS::ESF_DRAW_AI_PATHS))
			DrawAIPaths();
		if(m_flags.test(ESCENE_FLAGS::ESF_DRAW) && m_flags.test(ESCENE_FLAGS::ESF_DRAW_SELECTION))
			DrawObjectsInfo();
		if(psDeviceFlags.test(rsDrawDynamic))
			DrawObjects();
		if(m_flags.test(ESCENE_FLAGS::ESF_DRAW) && m_flags.test(ESCENE_FLAGS::ESF_DRAW_G_GRID))
			DrawGameGraph();
		if(m_flags.test(ESCENE_FLAGS::ESF_DRAW) && m_flags.test(ESCENE_FLAGS::ESF_DRAW_W_GRID))
			DrawWayPoints();
	}

	bool tris_empty = tris.empty();
	bool lines_empty = lines.empty();
	if (!tris_empty || !lines_empty)
	{
		if (hud_mode)
		{
			if (Device.m_pRender)
			{
				Device.m_pRender->SetCacheXform(Device.mView_hud, Device.mProject_hud);
				Device.m_pRender->SetCacheXformOld(Device.mView_hud_old, Device.mProject_hud_old);
			}
			UIRender->CacheSetXformWorld(Device.mFullTransform_hud);
			Render->rmNear();
		}

		UIRender->SetShader(*shader);
		UIRender->CacheSetCullMode(ERHI_CULLMODE::NONE);
		UIRender->zb_enable(zbuffer_enable);

		if (!tris_empty)
		{
			u32 MAX_TRIS_PER_BATCH = 58'000u;
			u32 total_tris = (u32)tris.size();

			for (u32 start_idx = 0u; start_idx < total_tris; start_idx += MAX_TRIS_PER_BATCH)
			{
				u32 total = total_tris - start_idx;
				u32 batch_size = std::min(MAX_TRIS_PER_BATCH, total);

				IUIRender::LITFast** buffer = UIRender->StartPrimitiveLITFast(batch_size * 3u, IUIRender::ptTriList);
				for (size_t i = 0u; i < batch_size; ++i)
				{
					tvertex& tri = tris[start_idx + i];
					(*buffer)->p = tri.v1;
					(*buffer)->color = tri.color;
					(*buffer)++;

					(*buffer)->p = tri.v2;
					(*buffer)->color = tri.color;
					(*buffer)++;

					(*buffer)->p = tri.v3;
					(*buffer)->color = tri.color;
					(*buffer)++;
				}

				UIRender->FlushPrimitive();
			}

			tris.clear();
		}

		if (!lines_empty)
		{
			u32 MAX_LINES_PER_BATCH = 87'000u;
			u32 total_lines = (u32)lines.size();

			for (u32 start_idx = 0u; start_idx < total_lines; start_idx += MAX_LINES_PER_BATCH)
			{
				u32 total = total_lines - start_idx;
				u32 batch_size = std::min(MAX_LINES_PER_BATCH, total);

				IUIRender::LITFast** buffer = UIRender->StartPrimitiveLITFast(batch_size * 2u, IUIRender::ptLineList);

				for (u32 i = 0u; i < batch_size; ++i)
				{
					lvertex& line = lines[start_idx + i];

					(*buffer)->p = line.v1;
					(*buffer)->color = line.color;
					(*buffer)++;

					(*buffer)->p = line.v2;
					(*buffer)->color = line.color;
					(*buffer)++;
				}

				UIRender->FlushPrimitive();
			}

			lines.clear();
		}

		UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);

		if (hud_mode)
		{
			Render->rmNormal();
			if (Device.m_pRender)
			{
				Device.m_pRender->SetCacheXform(Device.mView, Device.mProject);
				Device.m_pRender->SetCacheXformOld(Device.mView_old, Device.mProject_old);
			}
			UIRender->CacheSetXformWorld(Device.mFullTransform);
		}
	}

	if (!hud_mode && m_flags.test(ESCENE_FLAGS::ESF_DRAW_L_GRID))
		DrawLevelGraph();

	dbg_font->OnRender();
}

void LevelInspector::DrawWayPoints()
{
	PROF_EVENT(__FUNCTION__);
	constexpr float WAYPOINT_SIZE = 1.5f;
	constexpr float WAYPOINT_HEIGHT = WAYPOINT_SIZE * .85f;
	constexpr float WAYPOINT_RADIUS = WAYPOINT_SIZE * .5f;
	constexpr float WAYPOINT_LOW = WAYPOINT_SIZE * 0.15f;
	constexpr u32 WAYPOINT_COLOR = color_rgba(0, 255, 0, 255);
	constexpr u32 WAYPOINT_COLOR_RED = color_rgba(255, 0, 0, 255);
	constexpr u32 WAYPOINT_COLOR_SELECTED = color_rgba(255, 255, 255, 255);
	constexpr u32 WAYPOINT_LINK_COLOR = color_rgba(230, 155, 75, 75);
	constexpr u32 WAYPOINT_LINK_COLOR_SELECTED = color_rgba(255, 250, 180, 255);

	float RENDER_DISTANCE = visible_currents ? 50000.f : g_pGamePersistent->pEnvironment->CurrentEnv->fog_distance;
	float RENDER_DISTANCE_SQR = RENDER_DISTANCE * RENDER_DISTANCE;
	static Fvector& cam_pos = Device.vCameraPosition;
	cam_pos = Device.vCameraPosition;
	Fvector& cam_dir = Device.vCameraDirection;
	CFrustum& view_base = ::Render->ViewBase;

	IGameGraph& graph = ai().game_graph();
	ILevelGraph& lgraph = ai().level_graph();
	u8 curr_level_id = lgraph.level_id();
	static u8 last_level_id = curr_level_id;

	struct WayPoint
	{
		shared_str path_name;
		shared_str name;
		shared_str level_name;
		xr_vector<shared_str> m_flags;
		xr_vector<Fvector> m_links;
		Fvector pos, spatial_c;// , spatial_bd;
		float spatial_r = 1.f;
		Fobb obb;
		u32 color = color_rgba(0, 255, 0, 255);// WAYPOINT_COLOR
	};

	static xr_vector<WayPoint> m_way_points;

	if (last_level_id != curr_level_id || wp_recalc)
	{
		m_way_points.clear();
		last_level_id = curr_level_id;
		wp_recalc = false;
	}

	if (m_waypoint_flags.flags == EWAYPOINT_INFO::EWI_NONE)
		return;

	if(m_way_points.empty())
	{
		shared_str curr_level_name = graph.header().level(curr_level_id).name();

		CPatrolPathStorage::PATROL_REGISTRY& paths = const_cast<CPatrolPathStorage::PATROL_REGISTRY&>(ai().patrol_paths().patrol_paths());
		for (auto& [name, path] : paths)
		{
			if (!m_waypoint_flags.test(EWAYPOINT_INFO::EWI_ALL) && m_waypoint_flags.test(EWAYPOINT_INFO::EWI_PREFIX))
			{
				if (wp_prefix && name && name.c_str() != strstr(name.c_str(), wp_prefix.c_str()))
					continue;
			}

			auto& vertices = path->vertices();
			for (auto& [id, vertex] : vertices)
			{
				CPatrolPoint& point = vertex->data();

				const IGameGraph::CVertex* ggvertex = graph.vertex(point.game_vertex_id());
				const ILevelGraph::CVertex* lgvertex = lgraph.vertex(point.level_vertex_id());
				u8 level_id = ggvertex->level_id();

				if (!m_waypoint_flags.test(EWAYPOINT_INFO::EWI_ALL) && m_waypoint_flags.test(EWAYPOINT_INFO::EWI_LICATION_ID))
				{
					if (level_id != curr_level_id)
						continue;
				}

				Fbox bbox; bbox.invalidate();
				WayPoint& wp = m_way_points.emplace_back();

				auto it = graph.header().levels().find(level_id);
				if(it != graph.header().levels().end())
					wp.level_name = it->second.m_name;

				Flags32 flags; flags.flags = point.flags();
				u32 mask = 1;
				const int numBits = sizeof(u32) * 8;

				for (int i = 0; i < numBits; i++)
				{
					if (flags.test(mask))
						wp.m_flags.push_back(shared_str().printf("flag: %d", i));
					mask <<= 1;
				}

				Fvector pos = point.position();
				pos.y += WAYPOINT_HEIGHT;
				wp.pos = pos;
				pos.y -= WAYPOINT_RADIUS - WAYPOINT_LOW;
				wp.path_name = name;
				wp.name = point.name();
				if (level_id != curr_level_id)
					wp.color = WAYPOINT_COLOR_RED;
				Fbox box;
				box.setb(pos, { WAYPOINT_RADIUS , WAYPOINT_RADIUS , WAYPOINT_RADIUS });
				bbox.merge(box);
				wp.obb.identity();
				box.get_CD(wp.obb.m_translate, wp.obb.m_halfsize);
				auto& m_edges = vertex->edges();
				for (auto& edge : m_edges)
				{
					Fvector child_pos = edge.vertex()->data().position();

					bbox.modify(child_pos);
					child_pos.y += WAYPOINT_HEIGHT;
					bbox.modify(child_pos);
					wp.m_links.push_back(child_pos);
				}
				//bbox.get_CD(wp.spatial_c, wp.spatial_bd);
				bbox.getcenter(wp.spatial_c);
				wp.spatial_r += bbox.getradius()+WAYPOINT_RADIUS;
			}
		}
	}
	constexpr u32 color_green = color_rgba(0, 255, 100, 255);
	constexpr float sz = 0.25f;
	Fvector pp[2], D, R, N;
	static xr_vector<WayPoint*> way_points_z_sorting; way_points_z_sorting.clear();
	for (WayPoint& wp : m_way_points)
	{
		Fvector& spatial_c = wp.spatial_c;
		float spatial_r = wp.spatial_r;

		if (cam_pos.distance_to_sqr(spatial_c) > _sqr(RENDER_DISTANCE + spatial_r))
			continue;

		if (!visible_currents && !view_base.testSphere_dirty(spatial_c, spatial_r))
			continue;
		way_points_z_sorting.push_back(&wp);
	}
	std::sort(way_points_z_sorting.begin(), way_points_z_sorting.end(),
	[](const WayPoint* a, const WayPoint* b){return cam_pos.distance_to_sqr(a->pos) < cam_pos.distance_to_sqr(b->pos);});

	for (WayPoint* wp : way_points_z_sorting)
	{
		//Fobb obb; obb.identity();
		//obb.m_translate = spatial_c;
		//obb.m_halfsize = wp.spatial_bd;
		//append_obb(obb, color_rgba(0, 255, 0, 150));
		bool selected = false;
		Fvector& wppos = wp->pos;
		if(m_flags.test(ESCENE_FLAGS::ESF_DRAW_SELECTION) && m_selection_flags.test(ESELECTION_FLAGS::ESLF_WP))
		{
			float range = RD.range;
			if (visible_currents || (!RQ.O && wp->obb.intersect(cam_pos, cam_dir, range)))
			{
				bool text3d = append_text3d(wppos, wp->path_name, wp->color, CGameFont::alLeft);
				if(text3d)
					append_text_next(wp->name);
				else
					text3d = append_text3d(wppos, wp->name, wp->color, CGameFont::alLeft);
				for (auto& flag : wp->m_flags)
				{
					if (text3d)
						append_text_next(flag);
					else
						text3d = append_text3d(wppos, flag, wp->color, CGameFont::alLeft);
				}
				if(wp->level_name)
				{
					if (text3d)
						append_text_next(wp->level_name);
					else
						text3d = append_text3d(wppos, wp->level_name, wp->color, CGameFont::alLeft);
				}
				//draw selection box
				{
					Fmatrix transform;
					wp->obb.xform_full(transform);
					transform.k.mul(2.f);
					transform.i.mul(2.f);
					transform.j.mul(2.f);
					Fvector world_vertices[std::size(aabb_selection_vertices)];
					for (int i = 0; i < std::size(aabb_selection_vertices); i++)
						transform.transform_tiny(world_vertices[i], aabb_selection_vertices[i]);

					for (lindex& idx : aabb_selection_lindices)
						append_line({ world_vertices[idx.i1], world_vertices[idx.i2], WAYPOINT_COLOR_SELECTED });
				}

				selected = true;
				if(!visible_currents)
				{
					RQ.O = (CGameObject*)0xFF;
					RD.range = range;
				}
			}
		}

		append_line({ {wppos.x + WAYPOINT_RADIUS,wppos.y,wppos.z}, {wppos.x - WAYPOINT_RADIUS,wppos.y,wppos.z},wp->color });
		append_line({ {wppos.x,wppos.y + WAYPOINT_LOW,wppos.z}, {wppos.x,wppos.y - WAYPOINT_HEIGHT,wppos.z},wp->color });
		append_line({ {wppos.x,wppos.y,wppos.z + WAYPOINT_RADIUS}, {wppos.x,wppos.y,wppos.z - WAYPOINT_RADIUS},wp->color });
		u32 clr = selected ? WAYPOINT_LINK_COLOR_SELECTED : WAYPOINT_LINK_COLOR;
		xr_vector<Fvector>& links = wp->m_links;
		for (Fvector& link : links)
		{
			append_line({link, wppos, clr});
			N = { 0,1,0 };
			D.sub(link, wppos); D.normalize();
			R.crossproduct(N, D); R.mul(0.5f); D.mul(2.0f); N.mul(0.5f);
			// LR
			pp[0].add(R, D); pp[0].mul(sz * -0.5f);	pp[0].add(link);
			R.invert();
			pp[1].add(R, D); pp[1].mul(sz * -0.5f);	pp[1].add(link);
			append_line({link, pp[0], clr});
			append_line({link, pp[1], clr});
			// UB
			pp[0].add(N, D); pp[0].mul(sz * -0.5f);	pp[0].add(link);
			N.invert();
			pp[1].add(N, D); pp[1].mul(sz * -0.5f);	pp[1].add(link);
			append_line({link, pp[0], clr});
			append_line({link, pp[1], clr});
		}
	}
}

void PostProcessPath(
	xr_vector<u32>& path,
	float max_slope_angle = 2.0f,
	int min_points_on_straight = 5,
	bool enable_slope_check = true,
	bool enable_aggressive_smoothing = true)
{
	ILevelGraph& lgraph = ai().level_graph();

	if (path.size() < 3)
		return;

	thread_local xr_vector<u32> stage1;
	stage1.clear();
	stage1.reserve(path.size());

	thread_local xr_vector<Fvector> positions;
	if (positions.size() < path.size())
		positions.resize(path.size());

	for (size_t i = 0; i < path.size(); ++i)
		positions[i] = lgraph.vertex_position(path[i]);

	stage1.push_back(path[0]);
	size_t last_kept = 0;

	for (size_t i = 1; i < path.size() - 1; ++i)
	{
		if (!lgraph.check_vertex_in_direction(
			path[last_kept],
			positions[last_kept],
			path[i + 1]))
		{
			stage1.push_back(path[i]);
			last_kept = i;
		}
	}

	stage1.push_back(path.back());

	if (stage1.size() < 3 || !enable_aggressive_smoothing)
	{
		path = stage1;
		return;
	}

	thread_local xr_vector<u32> stage2;
	stage2.clear();
	stage2.reserve(stage1.size());

	thread_local xr_vector<Fvector> pos_cache;
	thread_local xr_vector<Fvector> norm_cache;

	if (pos_cache.size() < stage1.size())
	{
		pos_cache.resize(stage1.size());
		norm_cache.resize(stage1.size());
	}

	for (size_t i = 0; i < stage1.size(); ++i)
	{
		pos_cache[i] = lgraph.vertex_position(stage1[i]);

		if (enable_slope_check)
		{
			const ILevelGraph::CVertex& vertex = *lgraph.vertex(stage1[i]);
			Fplane plane;
			pvDecompress(plane.n, vertex.plane());
			norm_cache[i] = plane.n;
			norm_cache[i].normalize_safe();
		}
	}

	auto check_slope = [&](size_t idx1, size_t idx2) -> bool
		{
			if (!enable_slope_check || max_slope_angle >= 89.0f)
				return true;

			float dot = norm_cache[idx1].dotproduct(norm_cache[idx2]);
			float angle = acosf(clampr(dot, -1.0f, 1.0f)) * 180.0f / PI;
			return angle <= max_slope_angle;
		};

	size_t current = 0;
	stage2.push_back(stage1[current]);

	int straight_segment_length = 1;

	while (current < stage1.size() - 1)
	{
		size_t farthest = current + 1;
		const Fvector& current_pos = pos_cache[current];

		size_t max_search = std::min(current + 8, stage1.size() - 1);

		for (size_t test = max_search; test > current + 1; --test)
		{
			if (!lgraph.check_vertex_in_direction(
				stage1[current], current_pos, stage1[test]))
				continue;

			if (!check_slope(current, test))
				continue;

			bool slopes_ok = true;
			if (enable_slope_check)
			{
				for (size_t mid = current + 1; mid < test; ++mid)
				{
					if (!check_slope(mid - 1, mid))
					{
						slopes_ok = false;
						break;
					}
				}
			}

			if (slopes_ok)
			{
				farthest = test;
				break;
			}
		}

		if (farthest > current + 1)
		{
			if (straight_segment_length < min_points_on_straight)
			{
				size_t next_point = current + 1;
				stage2.push_back(stage1[next_point]);
				current = next_point;
				straight_segment_length++;
				continue;
			}

			if (stage2.size() >= 2)
			{
				u32 prev_id = stage2[stage2.size() - 2];
				u32 curr_id = stage2.back();
				u32 next_id = stage1[farthest];

				size_t prev_idx = 0, curr_idx = 0;
				for (size_t i = 0; i < stage1.size(); ++i)
				{
					if (stage1[i] == prev_id) prev_idx = i;
					if (stage1[i] == curr_id) curr_idx = i;
					if (stage1[i] == next_id) break;
				}

				const Fvector& prev_pos = pos_cache[prev_idx];
				const Fvector& curr_pos = pos_cache[curr_idx];
				const Fvector& next_pos = pos_cache[farthest];

				Fvector v1, v2;
				v1.sub(curr_pos, prev_pos);
				v2.sub(next_pos, curr_pos);

				v1.normalize_safe();
				v2.normalize_safe();

				float dot = v1.dotproduct(v2);

				if (dot < -0.866f)
				{
					size_t mid = (current + farthest) / 2;
					stage2.push_back(stage1[mid]);
					current = mid;
					straight_segment_length = 1;
					continue;
				}
			}

			current = farthest;
			straight_segment_length = 1;
		}
		else
		{
			current++;
			straight_segment_length++;
		}

		stage2.push_back(stage1[current]);

		if (stage2.size() > stage1.size() * 3)
			break;
	}

	if (stage2.size() > 1 && stage2.back() == stage2[stage2.size() - 2])
		stage2.pop_back();

	thread_local xr_vector<u32> final_path;
	final_path.clear();
	final_path.reserve(stage2.size());

	if (stage2.size() >= 3)
	{
		final_path.push_back(stage2[0]);

		for (size_t i = 1; i < stage2.size() - 1; ++i)
		{
			u32 prev = final_path.back();
			u32 curr = stage2[i];
			u32 next = stage2[i + 1];

			if (!lgraph.check_vertex_in_direction(
				prev,
				lgraph.vertex_position(prev),
				next))
			{
				final_path.push_back(curr);
			}
		}

		final_path.push_back(stage2.back());
	}
	else
	{
		final_path = stage2;
	}

	path = final_path;
}

bool SearchSmooth(u32 start_vertex_id, u32 dest_vertex_id, xr_vector<u32>& OutPath, float MaxRange = type_max(float), u32 MaxIterationCount = 0xFFFFFFFF, u32 MaxVisitedNodeCount = 0xFFFFFFFF)
{
	ILevelGraph& lgraph = ai().level_graph();

	if (start_vertex_id == dest_vertex_id)
	{
		OutPath.push_back(start_vertex_id);
		return true;
	}

	struct NodeRecord
	{
		u32 id;
		float g;
		float h;
		float f;
		u32 parent;

		bool operator>(const NodeRecord& other) const
		{
			return f > other.f;
		}
	};

	const u32 max_nodes = lgraph.header().vertex_count();

	thread_local xr_vector<float> g_costs;
	thread_local xr_vector<u32> parents;
	thread_local xr_vector<bool> closed;

	if (g_costs.size() < max_nodes)
	{
		g_costs.resize(max_nodes, flt_max);
		parents.resize(max_nodes, u32(-1));
		closed.resize(max_nodes, false);
	}

	std::fill(g_costs.begin(), g_costs.begin() + max_nodes, flt_max);
	std::fill(parents.begin(), parents.begin() + max_nodes, u32(-1));
	std::fill(closed.begin(), closed.begin() + max_nodes, false);

	std::priority_queue<NodeRecord, xr_vector<NodeRecord>, std::greater<NodeRecord>> open;

	Fvector target_pos = lgraph.vertex_position(dest_vertex_id);
	const float cell_size = lgraph.header().cell_size();

	auto euclidean_heuristic = [](u32 vertex_id, Fvector& target_pos) -> float
	{
		ILevelGraph& lgraph = ai().level_graph();
		Fvector pos = lgraph.vertex_position(vertex_id);
		return pos.distance_to(target_pos);
	};

	g_costs[start_vertex_id] = 0;
	parents[start_vertex_id] = start_vertex_id;

	float start_h = euclidean_heuristic(start_vertex_id, target_pos);
	open.push({ start_vertex_id, 0, start_h, start_h, start_vertex_id });

	u32 iterations = 0;
	u32 visited_count = 0;

	while (!open.empty() && iterations < MaxIterationCount)
	{
		NodeRecord current = open.top();
		open.pop();

		if (closed[current.id]) continue;

		if (current.id == dest_vertex_id)
		{
			OutPath.clear();
			u32 node = dest_vertex_id;
			while (node != start_vertex_id)
			{
				OutPath.push_back(node);
				node = parents[node];
			}
			OutPath.push_back(start_vertex_id);
			std::reverse(OutPath.begin(), OutPath.end());

			PostProcessPath(OutPath);
			return true;
		}

		closed[current.id] = true;
		visited_count++;

		if (visited_count >= MaxVisitedNodeCount)
			break;

		if (MaxRange < type_max(float))
		{
			Fvector current_pos = lgraph.vertex_position(current.id);
			if (current_pos.distance_to(target_pos) > MaxRange)
				continue;
		}

		ILevelGraph::CVertex* vertex = lgraph.vertex(current.id);

		for (s32 i = 0; i < 4; ++i)
		{
			u32 neighbor_id = vertex->link(i);

			if (!lgraph.valid_vertex_id(neighbor_id))
				continue;

			if (closed[neighbor_id]) continue;

			float movement_cost = cell_size;

			float new_g = current.g + movement_cost;

			if (new_g < g_costs[neighbor_id])
			{
				g_costs[neighbor_id] = new_g;
				parents[neighbor_id] = current.id;

				float h = euclidean_heuristic(neighbor_id, target_pos);
				float f = new_g + h;

				open.push({ neighbor_id, new_g, h, f, current.id });
			}
		}

		iterations++;
	}

	return false;
}

void LevelInspector::DrawGameGraph()
{
	PROF_EVENT(__FUNCTION__);
	IGameGraph& graph = ai().game_graph();
	ILevelGraph& lgraph = ai().level_graph();
	u8 curr_level_id = lgraph.level_id();
	static u8 last_level_id = curr_level_id;

	struct GraphPointD
	{
		bool is_blue[2]{ false, false };
		xr_concurrent_vector<Fvector> m_path;
		Fvector pos[3];
		Fvector spatial_c, spatial_bd;
		float spatial_r = 1.f;
	};

	struct GraphPointC
	{
		Fobb obb;
		Fvector pos;
		shared_str next_level;
		u32 gvid = u32(-1);
		float spatial_r = 1.f;
		bool is_blue = false;
	};

	struct EdgeKey
	{
		u16 min_vertex;
		u16 max_vertex;

		EdgeKey(u16 v1, u16 v2)
			: min_vertex(std::min(v1, v2))
			, max_vertex(std::max(v1, v2))
		{
		}

		bool operator<(const EdgeKey& other) const
		{
			if (min_vertex != other.min_vertex)
				return min_vertex < other.min_vertex;
			return max_vertex < other.max_vertex;
		}

		bool operator==(const EdgeKey& other) const
		{
			return min_vertex == other.min_vertex &&
				max_vertex == other.max_vertex;
		}
	};

	struct EdgeKeyHash
	{
		std::size_t operator()(const EdgeKey& key) const noexcept
		{
			return (static_cast<std::size_t>(key.min_vertex) << 16) ^
				static_cast<std::size_t>(key.max_vertex);
		}
	};

	static xr_concurrent_unordered_map<EdgeKey, GraphPointD, EdgeKeyHash> m_links;
	static xr_concurrent_unordered_map<u16, GraphPointC> m_graphs;
	if (last_level_id != curr_level_id)
	{
		m_links.clear();
		m_graphs.clear();
		last_level_id = curr_level_id;
	}

	if(m_links.empty() && m_graphs.empty())
	{
		u16 v_cnt = graph.header().vertex_count();
		static u32 main_thread_id = GetCurrentThreadId();
		xr_parallel_for(u16(0), v_cnt, [&](u16 global_vid)
		{
			PROF_START_THREAD("build_graph_nodes");
			PROF_EVENT("build_graph_points_and_links");
			const IGameGraph::CVertex* global_vertex = graph.vertex(global_vid);

			if (global_vertex->level_id() == lgraph.level_id())
			{
				Fvector main_graph_pos = global_vertex->level_point();
				GraphPointC& gr = m_graphs[global_vid];
				gr.is_blue = global_vertex->vertex_type()[3];
				gr.pos = main_graph_pos;
				Fbox bbox; bbox.identity();
				bbox.setb(main_graph_pos, {0.5f, 1.0f, 0.5f});
				gr.obb.identity();
				bbox.get_CD(gr.obb.m_translate, gr.obb.m_halfsize);
				//bbox.getcenter(gr.spatial_c);
				gr.spatial_r += bbox.getradius();
				gr.gvid = global_vid;
				IGameGraph::const_iterator VI2, E;
				graph.begin(global_vid, VI2, E);

				for (; VI2 != E; ++VI2)
				{
					const IGameGraph::CVertex* child_vertex = graph.vertex((u32)VI2->vertex_id());
					if (child_vertex->level_id() != lgraph.level_id())
					{
						auto it = graph.header().levels().find(child_vertex->level_id());
						if (it != graph.header().levels().end())
							gr.next_level = it->second.name();
						continue;
					}

					if (!lgraph.valid_vertex_id(global_vertex->level_vertex_id()) || !lgraph.valid_vertex_id(child_vertex->level_vertex_id()))
                        continue;

					auto it = m_links.find({ global_vid, VI2->vertex_id() });
					if (it != m_links.end())
						continue;

					auto& link = m_links[EdgeKey{ global_vid, VI2->vertex_id() }];
					link.is_blue[0] = global_vertex->vertex_type()[3];
					link.is_blue[1] = child_vertex->vertex_type()[3];

					Fvector child_pos = child_vertex->level_point();

					link.pos[0] = main_graph_pos + Fvector{0.f,1.f,0.f};
					link.pos[1] = child_pos + Fvector{ 0.f,1.f,0.f };
					link.pos[2].add(main_graph_pos, child_pos).mul(0.5f);

					thread_local xr_vector<u32> m_path; m_path.clear();
					//lgraph.Search(global_vertex->level_vertex_id(), child_vertex->level_vertex_id(), m_path);
					//PostProcessPath(m_path);
					SearchSmooth(global_vertex->level_vertex_id(), child_vertex->level_vertex_id(), m_path);
					if(m_path.size() > 3)
					{
						m_path.pop_back();
						m_path.erase(m_path.begin());

						link.m_path.push_back(link.pos[0]);
						for (u32 travelpoint : m_path)
						{
							Fvector lvtx_pos = lgraph.vertex_position(travelpoint);
							lvtx_pos.y += 1.f;
							bbox.modify(lvtx_pos);
							link.m_path.push_back(lvtx_pos);
						}
						link.m_path.push_back(link.pos[1]);
					}
					else
					{
						link.m_path.push_back(link.pos[0]);
						link.m_path.push_back(link.pos[2]);
						link.m_path.push_back(link.pos[1]);
					}
					bbox.modify(child_pos);
					bbox.get_CD(link.spatial_c, link.spatial_bd);
					//bbox.getcenter(graph_child.first->second.spatial_c);
					link.spatial_r += bbox.getradius();
				}
			}
			PROF_STOP_THREAD();
		});
	}
	float RENDER_DISTANCE = visible_currents ? 50000.f : g_pGamePersistent->pEnvironment->CurrentEnv->fog_distance;
	float RENDER_DISTANCE_SQR = RENDER_DISTANCE * RENDER_DISTANCE;

	constexpr u32 default_lcolor = color_rgba(255, 0, 255, 255);//розовый
	constexpr u32 graph_blue_lcolor = color_rgba(0, 255, 255, 255);//голубой

	constexpr u32 default_tcolor = color_rgba(255, 0, 255, 60);//розовый
	constexpr u32 graph_blue_tcolor = color_rgba(0, 255, 255, 60);//голубой
	constexpr u32 selection_tcolor = color_rgba(255, 0, 0, 150);//розовый
	Fvector& cam_pos = Device.vCameraPosition;
	Fvector& cam_dir = Device.vCameraDirection;
	CFrustum& view_base = ::Render->ViewBase;
	auto& graphs = m_graphs;
	constexpr u32 color_green = color_rgba(0, 255, 100, 255);
	
	for (auto& pair : graphs)
	{
		GraphPointC& graph_main = pair.second;
		Fvector& spatial_c = graph_main.obb.m_translate;
		float spatial_r = graph_main.spatial_r;

		if (cam_pos.distance_to_sqr(spatial_c) > _sqr(RENDER_DISTANCE + spatial_r))
			continue;
		
		if (!visible_currents && !view_base.testSphere_dirty(spatial_c, spatial_r))
			continue;

		//Fobb obb; obb.m_rotate.identity();
		//obb.m_translate = spatial_c;
		//obb.m_halfsize = graph_main.spatial_bd;
		//append_obb(obb, color_rgba(0, 255, 0, 150));
		u32 tcolor = graph_main.is_blue ? graph_blue_tcolor : default_tcolor;
		Fvector& pos = graph_main.pos;
		if(m_flags.test(ESCENE_FLAGS::ESF_DRAW_SELECTION) && m_selection_flags.test(ESELECTION_FLAGS::ESLF_GP))
		{
			float range = RD.range;
			if (visible_currents || (!RQ.O && graph_main.obb.intersect(cam_pos, cam_dir, range)))
			{
				Fvector post = pos;
				post.y += graph_main.obb.m_halfsize.y + 0.1f;
				bool text3d = append_text3d(post, shared_str().printf("%d", graph_main.gvid), color_green, CGameFont::alLeft);
				if(graph_main.next_level)
				{
					if (text3d)
						append_text_next(shared_str().printf("next_level: %s", *graph_main.next_level));
					else
						text3d = append_text3d(pos, shared_str().printf("next_level: %s", *graph_main.next_level), color_green, CGameFont::alLeft);
				}
				tcolor = selection_tcolor;
				if(!visible_currents)
				{
					RQ.O = (CGameObject*)0xFF;
					RD.range = range;
				}
			}
		}

		append_graph_point(pos, graph_main.is_blue ? graph_blue_lcolor : default_lcolor, tcolor);
	}

	for (auto& link : m_links)
	{
		Fvector& spatial_c = link.second.spatial_c;
		float spatial_r = link.second.spatial_r;

		if (cam_pos.distance_to_sqr(spatial_c) > _sqr(RENDER_DISTANCE + spatial_r))
			continue;

		if (!visible_currents && !view_base.testSphere_dirty(spatial_c, spatial_r))
			continue;

		//Fobb obb; obb.m_rotate.identity();
		//obb.m_translate = spatial_c;
		//obb.m_halfsize = link.second.spatial_bd;
		//append_obb(obb, color_rgba(0, 255, 0, 150));

		//append_line({ link.second.pos[0], link.second.pos[2], link.second.is_blue[0] ? graph_blue_lcolor : default_lcolor });
		//append_line({ link.second.pos[1], link.second.pos[2], link.second.is_blue[1] ? graph_blue_lcolor : default_lcolor });

		auto& child_vertices = link.second.m_path;

		size_t total = child_vertices.size();
		if (total < 2) return;

		size_t mid_point = total / 2; // индекс, после которого меняем цвет
		bool is_first_path = true;
		u32 color = link.second.is_blue[0] ? graph_blue_lcolor : default_lcolor;

		for (size_t i = 0; i + 1 < total; i++)
		{
			// Если следующая точка будет уже во второй половине
			if (is_first_path && (i + 1) >= mid_point)
			{
				is_first_path = false;
				color = link.second.is_blue[1] ? graph_blue_lcolor : default_lcolor;
			}

			append_line({ child_vertices[i], child_vertices[i + 1], color });
		}
	}
}

void LevelInspector::DrawLevelGraph()
{
	PROF_EVENT(__FUNCTION__);
	static xr_task_group calc_nodes;
	ILevelGraph& lgraph = ai().level_graph();
	u8 curr_level_id = lgraph.level_id();
	static u8 last_level_id = curr_level_id;
	static const float node_D = lgraph.header().cell_size();
	static const float node_R_F = node_D * 0.78f;
	static const float node_R = node_D * 0.5f;

	struct CachedNode
	{
		Fvector v1;
		Fvector v2;
		Fvector v3;
		Fvector v4;
		Fvector2 c;
		u32 base_color = 0u;
		u32 color = 0u;
	};

	struct QuadTreeNode
	{
		Fsphere bsphere;
		Fbox bbox;
		Fbox2 bbox2D;
		//u32 color = 0u;
		u32 start_index = 0;
		u32 end_index = 0;
		QuadTreeNode* children[4] = { nullptr, nullptr, nullptr, nullptr };
		bool is_leaf = false;

		~QuadTreeNode()
		{
			for (int i = 0; i < 4; ++i)
				if (children[i]) delete children[i];
		}
	};
	static xr_vector<CachedNode> cached_nodes;
	static xr_vector<CachedNode*> vertex_buffer[2];
	static xr_vector<u32> node_indices;
	static xr_vector<u32> temp_indices;
	static Fbox2 level_bbox2D;

	static QuadTreeNode* quadtree_root = nullptr;
	static bool quadtree_built = false;

	static int devider = 32000;
	static u32 nodes_size = lgraph.header().vertex_count();
	//int console_value = CCC_Integer::FastCommand("granularity", devider, 1, nodes_size);
	//if (devider != console_value)
	//{
	//	delete quadtree_root;
	//	quadtree_root = nullptr;
	//	quadtree_built = false;
	//	devider = console_value;
	//
	//	if (CCC_Boolean::FastCommand("granularity_full", false))
	//	{
	//		cached_nodes.clear();
	//		node_indices.clear();
	//		temp_indices.clear();
	//		level_bbox.invalidate();
	//		level_bbox2D.invalidate();
	//		nodes_size = lgraph.header().vertex_count();
	//	}
	//}

	if (last_level_id != curr_level_id)
	{
		calc_nodes.wait();
		vertex_buffer[0].clear();
		vertex_buffer[1].clear();
		delete quadtree_root;
		quadtree_root = nullptr;
		quadtree_built = false;
		cached_nodes.clear();
		node_indices.clear();
		temp_indices.clear();
		level_bbox2D.invalidate();
		nodes_size = lgraph.header().vertex_count();
		last_level_id = curr_level_id;
	}

	static int leafs_granularity = nodes_size / devider;
	leafs_granularity = nodes_size / devider;
	if (leafs_granularity < 1) leafs_granularity = 1;

	if (cached_nodes.empty())
	{
		PROF_EVENT("build_cached_nodes");
		ILevelGraph::CVertex* verts = lgraph.vertices();
		cached_nodes.reserve(nodes_size);
		for (u32 i = 0; i < nodes_size; ++i)
		{
			ILevelGraph::CVertex& vertex = verts[i];

			Fvector node_pos = lgraph.vertex_position(&vertex);

			node_pos.y += 0.045f;
			CachedNode& node = cached_nodes.emplace_back();

			Fplane plane;
			pvDecompress(plane.n, vertex.plane());
			plane.d = -plane.n.dotproduct(node_pos);

			node.v1.set(node_pos.x - node_R, node_pos.y, node_pos.z - node_R);
			node.v2.set(node_pos.x + node_R, node_pos.y, node_pos.z - node_R);
			node.v3.set(node_pos.x + node_R, node_pos.y, node_pos.z + node_R);
			node.v4.set(node_pos.x - node_R, node_pos.y, node_pos.z + node_R);
			node.c.set(node_pos.x, node_pos.z);

			node.v1.y -= plane.classify(node.v1) / plane.n.y;
			node.v2.y -= plane.classify(node.v2) / plane.n.y;
			node.v3.y -= plane.classify(node.v3) / plane.n.y;
			node.v4.y -= plane.classify(node.v4) / plane.n.y;

			if (!lgraph.is_accessible(i))
				node.base_color = color_rgba(128, 128, 128, 180);
			else
			{
				u32 neighbors = 0;
				for (u32 j = 0; j < 4; ++j)
				{
					u32 neighbor_id = vertex.link(j);
					if (lgraph.valid_vertex_id(neighbor_id))
						neighbors++;
				}

				switch (neighbors)
				{
				case 0: node.base_color = color_rgba(0, 0, 255, 255); break;
				case 1: node.base_color = color_rgba(0, 100, 255, 210); break;
				case 2: node.base_color = color_rgba(0, 180, 255, 180); break;
				case 3: node.base_color = color_rgba(50, 220, 255, 150); break;
				case 4: node.base_color = color_rgba(200, 220, 50, 100); break;
				}
			}

			node.color = node.base_color;
		}
		const Fbox& level_bbox = Level().ObjectSpace.GetBoundingVolume();
		level_bbox2D.min.set(level_bbox.min.x, level_bbox.min.z);
		level_bbox2D.max.set(level_bbox.max.x, level_bbox.max.z);

		node_indices.resize(nodes_size);
		for (u32 i = 0; i < nodes_size; ++i)
			node_indices[i] = i;

		temp_indices.resize(nodes_size);
	}

	if (!quadtree_built)
	{
		PROF_EVENT("build_quad_tree");
		auto build_quadtree = [](auto& self, u32 start, u32 end, const Fbox2& node_bbox, int depth) -> QuadTreeNode*
		{
			u32 node_count = end - start;
			if (node_count <= leafs_granularity || depth >= 12)
			{
				QuadTreeNode* leaf = new QuadTreeNode();
				leaf->is_leaf = true;
				leaf->start_index = start;
				leaf->end_index = end;
				leaf->bbox2D = node_bbox;
				//leaf->color = color_rgba(Random.randI(255), Random.randI(255), Random.randI(255), 10);

				leaf->bbox.invalidate();
				for (u32 i = start; i < end; ++i)
				{
					u32 idx = node_indices[i];
					leaf->bbox.modify(cached_nodes[idx].v1);
					//leaf->bbox.modify(cached_nodes[idx].v2);
					leaf->bbox.modify(cached_nodes[idx].v3);
					//leaf->bbox.modify(cached_nodes[idx].v4);
				}
				leaf->bbox.getsphere(leaf->bsphere.P, leaf->bsphere.R);
				return leaf;
			}

			Fvector2 center
			{
				(node_bbox.min.x + node_bbox.max.x) * 0.5f,
				(node_bbox.min.y + node_bbox.max.y) * 0.5f
			};

			Fbox2 quad_bbox[4];

			quad_bbox[0].min.set(node_bbox.min.x, center.y);
			quad_bbox[0].max.set(center.x, node_bbox.max.y);

			quad_bbox[1].min.set(center.x, center.y);
			quad_bbox[1].max.set(node_bbox.max.x, node_bbox.max.y);

			quad_bbox[2].min.set(node_bbox.min.x, node_bbox.min.y);
			quad_bbox[2].max.set(center.x, center.y);

			quad_bbox[3].min.set(center.x, node_bbox.min.y);
			quad_bbox[3].max.set(node_bbox.max.x, center.y);

			u32 quad_counts[4] = { 0U, 0U, 0U, 0U };

			for (u32 i = start; i < end; ++i)
			{
				const Fvector2& pos2D = cached_nodes[node_indices[i]].c;

				u8 quad = 0u;
				if (pos2D.x < center.x)
				{
					if (pos2D.y < center.y) quad = 2u;
					else quad = 0u;
				}
				else
				{
					if (pos2D.y < center.y) quad = 3u;
					else quad = 1u;
				}

				quad_counts[quad]++;
			}

			u32 quad_starts[4];
			quad_starts[0] = start;
			for (int i = 1; i < 4; ++i)
				quad_starts[i] = quad_starts[i - 1] + quad_counts[i - 1];

			u32 quad_positions[4];
			memcpy(quad_positions, quad_starts, sizeof(quad_positions));

			for (u32 i = start; i < end; ++i)
			{
				u32 idx = node_indices[i];
				const Fvector2& pos2D = cached_nodes[idx].c;

				u8 quad = 0u;
				if (pos2D.x < center.x)
				{
					if (pos2D.y < center.y) quad = 2u;
					else quad = 0u;
				}
				else
				{
					if (pos2D.y < center.y) quad = 3u;
					else quad = 1u;
				}

				temp_indices[quad_positions[quad]++] = idx;
			}

			memcpy(&node_indices[start], &temp_indices[start], (end - start) * sizeof(u32));

			QuadTreeNode* node = new QuadTreeNode();
			node->is_leaf = false;
			node->bbox2D = node_bbox;
			//node->color = color_rgba(Random.randI(255), Random.randI(255), Random.randI(255), 10);

			u32 child_start = start;
			for (int i = 0; i < 4; ++i)
			{
				u32 _qc = quad_counts[i];
				if (_qc > 0)
				{
					u32 child_end = child_start + _qc;
					node->children[i] = self(self, child_start, child_end, quad_bbox[i], depth + 1);
					child_start = child_end;
				}
			}

			node->bbox.invalidate();
			for (QuadTreeNode* lnode : node->children)
			{
				if (lnode)
					node->bbox.merge(lnode->bbox);
			}
			node->bbox.getsphere(node->bsphere.P, node->bsphere.R);
			return node;
		};

		quadtree_root = build_quadtree(build_quadtree, 0, (u32)node_indices.size(), level_bbox2D, 0);
		quadtree_built = true;
	}

	if (!quadtree_root)
		return;

	CachedNode* selected_nodes[6] = { nullptr, nullptr, nullptr, nullptr, nullptr, nullptr };
	if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_SELECTION) && m_selection_flags.test(ESELECTION_FLAGS::ESLF_LG) && RQR.r_count() && !RQ.O && g_actor)
	{
		Fvector search_pos;
		search_pos.mad(RD.start, RD.dir, RD.range+0.1f);
		u32 tv_id = lgraph.vertex(g_actor->ai_location().level_vertex_id(), search_pos);
		if (lgraph.valid_vertex_id(tv_id))
		{
			Fvector node_pos = lgraph.vertex_position(tv_id);
			node_pos.y += 0.06f;
			if (node_pos.distance_to_sqr(search_pos) <= 1.f)
			{
				selected_nodes[5] = &cached_nodes[tv_id];

				Fcolor t_clr;
				t_clr.set(selected_nodes[5]->base_color);
				t_clr.mul_rgba(1.7f);
				selected_nodes[5]->color = t_clr.get();

				append_text3d(node_pos, shared_str().printf("%d", tv_id), color_rgba(255, 0, 0, 255), CGameFont::alLeft);

				ILevelGraph::CVertex* vertex = lgraph.vertex(tv_id);
				for (u32 j = 0; j < 4; ++j)
				{
					u32 neighbor_id = vertex->link(j);
					if (lgraph.valid_vertex_id(neighbor_id))
					{
						selected_nodes[j] = &cached_nodes[neighbor_id];
						Fcolor t_clr;
						t_clr.set(selected_nodes[j]->base_color);
						t_clr.mul_rgba(1.7f);
						selected_nodes[j]->color = t_clr.get();
					}
				}
			}
		}
	}
	constexpr u32 selection_color = color_rgba(128, 128, 128, 255);
	calc_nodes.wait();
	static u8 update_idx = 0;
	static u8 render_idx = 1;
	std::swap(update_idx, render_idx);
	static Fvector cam_pos; cam_pos = Device.vCameraPosition;
	static CFrustum view_base; view_base.CreateFromMatrix(Device.mFullTransform, FRUSTUM_P_LRTB + FRUSTUM_P_FAR);
	calc_nodes.run([]()
	{
		PROF_EVENT("LevelInspector::calc_nodes");
		float RENDER_DISTANCE = g_pGamePersistent&&g_pGamePersistent->Environment().CurrentEnv ? g_pGamePersistent->Environment().CurrentEnv->fog_distance : 10.f;
		float RENDER_DISTANCE_SQR = RENDER_DISTANCE * RENDER_DISTANCE;

		vertex_buffer[update_idx].clear();
		static xr_vector<QuadTreeNode*> stack;
		stack.clear();
		stack.reserve(128);
		stack.push_back(quadtree_root);
		while (!stack.empty())
		{
			QuadTreeNode* node = stack.back();
			stack.pop_back();

			float dist_sqr = cam_pos.distance_to_sqr(node->bsphere.P);
			float max_dist_sqr = _sqr(RENDER_DISTANCE + node->bsphere.R);
			if (dist_sqr > max_dist_sqr)
				continue;

			u32 mask = 0xff;
			u32 res = view_base.testSAABB(node->bsphere.P, node->bsphere.R, node->bbox.data(), mask);
			if (fcvNone == res)
				continue;

			if (node->is_leaf)
			{
				for (u32 i = node->start_index; i < node->end_index; ++i)
					vertex_buffer[update_idx].push_back(&cached_nodes[node_indices[i]]);
				continue;
			}

			for (QuadTreeNode* lnode : node->children)
			{
				if (lnode)
					stack.push_back(lnode);
			}
		}
	});

	if (!vertex_buffer[render_idx].empty())
	{
		if (tris.empty() && lines.empty())
		{
			UIRender->SetShader(*shader);
			UIRender->CacheSetCullMode(ERHI_CULLMODE::BACK);
			UIRender->zb_enable(zbuffer_enable);
		}

		constexpr u32 TRIS_PER_QUAD = 2u;
		constexpr u32 MAX_TRIS_PER_BATCH = 58'000u;
		constexpr u32 MAX_QUADS_PER_BATCH = MAX_TRIS_PER_BATCH / TRIS_PER_QUAD;
		constexpr u32 VERTS_PER_BATCH = MAX_TRIS_PER_BATCH * 3u;

		xr_vector<CachedNode*>& nodes = vertex_buffer[render_idx];
		u32 total_quads = (u32)nodes.size();
		u32 total_tris = total_quads * TRIS_PER_QUAD;

		u32 num_batches = (total_tris + MAX_TRIS_PER_BATCH - 1) / MAX_TRIS_PER_BATCH;

		for (u32 batch_idx = 0u; batch_idx < num_batches; ++batch_idx)
		{
			u32 start_quad = batch_idx * MAX_QUADS_PER_BATCH;
			u32 quads_in_batch = std::min(MAX_QUADS_PER_BATCH, total_quads - start_quad);
			u32 tris_in_batch = quads_in_batch * TRIS_PER_QUAD;

			IUIRender::LITFast** buffer = UIRender->StartPrimitiveLITFast(tris_in_batch * 3u, IUIRender::ptTriList);

			u32 end_quad = start_quad + quads_in_batch;
			for (u32 q = start_quad; q < end_quad; ++q)
			{
				CachedNode* node = nodes[q];
				u32 color = node->color;

				(*buffer)->p = node->v3;
				(*buffer)->color = color;
				(*buffer)++;

				(*buffer)->p = node->v2;
				(*buffer)->color = color;
				(*buffer)++;

				(*buffer)->p = node->v1;
				(*buffer)->color = color;
				(*buffer)++;

				(*buffer)->p = node->v4;
				(*buffer)->color = color;
				(*buffer)++;

				(*buffer)->p = node->v3;
				(*buffer)->color = color;
				(*buffer)++;

				(*buffer)->p = node->v1;
				(*buffer)->color = color;
				(*buffer)++;
				node->color = node->base_color;
			}

			UIRender->FlushPrimitive();
		}
	}
}

void LevelInspector::DrawSkeleton(IKinematics* pKinematics, Fmatrix& xform, CGameObject* GO)
{
	if (m_skeleton_flags.test(ESKELETON_INFO::ESI_NONE))
		return;
	u32 lines_color = color_rgba(0, 255, 0, 255);
	u32 triangles_color = color_rgba(0, 255, 0, 10);
	
	if (GO)
	{
		if(CPhysicsShellHolder* SH = GO->cast_physics_shell_holder())
		{
			if ((SH->m_pPhysicsShell && SH->m_pPhysicsShell->isActive() && SH->m_pPhysicsShell->isEnabled()) ||
				(SH->character_physics_support() && SH->character_physics_support()->movement()->IsCharacterEnabled()))
			{
				lines_color = color_rgba(255, 0, 0, 255);
				triangles_color = color_rgba(255, 0, 0, 20);
			}
		}
	}
	Fvector box_c, box_hs;
	pKinematics->dcast_RenderVisual()->getVisData().box.get_CD(box_c, box_hs);
	float dist_to_cam_sqr = Device.vCameraPosition.distance_to_sqr(xform.c);
	if (dist_to_cam_sqr <= 2500.f)
	{
		Fmatrix obb_xform, bone_xform;
		for (std::pair<shared_str, u16>& bone : *pKinematics->LL_Bones())
		{
			bone_xform = pKinematics->LL_GetTransform(bone.second);
			CBoneData& data = pKinematics->LL_GetData(bone.second);
			SBoneShape shape = data.shape;
			if (m_skeleton_flags.test(ESKELETON_INFO::ESI_HIT_SHAPES) && pKinematics->LL_GetBoneVisible(bone.second))
			{
				bone_xform.mulA_43(xform);
				switch (shape.type)
				{
				case SBoneShape::stBox:
				{
					Fobb obb = shape.box;
					bone_xform.transform_dir(obb.m_rotate.k);
					bone_xform.transform_dir(obb.m_rotate.i);
					bone_xform.transform_dir(obb.m_rotate.j);
					bone_xform.transform_tiny(obb.m_translate);
					append_obb(obb, lines_color, triangles_color);
					if (m_selection_flags.test(ESELECTION_FLAGS::ESLF_O) && GO && (shape.flags.test(SBoneShape::sfNoPickable) || !GO->getEnabled()) && GO->dcast_CObject() != RQ.O && obb.intersect(RD.start, RD.dir, RD.range))
					{
						Fvector C = obb.m_translate;
						C.y += obb.m_rotate.j.magnitude() * 0.5f;
						DrawObjectInfo(GO, C, { 0.f, 0.f });
					}
					break;
				}
				case SBoneShape::stSphere:
				{
					Fsphere& sphere = shape.sphere;
					Fmatrix B;
					B.scale(sphere.R, sphere.R, sphere.R);
					B.translate_over(sphere.P);
					B.mulA_43(bone_xform);
					append_ellipse(B, lines_color, triangles_color);
					B.transform_tiny(sphere.P);
					if (m_selection_flags.test(ESELECTION_FLAGS::ESLF_O) && GO && (shape.flags.test(SBoneShape::sfNoPickable) || !GO->getEnabled()) && GO->dcast_CObject() != RQ.O && sphere.intersect2(RD.start, RD.dir, RD.range))
					{
						sphere.P.y += sphere.R;
						DrawObjectInfo(GO, sphere.P, { 0.f, 0.f });
					}
					break;
				}
				case SBoneShape::stCylinder:
				{
					Fcylinder Cyl = shape.cylinder;
					bone_xform.transform_tiny(Cyl.m_center);
					bone_xform.transform_dir(Cyl.m_direction);
					append_cylinder(Cyl, lines_color, triangles_color);
					if (m_selection_flags.test(ESELECTION_FLAGS::ESLF_O) && GO && (shape.flags.test(SBoneShape::sfNoPickable) || !GO->getEnabled()) && GO->dcast_CObject() != RQ.O && Cyl.intersect(RD.start, RD.dir, RD.range))
					{
						Fvector C = Cyl.m_center;
						C.y += Cyl.m_radius*2.f;
						DrawObjectInfo(GO, C, { 0.f, 0.f });
					}
					break;
				}
				case SBoneShape::stNone:
					break;
				default: NODEFAULT;
				}
			}
			if (m_skeleton_flags.test(ESKELETON_INFO::ESI_BBOXES) && pKinematics->LL_GetBoneVisible(bone.second))
			{
				bone_xform = pKinematics->LL_GetTransform(bone.second);
				Fobb obb = pKinematics->LL_GetData(bone.second).obb;
				obb.xform_get(obb_xform);
				obb_xform.mulA_43(bone_xform);
				obb_xform.mulA_43(xform);
				obb.xform_set(obb_xform);
				append_obb(obb, lines_color, triangles_color);
				if (m_selection_flags.test(ESELECTION_FLAGS::ESLF_O) && GO && (shape.flags.test(SBoneShape::sfNoPickable) || !GO->getEnabled()) && GO->dcast_CObject() != RQ.O && obb.intersect(RD.start, RD.dir, RD.range))
					DrawObjectInfo(GO, obb.m_translate, { 0.f, 0.f });
			}

			if (dist_to_cam_sqr < _sqr(box_hs.magnitude() * 5.f))
			{
				if (m_skeleton_flags.test(ESKELETON_INFO::ESI_BONES))
				{
					bone_xform = pKinematics->LL_GetTransform(bone.second);
					bone_xform.k.mul(0.008f);
					bone_xform.i.mul(0.008f);
					bone_xform.j.mul(0.008f);
					bone_xform.mulA_43(xform);
					append_axis(bone_xform, 1.0f);
					append_ellipse(bone_xform, color_rgba(0, 0, 0, 255), color_rgba(255, 100, 10, 50));
				}
				if (m_skeleton_flags.test(ESKELETON_INFO::ESI_BONES_INFO))
				{
					Fvector text_pos = pKinematics->LL_GetTransform(bone.second).c;
					xform.transform_tiny(text_pos);
					text_pos.y += 0.008f;
					append_text3d(text_pos, shared_str().printf("%s [%d]", *bone.first, bone.second));
				}
				if (m_skeleton_flags.test(ESKELETON_INFO::ESI_BONES_LINKS))
				{
					bone_xform.mulA_43(xform);
					CBoneData& bone_data = pKinematics->LL_GetData(bone.second);
					IBoneData& ibone_data = bone_data;
					u16	num_children = ibone_data.GetNumChildren();
					for (int i = 0; i < num_children; ++i)
					{
						Fvector self_pos, child_pos;
						pKinematics->LL_GetTransform(bone.second).transform_tiny(self_pos);
						xform.transform_tiny(self_pos);
						pKinematics->LL_GetTransform(ibone_data.GetChild(i).GetSelfID()).transform_tiny(child_pos);
						xform.transform_tiny(child_pos);
						append_line({ self_pos, child_pos, color_rgba(255, 200, 100, 255) });
					}
				}
			}
		}
	}

	if(m_skeleton_flags.test(ESKELETON_INFO::ESI_MAIN_BBOX))
	{
		Fobb obb;
		obb.m_rotate.identity();
		obb.m_halfsize = box_hs;
		obb.m_translate = box_c;
		Fmatrix obb_xform;
		obb.xform_get(obb_xform);
		obb_xform.mulA_43(xform);
		obb.xform_set(obb_xform);
		append_obb(obb, lines_color, triangles_color);
		if (m_selection_flags.test(ESELECTION_FLAGS::ESLF_O) && GO && GO->dcast_CObject() != RQ.O && obb.intersect(RD.start, RD.dir, RD.range))
		{
			Fvector C = obb.m_translate;
			C.y += obb.m_rotate.j.magnitude() * 0.5f;
			DrawObjectInfo(GO, C, { 0.f, 0.f });
		}
	}
}

void LevelInspector::DrawHud()
{
	PROF_EVENT(__FUNCTION__);
	if (g_player_hud)
	{
		auto draw_fdeps = [](attachable_hud_item* hi, LevelInspector& dprim)
		{
			firedeps fd;
			hi->setup_firedeps(fd);

			Fobb obb; obb.identity();
			obb.m_rotate.i.set(hi->m_item_transform.i);
			obb.m_rotate.j.set(hi->m_item_transform.j);
			obb.m_rotate.k.set(hi->m_item_transform.k);
			obb.m_rotate.i.mul(0.02f);
			obb.m_rotate.j.mul(0.02f);
			obb.m_rotate.k.mul(0.02f);

			if (hi->m_measures.m_prop_flags.test(hud_item_measures::e_fire_point))
			{
				obb.m_translate.set(fd.vLastFP);
				dprim.append_obb(obb, color_rgba(255, 0, 0, 255), color_rgba(255, 0, 0, 30));
				dprim.append_text3d(obb.m_translate, "fire_point", color_rgba(255, 0, 0, 255), CGameFont::alLeft);
			}
			if (hi->m_measures.m_prop_flags.test(hud_item_measures::e_fire_point2))
			{
				obb.m_translate.set(fd.vLastFP2);
				dprim.append_obb(obb, color_rgba(0, 0, 255, 255), color_rgba(0, 0, 255, 30));
				dprim.append_text3d(obb.m_translate, "fire_point2", color_rgba(0, 0, 255, 255), CGameFont::alLeft);
			}
			if (hi->m_measures.m_prop_flags.test(hud_item_measures::e_shell_point))
			{
				obb.m_translate.set(fd.vLastSP);
				dprim.append_obb(obb, color_rgba(255, 155, 0, 255), color_rgba(255, 155, 0, 30));
				dprim.append_text3d(obb.m_translate, "shell_point", color_rgba(255, 155, 0, 255), CGameFont::alLeft);
			}
		};

		bool b_r0 = (g_player_hud->attached_item(0) && g_player_hud->attached_item(0)->need_renderable());
		bool b_r1 = (g_player_hud->attached_item(1) && g_player_hud->attached_item(1)->need_renderable());

		if (b_r0)
		{
			DrawSkeleton(g_player_hud->attached_item(0)->m_model, g_player_hud->attached_item(0)->m_item_transform);
			if(m_skeleton_flags.test(ESKELETON_INFO::ESI_FIRE_POINTS))
				draw_fdeps(g_player_hud->attached_item(0), *this);
		}
		if (b_r1)
		{
			DrawSkeleton(g_player_hud->attached_item(1)->m_model, g_player_hud->attached_item(1)->m_item_transform);
			if (m_skeleton_flags.test(ESKELETON_INFO::ESI_FIRE_POINTS))
				draw_fdeps(g_player_hud->attached_item(1), *this);
		}
		if (g_player_hud->GetAnimator() && g_player_hud->GetAnimator()->IsPlaying || g_player_hud->GetHandsVisible() || b_r0 || b_r1)
			DrawSkeleton(g_player_hud->GetModel()->dcast_PKinematics(), g_player_hud->GetTransform());

		if (g_player_hud->GetAnimator() && g_player_hud->GetAnimator()->IsPlaying)
			DrawSkeleton(g_player_hud->GetAnimator()->m_item, g_player_hud->GetAnimator()->m_item_transform);
	}
}

void LevelInspector::DrawObjectInfo(CGameObject* GO, const Fvector& pos, Fvector2 xy)
{
	if (m_selection_text_flags.test(EOBJECT_INFO::EOI_NONE))
		return;

	IGameGraph& graph = ai().game_graph();
	constexpr u32 color_green = color_rgba(0, 255, 100, 255);
	constexpr u32 color_red = color_rgba(255, 100, 0, 255);
	//Fvector C;
	//GO->Center(C);
	//C.y += GO->Radius();
	bool text3d = false;
	
	if (m_selection_text_flags.test(EOBJECT_INFO::EOI_SNAME))
		text3d = append_text3d(pos, GO->cNameSect(), color_green, CGameFont::alLeft);

	if (m_selection_text_flags.test(EOBJECT_INFO::EOI_LNAME))
	{
		if (text3d)
			append_text_next(GO->Name());
        else
			text3d = append_text3d(pos, GO->Name(), color_green, CGameFont::alLeft);
	}

	if (m_selection_text_flags.test(EOBJECT_INFO::EOI_LCNAME))
	{
		string16 temp; CLSID2TEXT(GO->CLS_ID, temp);
        if (text3d)
			append_text_next(shared_str().printf("%s[%d]", temp, GO->CLS_ID));
        else
			text3d = append_text3d(pos, shared_str().printf("%s[%d]", temp, GO->CLS_ID), color_green, CGameFont::alLeft);

		if (text3d)
			append_text_next(typeid(*GO).name());
		else
			text3d = append_text3d(pos, typeid(*GO).name(), color_green, CGameFont::alLeft);
	}
	if (m_selection_text_flags.test(EOBJECT_INFO::EOI_VNAME) && GO->cNameVisual())
	{
		if (text3d)
			append_text_next(GO->cNameVisual());
		else
            text3d = append_text3d(pos, GO->cNameVisual(), color_green, CGameFont::alLeft);
	}
	if (m_selection_text_flags.test(EOBJECT_INFO::EOI_POSITION))
	{
		if (text3d)
			append_text_next(shared_str().printf("pos[%3.3f, %3.3f, %3.3f]", VPUSH(GO->Position())));
		else
            text3d = append_text3d(pos, shared_str().printf("pos[%3.3f, %3.3f, %3.3f]", VPUSH(GO->Position())), color_green, CGameFont::alLeft);
	}
	if (m_selection_text_flags.test(EOBJECT_INFO::EOI_GVERTEX_LVERTEX))
	{
		if (text3d)
			append_text_next(shared_str().printf("gvid[%d] lvid[%d]", GO->ai_location().game_vertex_id(), GO->ai_location().level_vertex_id()));
		else
			append_text3d(pos, shared_str().printf("gvid[%d] lvid[%d]", GO->ai_location().game_vertex_id(), GO->ai_location().level_vertex_id()), color_green, CGameFont::alLeft);
	}

	if (m_selection_text_flags.test(EOBJECT_INFO::EOI_INI))
	{
		//color_red
		dbg_font->SetColor(color_red);

		if (CInifile* ini = GO->spawn_ini())
		{
			for (CInifile::Sect* sect : ini->sections())
			{
				if (text3d)
					append_text_next(shared_str().printf("[%s]", *sect->Name));
				else
					text3d = append_text3d(pos, shared_str().printf("[%s]", *sect->Name), color_red, CGameFont::alLeft);

				for (CInifile::Item& line : sect->Data)
				{
					if (line.second)
					{
						if (text3d)
							append_text_next(shared_str().printf("%s = %s", *line.first, *line.second));
						else
							text3d = append_text3d(pos, shared_str().printf("%s = %s", *line.first, *line.second), color_red, CGameFont::alLeft);
					}
					else
					{
						if (text3d)
							append_text_next(line.first);
						else
							text3d = append_text3d(pos, line.first, color_red, CGameFont::alLeft);
					}
				}
			}
		}

		if (CLevelChanger* chngr = GO->cast_level_changer())
		{
			u16 vertex = chngr->NextVertexId();
			if (graph.valid_vertex_id(vertex))
			{
				auto it = graph.header().levels().find(graph.vertex(vertex)->level_id());
				if (it != graph.header().levels().end())
				{
					if (text3d)
						append_text_next(shared_str().printf("next_level: %s", it->second.name().c_str()));
					else
						text3d = append_text3d(pos, shared_str().printf("next_level: %s", it->second.name().c_str()), color_red, CGameFont::alLeft);
				}
			}
		}

		if (CSpaceRestrictor* restrictor = GO->cast_restrictor())
		{
			RestrictionSpace::ERestrictorTypes restr_type = RestrictionSpace::ERestrictorTypes(restrictor->m_space_restrictor_type);
			if(restr_type!=RestrictionSpace::ERestrictorTypes::eRestrictorTypeNone)
			{
				if (text3d)
					append_text_next(magic_enum::enum_name(restr_type).data());
				else
					text3d = append_text3d(pos, magic_enum::enum_name(restr_type).data(), color_red, CGameFont::alLeft);
			}
		}

		if (smart_cover::object* cover_object = smart_cast<smart_cover::object*>(GO))
		{
			if (cover_object->cover().is_combat_cover())
			{
				if (text3d)
					append_text_next(shared_str("is_combat_cover"));
				else
					text3d = append_text3d(pos, shared_str("is_combat_cover"), color_red, CGameFont::alLeft);
			}

			if (cover_object->cover().can_fire())
			{
				if (text3d)
					append_text_next(shared_str("can_fire"));
				else
					text3d = append_text3d(pos, shared_str("can_fire"), color_red, CGameFont::alLeft);
			}
			if (text3d)
				append_text_next(cover_object->cover().description()->table_id());
			else
				text3d = append_text3d(pos, cover_object->cover().description()->table_id(), color_red, CGameFont::alLeft);

			for (smart_cover::loophole* lophole : cover_object->cover().loopholes())
			{
				if (text3d)
					append_text_next(lophole->id());
				else
					text3d = append_text3d(pos, lophole->id(), color_red, CGameFont::alLeft);
			}
		}
	}
}

void LevelInspector::DrawObjectsInfo()
{
	Fvector& cam_pos = Device.vCameraPosition;
	Fvector& cam_dir = Device.vCameraDirection;
	RQ.set(nullptr, g_pGamePersistent->pEnvironment->CurrentEnv->fog_distance, -1);
	collide::rq_target test_static = psDeviceFlags.test(rsDrawStatic) ? collide::rqtStatic : collide::rqtNone;
	collide::rq_target test_dynamic = m_selection_flags.test(ESELECTION_FLAGS::ESLF_O)&&m_flags.test(ESCENE_FLAGS::ESF_DRAW_OBJECTS) &&psDeviceFlags.test(rsDrawDynamic) ? collide::rqtObject : collide::rqtNone;
	collide::rq_target test_zones = m_selection_flags.test(ESELECTION_FLAGS::ESLF_Z)&&m_flags.test(ESCENE_FLAGS::ESF_DRAW_ZONES) ? collide::rqtShape : collide::rqtNone;
	collide::rq_target test_flags = collide::rq_target(test_zones | test_dynamic | test_static);
	RD = collide::ray_defs(cam_pos, cam_dir, RQ.range, CDB::OPT_CULL | CDB::OPT_FULL_TEST, test_flags);
	static LevelInspector& LI = *this;
	Level().ObjectSpace.RayQuery(
	RQR, 
	RD,
	[](collide::rq_result & result, LPVOID params) -> BOOL
	{
		collide::rq_result* RQ = (collide::rq_result*)params;

		if (!result.O && result.element >=0)
		{
			CDB::TRI* T = Level().ObjectSpace.GetStaticTris() + result.element;
			SGameMtl* pMtl = GMLib.GetMaterialByIdx(T->material);
			if (pMtl != nullptr && (pMtl->Flags.is(SGameMtl::flPassable) || pMtl->Flags.is(SGameMtl::flActorObstacle)))
				return TRUE;
		}

		*RQ = result;
		return FALSE;
	},
	&RQ,
	[](const collide::ray_defs& rd, CObject* object, LPVOID params) -> BOOL
	{
		if (object && (object->SpatialComponent->spatial.type & ESPATIAL_TYPE::SHAPE) != ESPATIAL_TYPE::NONE)
		{
			if ((object->SpatialComponent->spatial.type & ESPATIAL_TYPE::SMART_COVER) != ESPATIAL_TYPE::NONE && !LI.m_zone_flags.test(EZONE_INFO::EZI_SMART_COVER))
				return FALSE;

			if ((object->SpatialComponent->spatial.type & ESPATIAL_TYPE::SPACE_RESTRICTOR) != ESPATIAL_TYPE::NONE && !LI.m_zone_flags.test(EZONE_INFO::EZI_RESTR))
				return FALSE;

			if ((object->SpatialComponent->spatial.type & ESPATIAL_TYPE::ANOMALY_ZONE) != ESPATIAL_TYPE::NONE && !LI.m_zone_flags.test(EZONE_INFO::EZI_ANOMALY_ZONE))
				return FALSE;

			if ((object->SpatialComponent->spatial.type & ESPATIAL_TYPE::LEVEL_CHANGER) != ESPATIAL_TYPE::NONE && !LI.m_zone_flags.test(EZONE_INFO::EZI_LEVEL_CHANGER))
				return FALSE;

			if ((object->SpatialComponent->spatial.type & ESPATIAL_TYPE::CAMP_ZONE) != ESPATIAL_TYPE::NONE && !LI.m_zone_flags.test(EZONE_INFO::EZI_CAMP_ZONE))
				return FALSE;

			if ((object->SpatialComponent->spatial.type & ESPATIAL_TYPE::ANOMAL_ZONE_LOGIC) != ESPATIAL_TYPE::NONE && !LI.m_zone_flags.test(EZONE_INFO::EZI_ANOMAL_ZONE_LOGIC))
				return FALSE;

			if ((object->SpatialComponent->spatial.type & ESPATIAL_TYPE::SMART_TERRAIN) != ESPATIAL_TYPE::NONE && !LI.m_zone_flags.test(EZONE_INFO::EZI_SMART_TERRAIN))
				return FALSE;
		}

		return TRUE;
	},
	m_selection_text_flags.test(EOBJECT_INFO::EOI_ACTOR) ? nullptr : Level().CurrentViewEntity());

	if (!visible_currents)
	{
		if (RQ.O)
		{
			if ((RQ.O->SpatialComponent->spatial.type & ESPATIAL_TYPE::SHAPE) != ESPATIAL_TYPE::NONE)
			{
				CCF_Shape* ccfshape = (CCF_Shape*)(RQ.O->CFORM());

				CCF_Shape::shape_def& shape = ccfshape->shapes[RQ.element];
				switch (shape.type)
				{
				case 0:
				{
					Fsphere sphere = shape.data.sphere;
					RQ.O->XFORM().transform_tiny(sphere.P);
					Fvector C = sphere.P;
					C.y += sphere.R;
					DrawObjectInfo(RQ.O->cast_game_object(), C, { 0.f, 0.f });
				}break;
				case 1:
				{
					Fmatrix matrix = shape.data.box;
					matrix.mulA_43(RQ.O->XFORM());
					Fobb obb; obb.xform_set(matrix);
					obb.m_halfsize.set(0.5f, 0.5f, 0.5f);
					Fvector C = obb.m_translate;
					C.y += obb.m_rotate.j.magnitude() * 0.5f;
					DrawObjectInfo(RQ.O->cast_game_object(), C, { 0.f, 0.f });
				}break;
				default: NODEFAULT;
				}
			}
			else
			{
				if (RQ.O->Visual())
				{
					Fvector box_c, box_hs;
					RQ.O->Visual()->getVisData().box.get_CD(box_c, box_hs);
					Fobb obb;
					obb.m_rotate.identity();
					obb.m_halfsize = box_hs;
					obb.m_translate = box_c;
					Fmatrix obb_xform;
					obb.xform_get(obb_xform);
					obb_xform.mulA_43(RQ.O->XFORM());
					obb.xform_set(obb_xform);
					float dist = RD.range + RQ.O->Radius();
					if (obb.intersect(RD.start, RD.dir, dist))
					{
						Fvector C;
						RQ.O->Center(C);
						C.y += box_hs.y;
						DrawObjectInfo(RQ.O->cast_game_object(), C, { 0.f, 0.f });
					}
				}
				else
				{
					Fvector C;
					RQ.O->Center(C);
					C.y += RQ.O->Radius();
					DrawObjectInfo(RQ.O->cast_game_object(), C, { 0.f, 0.f });
				}
			}
		}
	}
	RD.range = RQ.range;
}

void LevelInspector::DrawAIPaths()
{
	CFrustum& view_base = ::Render->ViewBase;
	Fvector& cam_pos = Device.vCameraPosition;
	g_SpatialSpace->q_sphere(m_objects, 0, ESPATIAL_TYPE::AI_ALIVE, cam_pos, ai().get_alife()->online_distance());
	for (ISpatialShared& spatial : m_objects)
	{
		if (!spatial.get()) continue;

		CObject* _O = spatial->dcast_CObject();
		CGameObject* GO = (_O && !_O->getDestroy()) ? _O->cast_game_object() : nullptr;
		if (!GO) continue;

		if ((spatial->spatial.type & ESPATIAL_TYPE::AI_ALIVE) != ESPATIAL_TYPE::NONE)
		{
			if (CCreature* monst = GO->cast_creature())
			{
				auto& path = monst->movement().detail().path();
				u32 curr_t_pioint = monst->movement().detail().curr_travel_point_index();
				if (path.empty() || curr_t_pioint >= path.size())
					continue;

				const Fvector& curr_pos = monst->movement().detail().curr_travel_point().position;
				const Fvector& dest_pos = monst->movement().detail().dest_position();
				Fvector dest_dir; dest_dir.sub(curr_pos, dest_pos);
				float rad = dest_dir.magnitude() * .5f;
				if (!view_base.testSphere_dirty(Fvector().mad(dest_pos, dest_dir.normalize_safe(), rad), rad + 0.4f))
					continue;

				Fcylinder cone;
				cone.m_direction = { 0,1,0 };
				cone.m_center = dest_pos;
				cone.m_radius = 0.4f;
				cone.m_height = .5f;
				append_cone(cone, 0, color_rgba(255, 0, 0, 100));

				u32 las_t_pioint = monst->movement().detail().last_patrol_point();
				for (u32 i = curr_t_pioint; i <= las_t_pioint; i++)
				{
					u32 next_idx = i + 1;
					if (next_idx < las_t_pioint)
					{
						Fvector pos1 = path[i].position;
						Fvector pos2 = path[next_idx].position;
						pos1.y += .5f; pos2.y += .5f;
						append_line({ pos1, pos2, color_rgba(255, 0, 0, 255) });
					}
				}
			}
		}
	}
}

void LevelInspector::DrawObjects()
{
	PROF_EVENT(__FUNCTION__);
	Fvector& cam_pos = Device.vCameraPosition;

	ESPATIAL_TYPE dyn_flags = m_flags.test(ESCENE_FLAGS::ESF_DRAW_OBJECTS) ? (ESPATIAL_TYPE::LADDER |
		ESPATIAL_TYPE::ACTOR |
		ESPATIAL_TYPE::ACTOR_DEAD |
		ESPATIAL_TYPE::ACTOR_ALIVE |
		ESPATIAL_TYPE::AI |
		ESPATIAL_TYPE::AI_DEAD |
		ESPATIAL_TYPE::AI_ALIVE |
		ESPATIAL_TYPE::STALKER |
		ESPATIAL_TYPE::STALKER_WOUNDED |
		ESPATIAL_TYPE::STALKER_DEAD |
		ESPATIAL_TYPE::STALKER_ALIVE |
		ESPATIAL_TYPE::MONSTER |
		ESPATIAL_TYPE::MONSTER_DEAD |
		ESPATIAL_TYPE::MONSTER_ALIVE |
		ESPATIAL_TYPE::CROW |
		ESPATIAL_TYPE::CROW_DEAD |
		ESPATIAL_TYPE::CROW_ALIVE |
		ESPATIAL_TYPE::ITEM |
		ESPATIAL_TYPE::WEAPON |
		ESPATIAL_TYPE::MISSILE |
		ESPATIAL_TYPE::ARTEFACT |
		ESPATIAL_TYPE::ANOMALY_DETECTOR |
		ESPATIAL_TYPE::CAR |
		ESPATIAL_TYPE::HELI |
		ESPATIAL_TYPE::PHYSIC_OBJECT |
		ESPATIAL_TYPE::INV_BOX |
		ESPATIAL_TYPE::AI_DOOR |
		ESPATIAL_TYPE::LIGHT_LAMP |
		ESPATIAL_TYPE::PHYSIC_OBJECT_DESTR |
		ESPATIAL_TYPE::PHYSIC_MOVEMENT |
		ESPATIAL_TYPE::ROCKET |
		ESPATIAL_TYPE::PHYSIC_SHELL_HOLDER |
		ESPATIAL_TYPE::PHYSIC_OBJECT_BRKBL)
		: ESPATIAL_TYPE::NONE;

	ESPATIAL_TYPE zone_flags = 
		m_flags.test(ESCENE_FLAGS::ESF_DRAW_ZONES) ? 
		(	(m_zone_flags.test(EZONE_INFO::EZI_RESTR) ? ESPATIAL_TYPE::SPACE_RESTRICTOR : ESPATIAL_TYPE::NONE) |
			(m_zone_flags.test(EZONE_INFO::EZI_SMART_TERRAIN) ? ESPATIAL_TYPE::SMART_TERRAIN : ESPATIAL_TYPE::NONE) |
			(m_zone_flags.test(EZONE_INFO::EZI_LEVEL_CHANGER) ? ESPATIAL_TYPE::LEVEL_CHANGER : ESPATIAL_TYPE::NONE) |
			(m_zone_flags.test(EZONE_INFO::EZI_SMART_COVER) ? ESPATIAL_TYPE::SMART_COVER : ESPATIAL_TYPE::NONE) |
			(m_zone_flags.test(EZONE_INFO::EZI_CAMP_ZONE) ? ESPATIAL_TYPE::CAMP_ZONE : ESPATIAL_TYPE::NONE) |
			(m_zone_flags.test(EZONE_INFO::EZI_ANOMALY_ZONE) ? ESPATIAL_TYPE::ANOMALY_ZONE : ESPATIAL_TYPE::NONE) |
			(m_zone_flags.test(EZONE_INFO::EZI_ANOMAL_ZONE_LOGIC) ? ESPATIAL_TYPE::ANOMAL_ZONE_LOGIC : ESPATIAL_TYPE::NONE))
		: ESPATIAL_TYPE::NONE;

	g_SpatialSpace->q_frustum(m_objects, 0, dyn_flags | zone_flags, Render->ViewBase);

	for (ISpatialShared& spatial : m_objects)
	{
		if (!spatial.get()) continue;

		float opt_dist = cam_pos.distance_to_sqr(spatial->spatial.sphere.P);
		float fog_dist = visible_currents ? 50000.f : g_pGamePersistent->pEnvironment->CurrentEnv->fog_distance;
		if (opt_dist >= _sqr(fog_dist + spatial->spatial.sphere.R))
			continue;

		CObject* _O = spatial->dcast_CObject();
		CGameObject* GO = (_O && !_O->getDestroy()) ? _O->cast_game_object() : nullptr;
		if (!GO) continue;

		if (GO->Visual())
		{
			if (!m_skeleton_flags.test(ESKELETON_INFO::ESI_ACTOR) && g_actor == GO->cast_actor())
				continue;

			DrawSkeleton(GO->Visual()->dcast_PKinematics(), GO->XFORM(), GO);

			if (((spatial->spatial.type & ESPATIAL_TYPE::STALKER_ALIVE) != ESPATIAL_TYPE::NONE ||
				(spatial->spatial.type & ESPATIAL_TYPE::ACTOR_ALIVE) != ESPATIAL_TYPE::NONE))
			{
				if (CInventoryOwner* IO = smart_cast<CInventoryOwner*>(GO))
				{
					for (CAttachableItem* item : IO->attached_objects())
						DrawSkeleton(item->object().Visual()->dcast_PKinematics(), item->object().XFORM(), &item->object());

					if (PIItem item = IO->inventory().ActiveItem())
						DrawSkeleton(item->object().Visual()->dcast_PKinematics(), item->object().XFORM(), &item->object());

					if (CEntityAlive* CurrEntity = GO->cast_entity_alive(); CurrEntity == Actor())
					{
						PIItem rWeapon = IO->inventory().ItemFromSlot(INV_SLOT_3);
						bool rValid = rWeapon ? rWeapon->BaseSlot() == INV_SLOT_3 : false;
						if (rWeapon && rValid && rWeapon != IO->inventory().ActiveItem())
							DrawSkeleton(rWeapon->object().Visual()->dcast_PKinematics(), rWeapon->object().XFORM(), &rWeapon->object());

						PIItem lWeapon = IO->inventory().ItemFromSlot(INV_SLOT_2);
						bool lValid = lWeapon ? lWeapon->BaseSlot() == INV_SLOT_3 : false;
						if (lWeapon && lValid && lWeapon != IO->inventory().ActiveItem())
							DrawSkeleton(lWeapon->object().Visual()->dcast_PKinematics(), lWeapon->object().XFORM(), &lWeapon->object());

						auto lWeapon2 = IO->inventory().ItemFromSlot(PISTOL_SLOT_NEW);
						bool lValid2 = lWeapon2 ? lWeapon2->BaseSlot() == INV_SLOT_3 : false;
						if (lWeapon2 && lValid2 && lWeapon2 != IO->inventory().ActiveItem())
							DrawSkeleton(lWeapon2->object().Visual()->dcast_PKinematics(), lWeapon2->object().XFORM(), &lWeapon2->object());
					}
				}
			}
		}
		//if ((spatial->spatial.type & ESPATIAL_TYPE::STALKER_ALIVE) != ESPATIAL_TYPE::NONE)
		//{
		//	if (CAI_Stalker* stlk = GO->cast_stalker())
		//	{
		//		for (auto& feelobject : stlk->feel_visible)
		//		{
		//			Fvector C1, C2;
		//			stlk->Center(C1);
		//			feelobject.O->Center(C2);
		//
		//			append_text3d(C2, shared_str().printf("fuzzy: %f", feelobject.fuzzy));
		//			append_text_next(shared_str().printf("Cache_vis: %f", feelobject.Cache_vis));
		//			append_text_next(shared_str().printf("bone: %s", PKinematics(feelobject.O->Visual())->LL_BoneName_dbg(feelobject.bone_id)));
		//			append_line({ C1, C2, color_rgba(255, 0, 0, 255) });
		//		}
		//	}
		//}

		if (m_skeleton_flags.test(ESKELETON_INFO::ESI_PH_BBOX) && (((spatial->spatial.type & ESPATIAL_TYPE::STALKER_ALIVE) != ESPATIAL_TYPE::NONE ||
			(spatial->spatial.type & ESPATIAL_TYPE::ACTOR_ALIVE) != ESPATIAL_TYPE::NONE ||
			(spatial->spatial.type & ESPATIAL_TYPE::MONSTER_ALIVE) != ESPATIAL_TYPE::NONE) &&
			(spatial->spatial.type & ESPATIAL_TYPE::PHYSIC_MOVEMENT) != ESPATIAL_TYPE::NONE))
		{
			if (GO->cast_entity_alive() &&
				GO->cast_entity_alive()->character_physics_support() &&
				GO->cast_entity_alive()->character_physics_support()->movement()->CharacterExist())
			{
				const Fmatrix& trans = GO->XFORM();
				Fvector box_c, box_hs;
				GO->cast_entity_alive()->character_physics_support()->movement()->Box().get_CD(box_c, box_hs);
				//Fobb obb;
				//obb.m_rotate.set(trans);
				//trans.transform_tiny(obb.m_translate, box_c);
				//obb.m_halfsize = box_hs;
				//append_obb(obb, color_rgba(0, 255, 0, 100));
				Fbox aabb;
				trans.transform_tiny(box_c);
				aabb.setb(box_c, box_hs);
				append_aabb(aabb, color_rgba(0, 255, 0, 100));
			}
		}

		
		if ((spatial->spatial.type & ESPATIAL_TYPE::SHAPE) != ESPATIAL_TYPE::NONE)
		{
			CCF_Shape* ccfshape = (CCF_Shape*)(GO->CFORM());

			u32 line_color = pSettings->line_exist(GO->cNameSect_str(), "shape_edge_color") ? pSettings->r_color(GO->cNameSect_str(), "shape_edge_color") : color_rgba(32, 32, 32, 255);
			u32 tri_color = pSettings->line_exist(GO->cNameSect_str(), "shape_transp_color") ? pSettings->r_color(GO->cNameSect_str(), "shape_transp_color") : color_rgba(128, 128, 128, 60);
			auto& shapes = ccfshape->shapes;
			
			for (int i = 0; i < shapes.size(); ++i)
			{
				u32 tritmp_color = tri_color;
				if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_SELECTION) && RQ.O == GO)
				{
					if(RQ.element == i)
					{
						Fcolor t_clr;
						t_clr.set(tri_color);
						t_clr.mul_rgba(1.5f);
						tritmp_color = t_clr.get();
					}
					else
					{
						Fcolor t_clr;
						t_clr.set(tri_color);
						t_clr.mul_rgba(1.2f);
						tritmp_color = t_clr.get();
					}
				}

				CCF_Shape::shape_def& shape = shapes[i];
				switch (shape.type)
				{
				case 0:
				{
					Fsphere sphere = shape.data.sphere;
					GO->XFORM().transform_tiny(sphere.P);
					append_sphere(sphere, line_color, tritmp_color);
					if (visible_currents && m_selection_flags.test(ESELECTION_FLAGS::ESLF_Z))
					{
						Fvector C = sphere.P;
						C.y += sphere.R;
						DrawObjectInfo(GO, C, { 0.f, 0.f });
					}
				}break;
				case 1:
				{
					Fmatrix matrix = shape.data.box;
					matrix.mulA_43(GO->XFORM());
					Fobb obb; obb.xform_set(matrix);
					obb.m_halfsize.set(0.5f, 0.5f, 0.5f);
					append_obb(obb, line_color, tritmp_color);
					if (visible_currents && m_selection_flags.test(ESELECTION_FLAGS::ESLF_Z))
					{
						Fvector C = obb.m_translate;
						C.y += obb.m_rotate.j.magnitude() * 0.5f;
						DrawObjectInfo(GO, C, { 0.f, 0.f });
					}
				}break;
				default: NODEFAULT;
				}
			}
		}
		else
		{
			if ((!GO->Visual() || !GO->CFORM()) && (spatial->spatial.type & ESPATIAL_TYPE::LADDER) == ESPATIAL_TYPE::NONE)
			{
				u32 line_color = pSettings->line_exist(GO->cNameSect_str(), "shape_edge_color") ? pSettings->r_color(GO->cNameSect_str(), "shape_edge_color") : color_rgba(32, 32, 32, 255);
				u32 tri_color = pSettings->line_exist(GO->cNameSect_str(), "shape_transp_color") ? pSettings->r_color(GO->cNameSect_str(), "shape_transp_color") : color_rgba(128, 128, 128, 60);

				Fsphere sphere;
				GO->Center(sphere.P);
				float rad = GO->Radius();
				sphere.R = rad < .15f ? .15f : rad;
				append_sphere(sphere, line_color, tri_color);

				if(m_selection_flags.test(ESELECTION_FLAGS::ESLF_O) && sphere.intersect2(RD.start, RD.dir, RD.range))
				{
					sphere.P.y += sphere.R;
					DrawObjectInfo(GO, sphere.P, { 0.f, 0.f });
				}
			}

			if (visible_currents && m_selection_flags.test(ESELECTION_FLAGS::ESLF_O))
			{
				Fvector C;
				GO->Center(C);
				C.y += GO->Radius();
				DrawObjectInfo(GO, C, { 0.f, 0.f });
			}
		}
		
		if ((spatial->spatial.type & ESPATIAL_TYPE::LADDER) != ESPATIAL_TYPE::NONE)
		{
			CClimableObject* ladder = GO->cast_climable_object();
			Fobb obb = ladder->BBox();
			Fmatrix trans;
			obb.xform_get(trans);
			trans.mulA_43(ladder->XFORM());
			obb.xform_set(trans);
			u32 tri_color = color_rgba(0, 255, 0, 10);
			if (m_flags.test(ESCENE_FLAGS::ESF_DRAW_SELECTION) && m_selection_flags.test(ESELECTION_FLAGS::ESLF_O) && obb.intersect(RD.start, RD.dir, RD.range))
			{
				Fvector C;
				GO->Center(C);
				C.y += GO->Radius();
				DrawObjectInfo(GO->cast_game_object(), C, { 0.f, 0.f });

				Fcolor t_clr;
				t_clr.set(tri_color);
				t_clr.mul_rgba(1.5f);
				tri_color = t_clr.get();
			}

			append_obb(obb, color_rgba(0, 255, 0, 255), tri_color);

			Fvector p1, p2, d;
			d.set(ladder->Axis());
			p1.add(ladder->XFORM().c, d);
			p2.sub(ladder->XFORM().c, d);
			append_line({ p1, p2, color_rgba(255, 0, 0, 255) });

			d.set(ladder->Side());
			p1.add(ladder->XFORM().c, d);
			p2.sub(ladder->XFORM().c, d);
			append_line({ p1, p2, color_rgba(255, 0, 0, 255) });

			d.set(ladder->Norm());
			d.mul(trans.j.magnitude() * 2.f);
			p1.add(ladder->XFORM().c, d);
			p2.set(ladder->XFORM().c);
			append_line({ p1, p2, color_rgba(0, 255, 0, 255) });
		}
	}
}