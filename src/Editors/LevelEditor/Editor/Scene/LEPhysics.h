#pragma once

#include "../../xrphysics/xrphysics.h"

class CObjectSpace;
class CScenePhysics
{
	 CObjectSpace *m_object_space ;
     bool		   b_update_level_collision;
 public:
 	CScenePhysics			() : m_object_space(0), b_update_level_collision(false)	{}
	~CScenePhysics			() ;
 public:
     void	GenerateCForm(CObjectSpace*To, CDB::build_callback cb);
    void	CreateWorld			();
    void 	DestroyWorld		();
    void	CreateShellsSelected();
    void	DestroyAll			();
    void	UseSimulatePoses	();
    void	UpdateLevelCollision(){ b_update_level_collision=true; }
    void	OnSceneModified		();
    bool	Simulating			();
private:
    bool 	CreateObjectSpace	(bool b_selected_only);
    void 	DestroyObjectSpace	();
};

  extern 	CScenePhysics	g_scene_physics;
