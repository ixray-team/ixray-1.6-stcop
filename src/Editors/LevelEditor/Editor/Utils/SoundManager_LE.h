#pragma once

class ESoundThumbnail;

class CLevelSoundManager: public CSoundManager
{
	typedef CSoundManager inherited;
	bool		bNeedRefreshEnvGeom;
    void		RealRefreshEnvGeometry();
	void 		MakeGameSound		(ESoundThumbnail* THM, const char* src_name, const char* game_name);
public:
				CLevelSoundManager	(){bNeedRefreshEnvGeom = false;}
				~CLevelSoundManager	(){;}

    virtual void OnFrame			();

    void		RefreshEnvLibrary	();
    void		RefreshEnvGeometry	(){bNeedRefreshEnvGeom = true;}

    bool		Validate			();
    bool        MakeEnvGeometry(CMemoryWriter& F, bool bErrMsg = false);
    void		MuteSounds			(bool bVal);
    void 		RefreshSounds		(bool bSync);
    xr_string	UpdateFileName		(xr_string& fn);
};

extern CLevelSoundManager* LSndLib;