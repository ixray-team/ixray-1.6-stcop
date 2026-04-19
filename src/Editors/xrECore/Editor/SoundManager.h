//---------------------------------------------------------------------------
#ifndef SoundManagerH
#define SoundManagerH


// refs
class ESoundThumbnail;

class ECORE_API CSoundManager
{
public:
	void 		MakeGameSound		(ESoundThumbnail* THM, const char* src_name, const char* game_name);
				CSoundManager		();
	virtual		~CSoundManager		(){;}
                    
    bool  RemoveSound		(const char* fname, EItemType type);
	void  RenameSound		(const char* p0, const char* p1, EItemType type);

	// texture routines
    int			GetSounds			(FS_FileSet& files, bool bFolders=false);
    int			GetGameSounds		(FS_FileSet& files);
    int			GetSoundEnvs		(AStringVec& items);

	int 		GetLocalNewSounds	(FS_FileSet& files);
//	void		SafeCopyLocalToServer(FS_FileSet& files);
	void		SynchronizeSounds	(bool sync_thm, bool sync_game, bool bForceGame, FS_FileSet* source_map, AStringVec* sync_list_without_extention, FS_FileSet* modif_map=nullptr);
//	void 		ChangeFileAgeTo		(FS_FileSet* tgt_map, int age);
    void		CreateSoundThumbnail(ESoundThumbnail* THM, const xr_string& src_name, const char* path=nullptr, bool bSetDefParam=true);
	void		CleanupSounds		(bool IsSoft);

    bool		OnCreate			();
    void		OnDestroy			();

    virtual void OnFrame			();

    virtual bool Validate			(){return true;}

    void		MuteSounds			(bool bVal);

    void 		RefreshSounds		(bool bSync, bool IsSoft);

	xr_string	UpdateFileName		(xr_string& fn);
};

extern ECORE_API CSoundManager* SndLib;
//---------------------------------------------------------------------------
#endif
