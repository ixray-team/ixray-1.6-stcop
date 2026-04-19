#ifndef LocatorAPI_NotificationsH
#define LocatorAPI_NotificationsH
#pragma once

class	CThread
{
	static void				startup			(void* P);
protected:
	volatile u32			thID;
	volatile bool			Terminated;
public:
							CThread			(u32 _ID)
	{
		thID				= _ID;
        Terminated			= false;
    }
	virtual 				~CThread		(){}
	void					Start			()
	{
		thread_spawn		(startup,"FS-notify",0,this);
	}
	virtual	void			Execute			() = 0;
    void					Terminate		() {Terminated=true;}
};

class CFS_PathNotificator : public CThread
{
private:
    struct Path	{
    	shared_str						FDirectory;
        void* 							FWaitHandle;
        fastdelegate::FastDelegate0<>	FChangeEvent;
		bool 						bRecurse;
    };

	using HANDLEVec = xr_vector<HANDLE>;
	using HANDLEIt = HANDLEVec::iterator;

    using PathVec = xr_vector<Path>;
	using PathIt = PathVec::iterator;

	PathVec					events;
public:
	void* 					FMutex;
	unsigned 				FNotifyOptionFlags;
protected:
	virtual void  			Execute				();
public:
					 		CFS_PathNotificator	();
	virtual 				~CFS_PathNotificator();
    void					RegisterPath		(FS_Path& path);
};

#endif // LocatorAPI_borlandH
