/*
* Copyright(c) 2015 Chew Esmero
* All rights reserved.
*/

#include <ntddk.h>
#include <wdmsec.h>                         /* for IoCreateDeviceSecure */
#include <windef.h>
#include <unknown.h>
#include <ks.h>
#include <ksmedia.h>
#include <kcom.h>
#include <initguid.h>
#include <dontuse.h>
#include <GuidDef.h>
#include <uuids.h>

#define L(msg, ...) KdPrintEx((DPFLTR_IHVDRIVER_ID, DPFLTR_ERROR_LEVEL, msg, __VA_ARGS__));

/*
 * GUID definition are required to be outside of header inclusion pragma to avoid error during precompiled headers.
 *
 * {41966169-3FD7-4392-AFE4-E6A9D0A92C72}
 */
DEFINE_GUID(GUID_SD_FILTER_CONTROL_OBJECT,
        0x41966169, 0x3fd7, 0x4392, 0xaf, 0xe4, 0xe6, 0xa9, 0xd0, 0xa9, 0x2c, 0x72);

/*
 * Internal definitions of related GUIDs
 */
#define STATIC_KSFORMAT_SUBTYPE_YUY2 \
    0x32595559L, 0x0000, 0x0010, 0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71

#define STATIC_KSFORMAT_SUBTYPE_NV12 \
    0x3231564eL, 0x0000, 0x0010, 0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71

#define STATIC_KSFORMAT_SUBTYPE_MJPG \
    0x47504a4dL, 0x0000, 0x0010, 0x80, 0x00, 0x00, 0xaa, 0x00, 0x38, 0x9b, 0x71

#if !defined(__LENCFLTR_H__)
#define __LENCFLTR_H__

/*
 * Required to build driver in Win2K and XP build environment.
 */
#ifndef STATUS_CONTINUE_COMPLETION

/*
 * This value should be returned from completion routines to continue completing the IRP upwards. Otherwise,
 * STATUS_MORE_PROCESSING_REQUIRED should be returned.
 */
#define STATUS_CONTINUE_COMPLETION          STATUS_SUCCESS

#endif

#define FN                                  __FUNCTION__

#define GUIDSTR_KSPIN_NAME                  L"{146F1A80-4791-11D0-A5D6-28DB04C10000}\\"

#define REG_FLTR                            L"\\Registry\\Machine\\SOFTWARE\\Chew\\CCFLTR"
#define REG_INTRNL_STRM                     L"\\Registry\\Machine\\SOFTWARE\\Chew\\Set\\{64E27A2E-B143-433E-B97D-F92326103C08}"
#define KEY_KSSTATE_RUN                     L"\\Registry\\Machine\\SOFTWARE\\Chew\\Set\\{64E27A2E-B143-433E-B97D-F92326103C10}"

#define POOL_TAG                            'liFT'

#define FOURCC_DMP(dw, a, b, c, d) \
{ \
    WORD hw = HIWORD(dw); \
    WORD lw = LOWORD(dw); \
    *a = (CHAR)HIBYTE(hw); \
    *b = (CHAR)LOBYTE(hw); \
    *c = (CHAR)HIBYTE(lw); \
    *d = (CHAR)LOBYTE(lw); \
}

/*
 * These are the states Filter transition to upon receiving a specific PnP Irp. Refer to the PnP Device States diagram
 * in DDK documentation for better understanding.
 */
typedef enum _DEVICE_PNP_STATE {
    NotStarted = 0,                         /* not started yet */
    Started,                                /* device has received the START_DEVICE IRP */
    StopPending,                            /* device has received the QUERY_STOP IRP */
    Stopped,                                /* device has received the STOP_DEVICE IRP */
    RemovePending,                          /* device has received the QUERY_REMOVE IRP */
    SurpriseRemovePending,                  /* device has received the SURPRISE_REMOVE IRP */
    Deleted                                 /* device has received the REMOVE_DEVICE IRP */
} DEVICE_PNP_STATE;

#define INITIALIZE_PNP_STATE(_Data_) \
	(_Data_)->DevicePnPState =  NotStarted; \
	(_Data_)->PreviousPnPState = NotStarted;

#define SET_NEW_PNP_STATE(_Data_, _state_) \
	(_Data_)->PreviousPnPState = (_Data_)->DevicePnPState; \
	(_Data_)->DevicePnPState = (_state_);

#define RESTORE_PREVIOUS_PNP_STATE(_Data_) \
	(_Data_)->DevicePnPState = (_Data_)->PreviousPnPState;

typedef enum _DEVICE_TYPE {
    DEVICE_TYPE_INVALID = 0,                /* invalid type */
    DEVICE_TYPE_FIDO,                       /* device is a filter device */
    DEVICE_TYPE_CDO,                        /* device is a control device */
} DEVICE_TYPE;

typedef enum _EVENT_IDX {
    EVENT_CTRL = 0,                         /* for actual trigger event */
    EVENT_EXIT,                             /* for termination event */
    EVENT_COUNT,                            /* total count (for checking) */
} EVENT_IDX;

/* A common header for the device extensions of the Filter and control device objects. */
typedef struct _COMMON_DEVICE_DATA {
    DEVICE_TYPE Type;
} COMMON_DEVICE_DATA, *PCOMMON_DEVICE_DATA;

/* Frame header. */
typedef struct __SHAREDBUFFER_HEADER {
    DWORD dwFrameType;                      /* compression hex (Data1 part of the GUID) */
    DWORD cbFrameSize;                      /* frame size in bytes */
    DWORD dwWidth;                          /* frame width */
    DWORD dwHeight;                         /* frame height */
} SHAREDBUFFER_HEADER, *PSHAREDBUFFER_HEADER;

typedef struct _DEVICE_EXTENSION {
    COMMON_DEVICE_DATA Common;              /* common data for both filter and control objects */
    PDEVICE_OBJECT Self;                    /* back pointer to the device object */
    PDEVICE_OBJECT NextLowerDriver;         /* top of stack before this filter was added */
    PDEVICE_OBJECT PhysicalDeviceObject;    /* our driver stack PDO */
    DEVICE_PNP_STATE DevicePnPState;        /* current PnP state of the device */
    DEVICE_PNP_STATE PreviousPnPState;      /* remembers the previous pnp state */
    IO_REMOVE_LOCK RemoveLock;              /* track IRPs so that device can be removed and the driver can be unloaded safely */
    LONG RemoveLockCount;                   /* debug: track RemoveLock calls */
    KEVENT ControlLock;                     /* synchronize multiple threads creating & deleting control deviceobjects */
    ULONG InstanceCount;                    /* ControlDeviceObject instance count */
    PDEVICE_OBJECT ControlDeviceObject;     /* sideband object */
    KS_DATAFORMAT_VIDEOINFOHEADER VideoInfoHeader;      /* stream video information header (v1) */
    KS_DATAFORMAT_VIDEOINFOHEADER2 VideoInfoHeader2;    /* stream video information header (v2) */
    LONG IsVideoInfoHeader2;                /* nonzero if version 2 header is used */
    LONG IsPdoSupported;                    /* nonzero if camera is supported */
    LPBYTE StreamBuffer;                    /* local copy of stream buffer */
    LONG DataUsed;                          /* size of 'StreamBuffer' from stream header */
    LONG StreamerPid;                       /* internal streamer's process ID */
    LONG GlobalStreamOpen;                  /* '\Global..." open monitor count */
    LONG PinNameOpen;                       /* pin name open monitor count */
    PKTHREAD ThreadRestream;                /* 'restream' system thread */
    KEVENT EventTerm;                       /* 'restream' system thread terminate event */
    KEVENT EventCtrl;                       /* 'restream' system thread trigger event */
    LONG KsStateRun;                        /* nonzero when streaming is active */
} DEVICE_EXTENSION, *PDEVICE_EXTENSION;

DRIVER_INITIALIZE DriverEntry;
DRIVER_ADD_DEVICE FilterAddDevice;
__drv_dispatchType(IRP_MJ_PNP)
DRIVER_DISPATCH FilterDispatchPnp;
__drv_dispatchType(IRP_MJ_POWER)
DRIVER_DISPATCH FilterDispatchPower;
__drv_dispatchType_other
DRIVER_DISPATCH FilterPass;

DRIVER_UNLOAD FilterUnload;
IO_COMPLETION_ROUTINE FilterDeviceUsageNotificationCompletionRoutine;
IO_COMPLETION_ROUTINE FilterStartCompletionRoutine;
IO_COMPLETION_ROUTINE WaitComplete;
IO_COMPLETION_ROUTINE ModifyStream;

PCHAR PnPMinorFunctionString(UCHAR MinorFunction);

#define NTDEVICE_NAME_STRING                L"\\Device\\CCamFilter"
#define SYMBOLIC_NAME_STRING                L"\\DosDevices\\CCamFilter"

#define IOCTL_LENCFLTR_TEST                 CTL_CODE(FILE_DEVICE_UNKNOWN, 0x800, METHOD_OUT_DIRECT, FILE_ANY_ACCESS)
#define IOCTL_LENCFLTR_TEST_SM              CTL_CODE(FILE_DEVICE_UNKNOWN, 0x801, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_LENCFLTR_TEST_X               CTL_CODE(FILE_DEVICE_UNKNOWN, 0x802, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_LENCFLTR_BUFFER_REFRESH       CTL_CODE(FILE_DEVICE_UNKNOWN, 0x803, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_LENCFLTR_INTRNAL_STRM_START   CTL_CODE(FILE_DEVICE_UNKNOWN, 0x804, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define IOCTL_LENCFLTR_INTRNAL_STRM_STOP    CTL_CODE(FILE_DEVICE_UNKNOWN, 0x805, METHOD_BUFFERED, FILE_ANY_ACCESS)

typedef struct _CONTROL_DEVICE_EXTENSION {
    COMMON_DEVICE_DATA Common;              /* common data */
    ULONG Deleted;                          /* FALSE if the deviceobject is valid, TRUE if it's deleted */
    PVOID ControlData;                      /* store our control data here */
    PDEVICE_EXTENSION FilterExtension;      /* this filter's device extension */
    PDEVICE_OBJECT Self;                    /* back pointer to our device object */
} CONTROL_DEVICE_EXTENSION, *PCONTROL_DEVICE_EXTENSION;

NTSTATUS FilterCreateControlObject(__in PDEVICE_OBJECT DeviceObject);
VOID FilterDeleteControlObject(__in PDEVICE_OBJECT DeviceObject);
VOID StreamMonitorSystemThread(PDEVICE_EXTENSION DeviceExtension);

/*
 * Control device object related dispatch functions.
 */
NTSTATUS ControlDispatchIoTestDirect(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp);
NTSTATUS ControlDispatchIoBufferRefresh(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp);
NTSTATUS ControlDispatchIoStreamStart(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp);
NTSTATUS ControlDispatchIoStreamStop(__in PDEVICE_OBJECT DeviceObject, __in PIRP Irp);

/*
 * Helper functions.
 */
void PrintGuidValues(PCHAR pszFn, PWCHAR pszLabel, LPGUID lpGuid);

__drv_dispatchType(IRP_MJ_CREATE)
__drv_dispatchType(IRP_MJ_CLOSE)
__drv_dispatchType(IRP_MJ_CLEANUP)
__drv_dispatchType(IRP_MJ_DEVICE_CONTROL)
DRIVER_DISPATCH FilterDispatchIo;

#endif /* __LENCFLTR_H__ */
