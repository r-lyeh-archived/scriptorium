/*
// Dao Virtual Machine
// http://www.daovm.net
//
// Copyright (c) 2006-2015, Limin Fu
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:
//
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED  BY THE COPYRIGHT HOLDERS AND  CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED  WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO,  THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL  THE COPYRIGHT HOLDER OR CONTRIBUTORS  BE LIABLE FOR ANY DIRECT,
// INDIRECT,  INCIDENTAL, SPECIAL,  EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES (INCLUDING,
// BUT NOT LIMITED TO,  PROCUREMENT OF  SUBSTITUTE  GOODS OR  SERVICES;  LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION)  HOWEVER CAUSED  AND ON ANY THEORY OF
// LIABILITY,  WHETHER IN CONTRACT,  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
// OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef DAO_TASKLET_H
#define DAO_TASKLET_H

#include"daoVmspace.h"



enum DaoTaskEventType
{
	DAO_EVENT_RESUME_TASKLET ,  /* Resume the tasklet; */
	DAO_EVENT_WAIT_TASKLET   ,  /* Wait for another tasklet; */
	DAO_EVENT_WAIT_RECEIVING ,  /* Wait for receiving from a channel; */
	DAO_EVENT_WAIT_SENDING   ,  /* Wait after sending to a channel; */
	DAO_EVENT_WAIT_SELECT       /* Wait for multiple futures or channels; */
};
enum DaoTaskEventState
{
	DAO_EVENT_WAIT ,
	DAO_EVENT_RESUME  /* Ensure the processing of timed-out events; */
};

enum DaoTaskStatus
{
	DAO_CALL_RUNNING ,
	DAO_CALL_PAUSED ,
	DAO_CALL_FINISHED ,
	DAO_CALL_ABORTED
};


typedef struct DaoTaskEvent  DaoTaskEvent;

/*
// Task event for scheduling.
//
// A task event can be generated in different situation:
// 1. Starting of a new tasklet by calling mt.start::{} or asynchronous methods:
//    DaoTaskEvent {
//        type = DAO_EVENT_RESUME_TASKLET;
//        future = future value for the new tasklet;
//        channel = NULL;
//    };
// 2. Waiting for a tasklet (future value):
//    DaoTaskEvent {
//        type = DAO_EVENT_WAIT_TASKLET/DAO_EVENT_RESUME_TASKLET;
//        future = future value for the waiting tasklet;
//        channel = NULL;
//    };
// 3. Waiting to Receive message from a channel:
//    DaoTaskEvent {
//        type = DAO_EVENT_WAIT_RECEIVING;
//        future = future value for the waiting tasklet;
//        channel = channel for receiving;
//    };
// 4. Waiting after sending message to a channel:
//    DaoTaskEvent {
//        type = DAO_EVENT_WAIT_SENDING;
//        future = future value for the sending tasklet;
//        channel = channel for sending;
//    };
// 5. Waiting for one of the future values or channels becoming ready:
//    DaoTaskEvent {
//        type = DAO_EVENT_WAIT_SELECT;
//        future = future value for the waiting tasklet;
//        channel = NULL;
//        channels = select list of channels;
//    };
//
*/
struct DaoTaskEvent
{
	uchar_t      type;
	uchar_t      state;
	uchar_t      timeout;
	uchar_t      auxiliary;
	double       expiring;  /* expiring time for a timeout event; */
	DaoFuture   *future;
	DaoChannel  *channel;
	DaoValue    *message;
	DaoValue    *selected;
	DaoMap      *selects;  /* DHash<DaoFuture*|DaoChannel*,0|1>; */
};



/*
// Channel for synchronous and asynchronous communication between tasklet.
//
// Each channel has a data buffer that holds data send to the channel.
// Each buffer has a cap/capacity limit, and if the number of data items
// has reached the cap, a sender will block when it sends data to this channel.
//
// Each time a receiver of a channel reads out one data item of the
// channel buffer, which will effectively move the rest data items
// forward and may cause one data item to enter the cap region.
// When this happens, the sender of this data item will be unblocked.
//
// If the buffer cap is zero, it will effectively block any sender,
// which is unblock only when the data item it sent has been read out.
*/
struct DaoChannel
{
	DAO_CSTRUCT_COMMON;

	daoint   cap;     /* capacity limit of the channel; */
	DList   *buffer;  /* DList<DaoValue*>; */
};



/*
// Future value for tasklet.
//
// Each tasklet is represented by a future value.
*/
struct DaoFuture
{
	DAO_CSTRUCT_COMMON;

	uchar_t      state;
	uchar_t      timeout;
	uchar_t      aux1;
	uchar_t      aux2;
	DaoValue    *value;
	DaoValue    *message;
	DaoValue    *selected;
	DaoObject   *actor;
	DaoProcess  *process;
	DaoFuture   *precond; /* the future value on which this one waits; */
};

DAO_DLL DaoType *dao_type_future;
DAO_DLL DaoFuture*  DaoFuture_New( DaoType *type, int vatype );

DAO_DLL void DaoCallServer_AddCall( DaoProcess *call );

#ifdef DAO_WITH_CONCURRENT

DAO_DLL DaoType *dao_type_channel;
DAO_DLL DaoChannel* DaoChannel_New( DaoType *type, int dtype );

DAO_DLL void DaoChannel_Send( DaoChannel *self, DaoValue *data );
DAO_DLL void DaoChannel_ActivateEvent( DaoChannel *self, int type );
DAO_DLL void DaoFuture_ActivateEvent( DaoFuture *self );

DAO_DLL void DaoProcess_ReturnFutureValue( DaoProcess *self, DaoFuture *future );


DAO_DLL int DaoCallServer_GetThreadCount();
DAO_DLL void DaoCallServer_Join();
DAO_DLL void DaoCallServer_Stop();
DAO_DLL void DaoCallServer_AddThread( DThreadTask func, void *param );
DAO_DLL void DaoCallServer_AddTask( DThreadTask func, void *param, int now );
DAO_DLL void DaoCallServer_AddWait( DaoProcess *wait, DaoFuture *future, double timeout );

#endif

#endif
