{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Windows Messages communication library

  Server endpoint class

  ©František Milt 2017-07-18

  Version 1.4.1

  Dependencies:
    AuxTypes       - github.com/ncs-sniper/Lib.AuxTypes
    AuxClasses     - github.com/ncs-sniper/Lib.AuxClasses
    CRC32          - github.com/ncs-sniper/Lib.CRC32
    UtilityWindow  - github.com/ncs-sniper/Lib.UtilityWindow
    MulticastEvent - github.com/ncs-sniper/Lib.MulticastEvent
    WndAlloc       - github.com/ncs-sniper/Lib.WndAlloc
    BitVector      - github.com/ncs-sniper/Lib.BitVector
    StrRect        - github.com/ncs-sniper/Lib.StrRect
    BitOps         - github.com/ncs-sniper/Lib.BitOps
  * SimpleCPUID    - github.com/ncs-sniper/Lib.SimpleCPUID

  SimpleCPUID might not be needed, see BitOps library for details.

===============================================================================}
unit WinMsgCommServer;

{$INCLUDE '.\WinMsgComm_defs.inc'}

interface

uses
  Windows, UtilityWindow, WinMsgComm;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TWinMsgCommServer                               }
{------------------------------------------------------------------------------}
{==============================================================================}
type
  TWinMsgCommServer = class(TWinMsgCommBase)
  private
    fControlMutex:        THandle;
    fOnClientConnect:     TWMCConnectionEvent;
    fOnClientDisconnect:  TWMCConnectionEvent;
  protected
    Function ProcessMessage(SenderID: TWMCConnectionID; MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam): lResult; override;
  public
    class Function OtherServerRuning(const MessageName: String): Boolean; virtual;
    constructor Create(Window: TUtilityWindow = nil; Synchronous: Boolean = False; const MessageName: String = WMC_MessageName); override;
    destructor Destroy; override;
    Function SendMessage(MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam; RecipientID: TWMCConnectionID = WMC_SendToAll): lResult; override;
  published
    property OnClientConnect: TWMCConnectionEvent read fOnClientConnect write fOnClientConnect;
    property OnClientDisconnect: TWMCConnectionEvent read fOnClientDisconnect write fOnClientDisconnect;
  end;

implementation

uses
  SysUtils;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TWinMsgCommServer                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TWinMsgCommServer - Protected methods                                      }
{==============================================================================}

Function TWinMsgCommServer.ProcessMessage(SenderID: TWMCConnectionID; MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam): lResult;
var
  NewClient:  TWMCConnectionInfo;
  Index:      Integer;
begin
case MessageCode of
  WMC_QUERYSERVER:    begin
                        SendMessageTo(HWND(Payload),BuildWParam(ID,WMC_SERVERONLINE,0),lParam(WindowHandle),False);
                        Result := WMC_RESULT_ok;
                      end;
  WMC_SERVERONLINE:   Result := WMC_RESULT_error;
  WMC_SERVEROFFLINE:  Result := WMC_RESULT_error;
  WMC_CLIENTONLINE:   begin
                        NewClient.Valid := True;
                        NewClient.ConnectionID := AcquireID;
                        NewClient.WindowHandle := HWND(Payload);
                        NewClient.Transacting := False;
                        Result := lResult(NewClient.ConnectionID);
                        Index := AddConnection(NewClient);
                        If Assigned(fOnClientConnect) then fOnClientConnect(Self,NewClient,Index);
                      end;
  WMC_CLIENTOFFLINE:  begin
                        Index := IndexOfConnection(SenderID);
                        If Index >= 0 then
                          begin
                            If Assigned(fOnClientDisconnect) then fOnClientDisconnect(Self,Connections[Index],Index);
                            DeleteConnection(Index);
                            ReleaseID(SenderID);
                            Result := WMC_RESULT_ok;
                          end
                        else Result := WMC_RESULT_error;
                      end;
  WMC_ISSERVER:       Result := WMC_RESULT_ok;
  WMC_QUERYPEERS:     Result := WMC_RESULT_error;
  WMC_PEERONLINE:     Result := WMC_RESULT_error;
  WMC_PEEROFFLINE:    Result := WMC_RESULT_error;
else
  Result := inherited ProcessMessage(SenderID,MessageCode,UserCode,Payload);
end;
end;

{==============================================================================}
{   TWinMsgCommServer - Public methods                                         }
{==============================================================================}

class Function TWinMsgCommServer.OtherServerRuning(const MessageName: String): Boolean;
var
  ControlMutex: THandle;
begin
ControlMutex := CreateMutex(nil,False,PChar(MessageName + '_mutex'));
try
  Result := not GetLastError = ERROR_ALREADY_EXISTS;
finally
  CloseHandle(ControlMutex);
end;
end;

//------------------------------------------------------------------------------

constructor TWinMsgCommServer.Create(Window: TUtilityWindow = nil; Synchronous: Boolean = False; const MessageName: String = WMC_MessageName);
begin
inherited Create(Window,Synchronous,MessageName);
InitIDArray;
fControlMutex := CreateMutex(nil,False,PChar(MessageName + '_mutex'));
If GetLastError = ERROR_ALREADY_EXISTS then
  raise Exception.Create('Server on this domain is already running.');
SetID(0);
SendMessageTo(HWND_BROADCAST,BuildWParam(ID,WMC_SERVERONLINE,0),lParam(WindowHandle),False);
end;

//------------------------------------------------------------------------------

destructor TWinMsgCommServer.Destroy;
begin
SendMessageTo(HWND_BROADCAST,BuildWParam(ID,WMC_SERVEROFFLINE,0),lParam(WindowHandle),False);
CloseHandle(fControlMutex);
FinalIDArray;
inherited;
end;

//------------------------------------------------------------------------------

Function TWinMsgCommServer.SendMessage(MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam; RecipientID: TWMCConnectionID = WMC_SendToAll): lResult;
begin
If RecipientID = WMC_SendToAll then
  Result := SendMessageToAll(BuildWParam(ID,MessageCode,UserCode),Payload)
else
  begin
    If fConnectionsArray[RecipientID].Valid then
      Result := SendMessageTo(fConnectionsArray[RecipientID].WindowHandle,BuildWParam(ID,MessageCode,UserCode),Payload)
    else
      Result := WMC_RESULT_error;
  end;
end;

end.
