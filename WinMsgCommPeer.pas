{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Windows Messages communication library

  Peer endpoint class

  ©František Milt 2017-02-27

  Version 1.4

  Dependencies:
    AuxTypes       - github.com/ncs-sniper/Lib.AuxTypes
    CRC32          - github.com/ncs-sniper/Lib.CRC32
    UtilityWindow  - github.com/ncs-sniper/Lib.UtilityWindow
    MulticastEvent - github.com/ncs-sniper/Lib.MulticastEvent
    WndAlloc       - github.com/ncs-sniper/Lib.WndAlloc
    BitVector      - github.com/ncs-sniper/Lib.BitVector
    BitOps         - github.com/ncs-sniper/Lib.BitOps

===============================================================================}
unit WinMsgCommPeer;

{$INCLUDE '.\WinMsgComm_defs.inc'}

interface

uses
  Windows, UtilityWindow, WinMsgComm;

{==============================================================================}
{------------------------------------------------------------------------------}
{                               TWinMsgCommPeer                                }
{------------------------------------------------------------------------------}
{==============================================================================}
type
  TWinMsgCommPeer = class(TWinMsgCommBase)
  private
    fOnPeerConnect:     TWMCConnectionEvent;
    fOnPeerDisconnect:  TWMCConnectionEvent;
  protected
    Function ProcessMessage(SenderID: TWMCConnectionID; MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam): lResult; override;
  public
    constructor Create(Window: TUtilityWindow = nil; Synchronous: Boolean = False; const MessageName: String = WMC_MessageName); override;
    destructor Destroy; override;
    Function SendMessage(MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam; RecipientID: TWMCConnectionID = WMC_SendToAll): lResult; override;
  published
    property OnPeerConnect: TWMCConnectionEvent read fOnPeerConnect write fOnPeerConnect;
    property OnPeerDisconnect: TWMCConnectionEvent read fOnPeerDisconnect write fOnPeerDisconnect;
  end;

implementation

{==============================================================================}
{------------------------------------------------------------------------------}
{                               TWinMsgCommPeer                                }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TWinMsgCommPeer - Protected methods                                        }
{==============================================================================}

Function TWinMsgCommPeer.ProcessMessage(SenderID: TWMCConnectionID; MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam): lResult;
var
  NewPeer:  TWMCConnectionInfo;
  Index:    Integer;
begin
case MessageCode of
  WMC_QUERYSERVER:    Result := WMC_RESULT_error;
  WMC_SERVERONLINE:   Result := WMC_RESULT_error;
  WMC_SERVEROFFLINE:  Result := WMC_RESULT_error;
  WMC_CLIENTONLINE:   Result := WMC_RESULT_error;
  WMC_CLIENTOFFLINE:  Result := WMC_RESULT_error;
  WMC_ISSERVER:       Result := WMC_RESULT_error;
  WMC_QUERYPEERS:     begin
                        If HWND(Payload) <> WindowHandle then
                          begin
                            SendMessageTo(HWND(Payload),BuildWParam(ID,WMC_PEERONLINE,0),lParam(WindowHandle),True);
                            Result := WMC_RESULT_ok;
                          end
                        else Result := WMC_RESULT_error;
                      end;
  WMC_PEERONLINE:     begin
                        Result := WMC_RESULT_ok;
                        NewPeer.Valid := True;
                        NewPeer.ConnectionID := SenderID;
                        NewPeer.WindowHandle := HWND(Payload);
                        NewPeer.Transacting := False;
                        Index := AddConnection(NewPeer);
                        If Assigned(fOnPeerConnect) then fOnPeerConnect(Self,NewPeer,Index);
                      end;
  WMC_PEEROFFLINE:    begin
                        Index := IndexOfConnection(SenderID);
                        If Index >= 0 then
                          begin
                            If Assigned(fOnPeerDisconnect) then fOnPeerDisconnect(Self,Connections[Index],Index);
                            DeleteConnection(Index);
                            Result := WMC_RESULT_ok;
                          end
                        else Result := WMC_RESULT_error;
                      end;
else
  Result := inherited ProcessMessage(SenderID,MessageCode,UserCode,Payload);
end;
end;

{==============================================================================}
{   TWinMsgCommPeer - Public methods                                           }
{==============================================================================}

constructor TWinMsgCommPeer.Create(Window: TUtilityWindow = nil; Synchronous: Boolean = False; const MessageName: String = WMC_MessageName);
begin
inherited Create(Window,Synchronous,MessageName);
InitIDArray;
SetID(AcquireID);
SendMessageTo(HWND_BROADCAST,BuildWParam(ID,WMC_QUERYPEERS,0),lParam(WindowHandle),True);
SendMessageToAll(BuildWParam(ID,WMC_PEERONLINE,0),lParam(WindowHandle),False);
end;

//------------------------------------------------------------------------------

destructor TWinMsgCommPeer.Destroy;
begin
SendMessageTo(HWND_BROADCAST,BuildWParam(ID,WMC_PEEROFFLINE,0),lParam(WindowHandle),False);
ReleaseID(ID);
FinalIDArray;
inherited;
end;

//------------------------------------------------------------------------------

Function TWinMsgCommPeer.SendMessage(MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam; RecipientID: TWMCConnectionID = WMC_SendToAll): lResult;
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
