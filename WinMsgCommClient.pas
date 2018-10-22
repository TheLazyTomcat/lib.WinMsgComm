{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Windows Messages communication library

  Client endpoint class

  ©František Milt 2018-10-22

  Version 1.4.2

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
unit WinMsgCommClient;

{$INCLUDE '.\WinMsgComm_defs.inc'}

interface

uses
  Windows, Classes, UtilityWindow, WinMsgComm;

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TWinMsgCommClient                               }
{------------------------------------------------------------------------------}
{==============================================================================}
type
  TWinMsgCommClient = class(TWinMsgCommBase)
  private
    fOnServerStatusChange:  TNotifyEvent;
    Function GetServerOnline: Boolean;
    Function GetServerWindow: HWND;
  protected
    Function ProcessMessage(SenderID: TWMCConnectionID; MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam): lResult; override;
  public
    constructor Create(Window: TUtilityWindow = nil; Synchronous: Boolean = False; const MessageName: String = WMC_MessageName); override;
    destructor Destroy; override;
    Function SendMessage(MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam; RecipientID: TWMCConnectionID = WMC_SendToAll): lResult; override;
    Function PingServer: Boolean;
    property ServerOnline: Boolean read GetServerOnline;
    property ServerWindow: HWND read GetServerWindow;
    property OnServerStatusChange: TNotifyEvent read fOnServerStatusChange write fOnServerStatusChange;
  end;

implementation

uses
  SysUtils;

{$IFDEF FPC_DisableWarns}
  {$DEFINE FPCDWM}
  {$DEFINE W5024:={$WARN 5024 OFF}} // Parameter "$1" not used
{$ENDIF}

{==============================================================================}
{------------------------------------------------------------------------------}
{                              TWinMsgCommClient                               }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TWinMsgCommClient - Private methods                                        }
{==============================================================================}

Function TWinMsgCommClient.GetServerOnline: Boolean;
begin
Result := ConnectionCount > 0;
end;

//------------------------------------------------------------------------------

Function TWinMsgCommClient.GetServerWindow: HWND;
begin
If ConnectionCount > 0 then
  Result := Connections[0].WindowHandle
else
  Result := INVALID_HANDLE_VALUE;
end;

{==============================================================================}
{   TWinMsgCommClient - Protected methods                                      }
{==============================================================================}

Function TWinMsgCommClient.ProcessMessage(SenderID: TWMCConnectionID; MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam): lResult;
var
  Server:     TWMCConnectionInfo;
  AssignedID: TWMCConnectionID;
begin
case MessageCode of
  WMC_QUERYSERVER:    Result := WMC_RESULT_error;
  WMC_SERVERONLINE:   If not ServerOnline then
                        begin
                          Server.Valid := True;
                          Server.ConnectionID := 0;
                          Server.WindowHandle := HWND(Payload);
                          Server.Transacting := False;
                          AssignedID := TWMCConnectionID(SendMessageTo(HWND(Payload),BuildWParam(ID,WMC_CLIENTONLINE,0),lParam(WindowHandle),True));
                          If AssignedID > 0 then
                            begin
                              SetID(AssignedID);
                              AddConnection(Server);
                              If Assigned(fOnServerStatusChange) then fOnServerStatusChange(Self);
                              Result := WMC_RESULT_ok;
                            end
                          else Result := WMC_RESULT_error;
                        end
                      else Result := WMC_RESULT_error;
  WMC_SERVEROFFLINE:  begin
                        while ConnectionCount > 0 do DeleteConnection(0);
                        SetID(0);
                        If Assigned(fOnServerStatusChange) then fOnServerStatusChange(Self);
                        Result := WMC_RESULT_ok;
                      end;
  WMC_CLIENTONLINE:   Result := WMC_RESULT_error;
  WMC_CLIENTOFFLINE:  Result := WMC_RESULT_error;
  WMC_ISSERVER:       Result := WMC_RESULT_error;
  WMC_QUERYPEERS:     Result := WMC_RESULT_error;
  WMC_PEERONLINE:     Result := WMC_RESULT_error;
  WMC_PEEROFFLINE:    Result := WMC_RESULT_error;
else
  Result := inherited ProcessMessage(SenderID,MessageCode,UserCode,Payload);
end;
end;

{==============================================================================}
{   TWinMsgCommClient - Public methods                                         }
{==============================================================================}

constructor TWinMsgCommClient.Create(Window: TUtilityWindow; Synchronous: Boolean; const MessageName: String);
begin
inherited Create(Window,Synchronous,MessageName);
SendMessageTo(HWND_BROADCAST,BuildWParam(ID,WMC_QUERYSERVER,0),lParam(WindowHandle),False);
end;

//------------------------------------------------------------------------------

destructor TWinMsgCommClient.Destroy;
begin
If ServerOnline then
  SendMessageTo(ServerWindow,BuildWParam(ID,WMC_CLIENTOFFLINE,0),lParam(WindowHandle),False);
inherited;
end;

//------------------------------------------------------------------------------

{$IFDEF FPCDWM}{$PUSH}W5024{$ENDIF}
Function TWinMsgCommClient.SendMessage(MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: lParam; RecipientID: TWMCConnectionID = WMC_SendToAll): lResult;
begin
If ServerOnline then
  Result := SendMessageTo(ServerWindow,BuildWParam(ID,MessageCode,UserCode),Payload)
else
  Result := WMC_RESULT_error;
end;
{$IFDEF FPCDWM}{$POP}{$ENDIF}

//------------------------------------------------------------------------------

Function TWinMsgCommClient.PingServer: Boolean;
begin
ClearInvalidConnections;
Result := ServerOnline;
end;

end.
