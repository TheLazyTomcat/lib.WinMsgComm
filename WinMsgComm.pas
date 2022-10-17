unit WinMsgComm;

{$IF Defined(CPU64) or Defined(CPU64BITS)}
  {$DEFINE CPU64bit}
{$ELSEIF Defined(CPU16)}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$ELSE}
  {$DEFINE CPU32bit}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {.$MODESWITCH DuplicateLocals+}
  {.$DEFINE FPC_DisableWarns}
  {.$MACRO ON}
{$ENDIF}
{$H+}

//------------------------------------------------------------------------------

{.$DEFINE ConserveMemory}

{$DEFINE UseWindowsMessages}

//------------------------------------------------------------------------------

// do not touch following...
{$IFNDEF Windows}
  {$UNDEF UseWindowsMessages}
{$ENDIF}

interface

uses
  {$IFDEF UseWindowsMessages}Windows, Messages,{$ENDIF} SysUtils,
  AuxTypes, AuxClasses, BitVector, CRC32, SharedMemoryStream,
  {$IFDEF UseWindowsMessages}UtilityWindow{$ELSE}SimpleMessages{$ENDIF};

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  EWMCException = class(Exception);

  EWMCIndexOutOfBounds  = class(EWMCException);
  EWMCOutOfResources    = class(EWMCException);
  EWMCInvalidConnection = class(EWMCException);

{===============================================================================
--------------------------------------------------------------------------------
                                   TWinMsgComm
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    TWinMsgComm - common types
===============================================================================}
type
  TWMCConnectionID = UInt16;
  TWMCMessageCode  = UInt8;
  TWMCUserCode     = Int8;

{===============================================================================
    TWinMsgComm - internal types
===============================================================================}
type
  TWMCIDPool = packed record
    Flags:  UInt32;
    Pool:   array[0..Pred($10000 div 8){8191}] of UInt8;
  end;
  PWMCIDPool = ^TWMCIDPool;

{$IFNDEF ConserveMemory}
  TWMCIDToIndexTable = array[TWMCConnectionID] of Integer;
{$ENDIF}  

  TWMCDataSize     = UInt32;
  TWMCDataPosition = UInt32;
  TWMCDataChecksum = TCRC32;

  TWMCTransaction = record
    DataPtr:  Pointer;
    DataSize: TWMCDataSize;
    Position: TWMCDataPosition;
    CheckSum: TWMCDataChecksum;
  end;

  TWMCSystemID = HWND;

  TWMCConnectionData = record
    ConnectionID: TWMCConnectionID;
    Is32bit:      Boolean;
    SystemID:     TWMCSystemID;
    Transacting:  Boolean;
    Transaction:  TWMCTransaction;
  end;

  TWMCMessagePayload = UInt64;
  TWMCMessageResult  = Int32;

{===============================================================================
    TWinMsgComm - public types
===============================================================================}
type
  TWMCConnection = record
    ConnectionID: TWMCConnectionID;
    Transacting:  Boolean;
  end;

  TWMCValueType = (mvtBool,mvtUInt8,mvtInt8,mvtUInt16,mvtInt16,mvtUInt32,
                   mvtInt32,mvtUInt64,mvtInt64,mvtSingle,mvtDouble,mvtString,
                   mvtData);

  TWMCValue = record
    UserCode:    TWMCUserCode;
    StringValue: String;
    case ValueType: TWMCValueType of
      mvtBool:    (BoolValue:     ByteBool);
      mvtUInt8:   (UInt8Value:    UInt8);
      mvtInt8:    (Int8Value:     Int8);
      mvtUInt16:  (UInt16Value:   UInt16);
      mvtInt16:   (Int16Value:    Int16);
      mvtUInt32:  (UInt32Value:   UInt32);
      mvtInt32:   (Int32Value:    Int32);
      mvtUInt64:  (UInt64Value:   UInt64);
      mvtInt64:   (Int64Value:    Int64);
      mvtSingle:  (SingleValue:   Single);
      mvtDouble:  (DoubleValue:   Double);
      mvtData:    (DataSize:      TWMCDataSize;
                   DataPtr:       Pointer)
  end;

  TWMCValueEvent = procedure(Sender: TObject; Value: TWMCValue) of object;
  TWMCValueCallback = procedure(Sender: TObject; Value: TWMCValue);

const
  WMC_CLIENT_ALL = TWMCConnectionID($FFFF);

{===============================================================================
    TWinMsgComm - class declaration
===============================================================================}
type
  TWinMsgComm = class(TCustomListObject)
  protected
    fLargeDataThreshold:      TMemSize;
    fDomainName:              String;
    fIDPoolSharedMemory:      TSharedMemory;
    fIDPoolVector:            TBitVector;
    fConnectionID:            TWMCConnectionID;
    fConnections:             array of TWMCConnectionData;
    fConnectionCount:         Integer;
  {$IFNDEF ConserveMemory}
    fIDToIndexTable:          TWMCIDToIndexTable;
  {$ENDIF}
  {$IFDEF UseWindowsMessages}
    fWindowsMessageID:        UINT;
    fOwnsRecevingWindow:      Boolean;
    fReceivingWindow:         TUtilityWindow;
  {$ELSE}
    fMessagesClient:          TSimpleMessagesClient;
  {$ENDIF}
    fOnIncomingValueCallback: TWMCValueCallback;
    fOnIncomingValueEvent:    TWMCValueEvent;
    fOnConnectionCallback:    TNotifyCallback;
    fOnConnectionEvent:       TNotifyEvent;
    // getters, setters
    Function GetConnection(Index: Integer): TWMCConnection; virtual;
    // list methods
    Function GetCapacity: Integer; override;
    procedure SetCapacity(Value: Integer); override;
    Function GetCount: Integer; override;
    procedure SetCount(Value: Integer); override;
    Function ConnectionAdd(ConnectionID: TWMCConnectionID; Is32bit: Boolean; SystemID: TWMCSystemID): Integer; virtual;
    Function ConnectionRemove(ConnectionID: TWMCConnectionID): Integer; virtual;
    // internals
    Function TranslateIDToIndex(ConnectionID: TWMCConnectionID): Integer; virtual;
    // message processing
  {$IFDEF UseWindowsMessages}
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean; Sent: Boolean); virtual; abstract;
    //Function HandleData(): TWMCMessageResult; virtual; abstract;
  {$ELSE}
  {$ENDIF}
    Function ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: TWMCMessagePayload): TWMCMEssageResult; virtual;
    // message sending
    
    // events firing
    procedure DoIncomingValue(Value: TWMCValue); virtual;
    procedure DoConnectionChange; virtual;
    // init/final
    procedure Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF}); virtual;
    procedure Finalize; virtual;
  public
    class Function MaxDataSize: TMemSize; virtual;
    constructor Create(const DomainName: String = ''{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow = nil{$ENDIF});
    destructor Destroy; override;
    Function LowIndex: Integer; override;
    Function HighIndex: Integer; override;
    Function ConnectionIndexOf(ConnectionID: TWMCConnectionID): Integer; virtual;
    //procedure ConnectionsCheck: Boolean; virtual;

    //Send...
    //Post...
    //procedure Update; virtual;

    property LargeDataThreshold: TMemSize read fLargeDataThreshold write fLargeDataThreshold;
    property DomainName: String read fDomainName;
    property ConnectionID: TWMCConnectionID read fConnectionID;
    property Connections[Index: Integer]: TWMCConnection read GetConnection; default;
    property OnIncomingValueCallback: TWMCValueCallback read fOnIncomingValueCallback write fOnIncomingValueCallback;
    property OnIncomingValueEvent: TWMCValueEvent read fOnIncomingValueEvent write fOnIncomingValueEvent;
    property OnIncomingValue: TWMCValueEvent read fOnIncomingValueEvent write fOnIncomingValueEvent;
    property OnConnectionChangeCallback: TNotifyCallback read fOnConnectionCallback write fOnConnectionCallback;
    property OnConnectionChangeEvent: TNotifyEvent read fOnConnectionEvent write fOnConnectionEvent;
    property OnConnectionChange: TNotifyEvent read fOnConnectionEvent write fOnConnectionEvent;
  end;

implementation

uses
  StrRect;

{===============================================================================
--------------------------------------------------------------------------------
                                   TWinMsgComm
--------------------------------------------------------------------------------
===============================================================================}
const
  WMC_NAMEPREFIX_IDPOOL = 'wmc_idpool_';
  WMC_NAMEPREFIX_WINMSG = 'wmc_winmsg_';

  WMC_FLAG_INITIALIZED = UInt32($00000001);
  WMC_FLAG_WINDOWSMSGS = UInt32($00000002);

  WMC_USERDATA_FLAG_32BIT = Int8($01);

  WMC_MSGRES_ERR = TWMCMessageResult(0);
  WMC_MSGRES_OK  = TWMCMessageResult(1);

{===============================================================================
    TWinMsgComm - class implementation
===============================================================================}
{-------------------------------------------------------------------------------
    TWinMsgComm - protected methods
-------------------------------------------------------------------------------}

Function TWinMsgComm.GetConnection(Index: Integer): TWMCConnection;
begin
If CheckIndex(Index) then
  begin
    Result.ConnectionID := fConnections[Index].ConnectionID;
    Result.Transacting := fConnections[Index].Transacting;
  end
else raise EWMCIndexOutOfBounds.CreateFmt('TWinMsgComm.GetConnection: Index (%d) out of bounds.',[Index]);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.GetCapacity: Integer;
begin
Result := Length(fConnections);
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.SetCapacity(Value: Integer);
begin
SetLength(fConnections,Value);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.GetCount: Integer;
begin
Result := fConnectionCount;
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.SetCount(Value: Integer);
begin
// do nothing
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.ConnectionAdd(ConnectionID: TWMCConnectionID; Is32bit: Boolean; SystemID: TWMCSystemID): Integer;
begin
If ConnectionID <> WMC_CLIENT_ALL then
  begin
    Result := ConnectionIndexOf(ConnectionID);
    If not CheckIndex(Result) then
      begin
        Grow;
        Result := fConnectionCount;
        fConnections[Result].ConnectionID := ConnectionID;
        fConnections[Result].Is32bit := Is32Bit;
        fConnections[Result].SystemID := SystemID;
        fConnections[Result].Transacting := False;
        fConnections[Result].Transaction.DataPtr := nil;        
        fConnections[Result].Transaction.DataSize := 0;
        fConnections[Result].Transaction.Position := 0;
        fConnections[Result].Transaction.CheckSum := ZeroCRC32;
        Inc(fConnectionCount);
        fIDToIndexTable[ConnectionID] := Result;
        DoConnectionChange;
      end;
  end
else raise EWMCInvalidConnection.CreateFmt('TWinMsgComm.ConnectionAdd: Invalid connection ID (%u)',[ConnectionID]);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.ConnectionRemove(ConnectionID: TWMCConnectionID): Integer;
var
  i:  Integer;
begin
If ConnectionID <> WMC_CLIENT_ALL then
  begin
    Result := ConnectionIndexOf(ConnectionID);
    If CheckIndex(Result) then
      begin
        If fConnections[Result].Transacting then
          with fConnections[Result].Transaction do
          FreeMem(DataPtr,DataSize);
        fIDToIndexTable[ConnectionID] := -1;
        For i := Result to Pred(HighIndex) do
          begin
            fConnections[i] := fConnections[i + 1];
            fIDToIndexTable[Connections[i].ConnectionID] := i;
          end;
        Dec(fConnectionCount);
        Shrink;
        DoConnectionChange;
      end;
  end
else raise EWMCInvalidConnection.CreateFmt('TWinMsgComm.ConnectionRemove: Invalid connection ID (%u)',[ConnectionID]);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.TranslateIDToIndex(ConnectionID: TWMCConnectionID): Integer;
begin
{$IFDEF ConserveMemory}
// slower, smaller memory usage
Result := ConnectionIndexOf(ConnectionID);
{$ELSE}
// faster, larger memory usage
Result := fIDToIndexTable[ConnectionID];
{$ENDIF}
end;

//------------------------------------------------------------------------------

{$IFDEF UseWindowsMessages}
(*
procedure TWinMsgComm.HandleMessage(var Msg: TMessage; var Handled: Boolean; Sent: Boolean);
begin
If Msg.Msg = fWindowsMessageID then
  begin

    Handled := True;
  end
else If Msg.Msg = WM_COPYDATA then
  begin
  
    Handled := True;
  end
else Handled := False;
end;
(*
begin
Handled := False;
If Assigned(fOnMessage) then fOnMessage(Msg,Handled);
If not Handled then
  If Msg.Msg = fMessageID then
    begin
      Msg.Result := ProcessMessage(GetConnectionID(Msg.WParam),GetMessageCode(Msg.WParam),GetUserCode(Msg.WParam),Msg.LParam);
      Handled := True;
    end
  else If Msg.Msg = WM_COPYDATA then
    begin
      Process_WM_COPYDATA(Msg);
      Msg.Result := lResult(WMC_RESULT_ok);
      Handled := True;
    end;
end;
*)
{$ELSE}

{$ENDIF}

//------------------------------------------------------------------------------

Function TWinMsgComm.ProcessMessage(Sender: TWMCConnectionID; MessageCode: TWMCMessageCode; UserCode: TWMCUserCode; Payload: TWMCMessagePayload): TWMCMEssageResult;
begin
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.DoIncomingValue(Value: TWMCValue);
begin
If Assigned(fOnIncomingValueEvent) then
  fOnIncomingValueEvent(Self,Value)
else If Assigned(fOnIncomingValueCallback) then
  fOnIncomingValueCallback(Self,Value);
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.DoConnectionChange;
begin
If Assigned(fOnConnectionEvent) then
  fOnConnectionEvent(Self)
else If Assigned(fOnConnectionCallback) then
  fOnConnectionCallback(Self);
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.Initialize(const DomainName: String{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow{$ENDIF});
var
  i:  Integer;
begin
fLargeDataThreshold := 128{bytes};
fDomainName := DomainName;
fIDPoolSharedMemory := TSharedMemory.Create(SizeOf(TWMCIDPool),WMC_NAMEPREFIX_IDPOOL + fDomainName);
fIDPoolSharedMemory.Lock;
try
  // set or check flags
  If PWMCIDPool(fIDPoolSharedMemory.Memory)^.Flags and WMC_FLAG_INITIALIZED = 0 then
    PWMCIDPool(fIDPoolSharedMemory.Memory)^.Flags := WMC_FLAG_INITIALIZED {$IFDEF UseWindowsMessages}or WMC_FLAG_WINDOWSMSGS{$ENDIF}
  else If PWMCIDPool(fIDPoolSharedMemory.Memory)^.Flags and WMC_FLAG_WINDOWSMSGS {$IFDEF UseWindowsMessages}={$ELSE}<>{$ENDIF} 0 then
     raise EWMCInvalidConnection.Create('TWinMsgComm.Initialize: Invalid connection type.');
  // get first free ID
  fIDPoolVector := TBitVector.Create(Addr(PWMCIDPool(fIDPoolSharedMemory.Memory)^.Pool),$10000);
  fIDPoolVector[Integer(WMC_CLIENT_ALL)] := True;
  i := fIDPoolVector.FirstClean;
  If (i >= 0) and (i < $FFFF) then
    begin
      fIDPoolVector[i] := True;
      fConnectionID := TWMCConnectionID(i);
    end
  else raise EWMCOutOfResources.Create('TWinMsgComm.Initialize: No free connection ID.');
finally
  fIDPoolSharedMemory.Unlock;
end;
// initilaize connections
SetLength(fConnections,0);
fConnectionCount := 0;
{$IFNDEF ConserveMemory}
// connection ID to index table
For i := Low(fIDToIndexTable) to High(fIDToIndexTable) do
  fIDToIndexTable[i] := -1;
{$ENDIF}
{$IFDEF UseWindowsMessages}
fWindowsMessageID := RegisterWindowMessageW(PWideChar(StrToWinW(WMC_NAMEPREFIX_WINMSG + fDomainName)));
If Assigned(ReceivingWindow) then
  begin
    fOwnsRecevingWindow := False;
    fReceivingWindow := ReceivingWindow;
  end
else
  begin
    fOwnsRecevingWindow := True;
    fReceivingWindow := TUtilityWindow.Create
  end;
fReceivingWindow.OnMessage.Add(HandleMessage);
{$ENDIF}
// init events
fOnIncomingValueCallback := nil;
fOnIncomingValueEvent := nil;
fOnConnectionCallback := nil;
fOnConnectionEvent := nil;
end;

//------------------------------------------------------------------------------

procedure TWinMsgComm.Finalize;
var
  i:  Integer;
begin
// disable events
fOnIncomingValueCallback := nil;
fOnIncomingValueEvent := nil;
fOnConnectionCallback := nil;
fOnConnectionEvent := nil;
{$IFDEF UseWindowsMessages}
fReceivingWindow.OnMessage.Remove(HandleMessage);
If fOwnsRecevingWindow then
  fReceivingWindow.Free;
{$ENDIF}
// remove self
If Assigned(fIDPoolSharedMemory) then
  begin
    fIDPoolSharedMemory.Lock;
    try
      If Assigned(fIDPoolVector) then
        fIDPoolVector[Integer(fConnectionID)] := False;      
    finally
      fIDPoolSharedMemory.Unlock;
    end;
  end;
fIDPoolVector.Free;
fIDPoolSharedMemory.Free;
// clear all connections, if there is transaction in progress, end it and free memory
For i := LowIndex to HighIndex do
  If fConnections[i].Transacting then
    with fConnections[i].Transaction do
      FreeMem(DataPtr,DataSize);
SetLength(fConnections,0);
fConnectionCount := 0;
end;

{-------------------------------------------------------------------------------
    TWinMsgComm - public methods
-------------------------------------------------------------------------------}

class Function TWinMsgComm.MaxDataSize: TMemSize;
begin
Result := 512 * 1024 * 1024;  // 512MiB
end;

//------------------------------------------------------------------------------

constructor TWinMsgComm.Create(const DomainName: String = ''{$IFDEF UseWindowsMessages}; ReceivingWindow: TUtilityWindow = nil{$ENDIF});
begin
inherited Create;
Initialize(DomainName{$IFDEF UseWindowsMessages},ReceivingWindow{$ENDIF});
end;

//------------------------------------------------------------------------------

destructor TWinMsgComm.Destroy;
begin
Finalize;
inherited;
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.LowIndex: Integer;
begin
Result := Low(fConnections);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.HighIndex: Integer;
begin
Result := Pred(fConnectionCount);
end;

//------------------------------------------------------------------------------

Function TWinMsgComm.ConnectionIndexOf(ConnectionID: TWMCConnectionID): Integer;
var
  i:  Integer;
begin
Result := -1;
For i := LowIndex to HighIndex do
  If fConnections[i].ConnectionID = ConnectionID then
    begin
      Result := i;
      Break{For i};
    end;
end;

end.

