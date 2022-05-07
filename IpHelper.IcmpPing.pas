// Type: Windows network ICMP ping.
// Author: 2022 Wei-Lun Huang
// Description: ping.
//
// Features:
//   1. TPing
//      1. First TPing.Create
//      2. Settings (options): TimeoutMS, IcmpCreate, CreateRequest.
//      3. Call SendEcho to send a ping, then CheckSucceed,
//         and ICMP information returned by EchoReply.
//
// Tested in Delphi 10 Seattle.
//
// Last modified date: May 7, 2022.

unit IpHelper.IcmpPing;

interface

uses
  Winapi.Windows, Winapi.Winsock2, Winapi.IpExport, Winapi.IpHlpApi,
  System.SysUtils,
  IpHelper.Addresses;

{$IFDEF WIN32}
{$ALIGN 4}
{$ELSE}
{$ALIGN ON}
{$ENDIF}
{$MINENUMSIZE 4}

type
  PICMPV6_ECHO_REPLY = ^ICMPV6_ECHO_REPLY;
  ICMPV6_ECHO_REPLY = record
    Address: IPV6_ADDRESS_EX; // IPV6_ADDRESS_EX Address;
    Status: ULONG;            // ULONG           Status;
    RoundTripTime: ULONG;     // unsigned int    RoundTripTime;
  end;
  TIcmpV6EchoReply = ICMPV6_ECHO_REPLY;
  PIcmpV6EchoReply = ^TIcmpV6EchoReply;

  _IO_STATUS_BLOCK = record
    Status: NTSTATUS;
    Information: ULONG_PTR;
  end;
  IO_STATUS_BLOCK = _IO_STATUS_BLOCK;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;
  TIoStatusBlock = IO_STATUS_BLOCK;
  PIoStatusBlock = ^TIoStatusBlock;

//(*PIO_APC_ROUTINE) (
//   IN PVOID ApcContext,
//   IN PIO_STATUS_BLOCK IoStatusBlock,
//   IN ULONG Reserved
//   );

function IcmpCreateFile: THandle; stdcall;
function IcmpCloseHandle(icmpHandle: THandle): Boolean; stdcall;
function IcmpSendEcho(icmpHandle: THandle; DestinationAddress: TInAddr;
  RequestData: Pointer; RequestSize: Smallint; RequestOptions: PIP_OPTION_INFORMATION;
  ReplyBuffer: Pointer; ReplySize: DWORD; Timeout: DWORD): DWORD; stdcall;

function IcmpSendEcho2Ex(
  icmpHandle        : THandle;  // [in]           HANDLE                 IcmpHandle,
  Event             : THandle;  // [in, optional] HANDLE                 Event,
  ApcRoutine        : Pointer;  // [in, optional] PIO_APC_ROUTINE        ApcRoutine,
  ApcContext        : Pointer;  // [in, optional] PVOID                  ApcContext,
  const SourceAddress     : TInAddr;  // [in]           IPAddr                 SourceAddress,
  const DestinationAddress: TInAddr;  // [in]           IPAddr                 DestinationAddress,
  RequestData       : Pointer;  // [in]           LPVOID                 RequestData,
  RequestSize       : WORD;     // [in]           WORD                   RequestSize,
  RequestOptions: PIP_OPTION_INFORMATION; // [in, optional] PIP_OPTION_INFORMATION RequestOptions,
  ReplyBuffer       : Pointer;  // [out]          LPVOID                 ReplyBuffer,
  ReplySize         : DWORD;    // [in]           DWORD                  ReplySize,
  Timeout           : DWORD     // [in]           DWORD                  Timeout
  ): DWORD; stdcall;

function Icmp6CreateFile(): THandle; stdcall;

function Icmp6SendEcho2(
  IcmpHandle        : THandle;// [in]           HANDLE                 IcmpHandle,
  Event             : THandle;// [in, optional] HANDLE                 Event,
  ApcRoutine        : Pointer;// [in, optional] PIO_APC_ROUTINE        ApcRoutine,
  ApcContext        : Pointer;// [in, optional] PVOID                  ApcContext,
  const SourceAddress     : sockaddr_in6;// [in]           sockaddr_in6           *SourceAddress,
  const DestinationAddress: sockaddr_in6;// [in]           sockaddr_in6           *DestinationAddress,
  RequestData       : Pointer;// [in]           LPVOID                 RequestData,
  RequestSize       : WORD;   // [in]           WORD                   RequestSize,
  RequestOptions: PIP_OPTION_INFORMATION;// [in, optional] PIP_OPTION_INFORMATION RequestOptions,
  ReplyBuffer       : Pointer;// [out]          LPVOID                 ReplyBuffer,
  ReplySize         : DWORD;  // [in]           DWORD                  ReplySize,
  Timeout           : DWORD   // [in]           DWORD                  Timeout
): DWORD; stdcall;

type
  PIcmpEchoReplyEx = ^TIcmpEchoReplyEx;
  TIcmpEchoReplyEx = record
    case Integer of
      0: (v4: TIcmpEchoReply);   // IP v4
      1: (v6: TIcmpV6EchoReply); // IP v6
  end;

  TPingEchoReply = record
    Family: Smallint;
    Address: TAddrBytes;  // Replying address
    Status: ULONG;        // Reply IP_STATUS
    RoundTripTime: ULONG; // RTT in milliseconds
  end;

  TCreatRequestMode = set of (
    _CRM_NumberChar, // 0-9
    _CRM_LowerCase,  // a-z
    _CRM_UpperCase,  // A-Z
    _CRM_Random);

const
  CRM_Standard         = []; // a-w
  CRM_Order_NLU        = [_CRM_NumberChar, _CRM_LowerCase, _CRM_UpperCase];
  CRM_Order_Number     = [_CRM_NumberChar];
  CRM_Order_LowerCase  = [_CRM_LowerCase];
  CRM_Order_UpperCase  = [_CRM_UpperCase];
  CRM_Random_NLU       = [_CRM_NumberChar, _CRM_LowerCase, _CRM_UpperCase];
  CRM_Random_Number    = [_CRM_NumberChar, _CRM_Random];
  CRM_Random_LowerCase = [_CRM_LowerCase, _CRM_Random];
  CRM_Random_UpperCase = [_CRM_UpperCase, _CRM_Random];

  _EchoReplyV4Size = SizeOf(TIcmpEchoReply);   // ICMP_ECHO_REPLY
  _EchoReplyV6Size = SizeOf(TIcmpV6EchoReply); // ICMPV6_ECHO_REPLY
  _IoStatusBlockSize = SizeOf(TIoStatusBlock); // IO_STATUS_BLOCK
  _ReplyV4BufferSize = _EchoReplyV4Size + _IoStatusBlockSize;
  _ReplyV6BufferSize = _EchoReplyV6Size + _IoStatusBlockSize;

  _DefaultTimeoutMS = 500; // 預設 ICMP ping API 的逾時時間

type
  TPing = class(TObject)
  private const
    _WSA_Version = $0202; // Winsock DLL 初始化的目標版本(例：2.2 = $0202)
    _DefaultTimeout = _DefaultTimeoutMS; // 預設逾時時間
  private
    FError: DWORD;       // SendEcho API的錯誤碼
    FTimeoutMS: DWORD;       // 最長等待時間 MilliSecond(ms)
    FTimes: Cardinal;        // SendEcho 的執行次數
    FFails: Cardinal;        // SendEcho 的失敗次數
    FLost: Cardinal;         // SendEcho 回應的錯誤次數
    FTimeouts: Cardinal;     // SendEcho 的超時次數
    FEchoCumulant: Cardinal; // RoundTripTime 的累計時間加總(ms)
    FEchoMin: Word;          // RoundTripTime 最小值(ms)
    FEchoMax: Word;          // RoundTripTime 最大值(ms)
    FhIcmp: THandle;         // ICMP echo 通訊控制
    FFamily: Smallint;       // 位址類型
    FWSAData: TWSAData;      // Winsock DLL 資訊
    FOptions: TIpOptionInformation; // 選項參數
    FSource: TSockAddr;             // 來源位址
    FDestination: TSockAddr;        // 目標位址
    FRequestBuffer: array of Byte;  // 傳送緩衝區
    FReplyBuffer: array of Byte;    // 接收緩衝區

    procedure WSABegin; inline; // 初始 Winsock
    procedure WSAEnd; inline;   // 結束 Winsock
    procedure Initial; inline;  // 初始化本物件的變數(僅用於 Create)

    function TryGetEchoReplyPtr: PIcmpEchoReplyEx;

    function GetLossRate: Single;                       // 取得遺失率
    function GetAverage: Single; inline;                // RoundTripTime(RTT) 的平均值(ms)
    function CalcReplySize(RequestSize: DWORD): DWORD;  // 計算回應緩衝區的所需大小
    function GetRequestSize: DWORD; inline;             // 取得目前發送緩衝區的大小
    function GetReplySize: DWORD; inline;               // 取得目前接收緩衝區的大小
    function GetEchoReplyPtr: PIcmpEchoReplyEx; inline; // 取得目前接收緩衝區位址指標
    function GetEchoReplyV4: PIcmpEchoReply;            // for AF_INET
    function GetEchoReplyV6: PIcmpV6EchoReply;          // for AF_INET6
    function GetStatus: ULONG;                          // Get Reply IP_STATUS
    function GetRoundTripTime: ULONG;                   // Get RTT(ms)
    function GetErrorMessage: string;                   // 依照系統地區語言取得錯誤訊息

    procedure AddStatisticsRTT(RTT: ULONG); inline; // 以輸入的 RTT 更新 RTT 統計狀態
  public
    constructor Create; overload;
    constructor Create(const ASource, ADestination: TSockAddr; RequestSize: DWORD; ATimeoutMS: DWORD); overload;
    constructor Create(const ASource, ADestination: TSockAddr; ATimeoutMS: DWORD); overload;
    constructor Create(const ASource, ADestination: TSockAddr); overload;
    destructor Destroy; override;

    // 直接回傳回應時間，-1(小於0)表示失敗
    class function RunSendEcho(const ASource, ADestination: TSockAddr; RequestSize: DWORD; ATimeoutMS: DWORD): Integer; overload; static;
    class function RunSendEcho(const ASource, ADestination: TSockAddr; ATimeoutMS: DWORD = _DefaultTimeout): Integer; overload; static;

    // SendEcho 緩衝區 (SendEcho 使用前的必要前置作業)
    procedure InitialReply;
    procedure CreatRequest(Size: WORD; Mode: TCreatRequestMode = CRM_Standard);
    procedure SetRequest(const Buffer; Size: WORD);      // 複製資料至傳送緩衝區
    function GetRequest(var Buffer; Size: DWORD): DWORD; // 取得傳送的緩衝區資料
    function GetReply(var Buffer; Size: DWORD): DWORD;   // 取得接收的緩衝區資料
    function GetEchoReply(var EchoReply: TPingEchoReply): Boolean; // 僅取得回應的狀態類資訊

    //
    // ICMP 通訊 (SendEcho 使用前的必要前置作業)
    //
    procedure ResetState; inline;          // 歸零統計
    function IcmpCreated: Boolean; inline; // 檢查 ICMP echo 通訊是否已建立
    // 建立 ICMP 通訊，設定並檢查 來源與目的 網路位址
    procedure IcmpCreate(const ASource, ADestination: TSockAddr);
    // 關閉 ICMP 通訊
    procedure IcmpClose;  inline;

    // 發送封包
    function SendEcho: DWORD; overload;
    function SendEcho(ATimeoutMS: Word): DWORD; overload;
    function SendEcho(const ASource, ADestination: TSockAddr): DWORD; overload;
    function SendEcho(const ASource, ADestination: TSockAddr; ATimeoutMS: DWORD): DWORD; overload;

    // 基礎參數
    property TimeoutMS: DWORD read FTimeoutMS write FTimeoutMS; // API 的逾時時間
    property Family: Smallint read FFamily;                     // IP 類型
    property WSAData: TWSAData read FWSAData;                   // Winsock 資訊
    property Options: TIpOptionInformation read FOptions write FOptions;
    property Source: TSockAddr read FSource;
    property Destination: TSockAddr read FDestination;

    // 檢查狀態 (SendEcho 執行後才是有效值)
    function CheckReply: Boolean;
    function CheckSucceed: Boolean;

    property Error: DWORD read FError write FError;
    property Status: ULONG read GetStatus;               // Reply IP_STATUS
    property RoundTripTime: ULONG read GetRoundTripTime; // RTT in milliseconds
    property ErrorMessage: string read GetErrorMessage;  // 錯誤訊息，成功則無字串輸出

    // 狀態統計
    property Times: Cardinal read FTimes;              // 執行 Ping 的次數
    property Fails: Cardinal read FFails;              // 失敗數 (API 錯誤)
    property Lost: Cardinal read FLost;                // 遺失數 (回應狀態不成功)
    property Timeouts: Cardinal read FTimeouts;        // 成功時的逾時次數(以TimeoutMS為依據)
    property LossRate: Single read GetLossRate;        // 遺失率 (回應狀態不成功)
    property RttCumulant: Cardinal read FEchoCumulant; // 回應時間加總
    property RttAverage: Single read GetAverage;       // 回應時間平均值
    property RttMin: Word read FEchoMin;               // 回應時間最小值
    property RttMax: Word read FEchoMax;               // 回應時間最大值

    // 緩衝區大小
    property RequestSize: DWORD read GetRequestSize;
    property ReplySize: DWORD read GetReplySize;

    // 取得接收緩衝區的指標
    property EchoReply: PIcmpEchoReplyEx read GetEchoReplyPtr;  // 通用
    property EchoReplyV4: PIcmpEchoReply read GetEchoReplyV4;   // 只支援 IPv4
    property EchoReplyV6: PIcmpV6EchoReply read GetEchoReplyV6; // 只支援 IPv6
  end;

implementation

//uses
//  HighAccuracyGauge, Debug;

//
// ICMP echo requests - Windows API
//

const
  iphlpapi = 'iphlpapi.dll';

function IcmpCreateFile; external iphlpapi name 'IcmpCreateFile';
function IcmpCloseHandle; external iphlpapi name 'IcmpCloseHandle';
function IcmpSendEcho; external iphlpapi name 'IcmpSendEcho';
function IcmpSendEcho2Ex; external iphlpapi name 'IcmpSendEcho2Ex';
function Icmp6CreateFile; external iphlpapi name 'Icmp6CreateFile';
function Icmp6SendEcho2; external iphlpapi name 'Icmp6SendEcho2';

// 取得 狀態碼(型態IP_STATUS) 相對應的訊息
function GetReplyStatusString(Status: ULONG): string;
var
  rCodr: DWORD;
  Len: DWORD;
begin
  Len := 0;
  repeat
    rCodr := GetIpErrorString(Status, PChar(Result), Len);
    SetLength(Result, Len);
  until (rCodr <> ERROR_INSUFFICIENT_BUFFER);
end;

{ TPing }

resourcestring
  errDiffeFamily = 'The source(%s) and destination(%s) IP families are not the same.';
  errPingFamily = 'The %s address family(%s) cannot be used for Ping.';
  errOutpuType = 'The output %s is different from the %s currently acting in the container.';
  errUnsupportedFamily = 'Unsupported Family %s.';
  errRequestMode = 'When the Mode of CreatRequest is not standard, need to specify types of the char sets.';
  errEchoReply = 'Echo reply buffer is not valid.';

constructor TPing.Create;
begin
  inherited;
  Initial;
end;

constructor TPing.Create(const ASource, ADestination: TSockAddr; RequestSize: DWORD; ATimeoutMS: DWORD);
begin
  inherited Create;
  Initial;
  FTimeoutMS := ATimeoutMS;
  CreatRequest(RequestSize);
  IcmpCreate(ASource, ADestination);
end;

constructor TPing.Create(const ASource, ADestination: TSockAddr; ATimeoutMS: DWORD);
begin
  inherited Create;
  Initial;
  FTimeoutMS := ATimeoutMS;
  CreatRequest(32);
  IcmpCreate(ASource, ADestination);
end;

constructor TPing.Create(const ASource, ADestination: TSockAddr);
begin
  inherited Create;
  Initial;
  CreatRequest(32);
  IcmpCreate(ASource, ADestination);
end;

destructor TPing.Destroy;
begin
  IcmpClose;
  WSAEnd;
  inherited;
end;

class function TPing.RunSendEcho(const ASource, ADestination: TSockAddr;
  RequestSize: DWORD; ATimeoutMS: DWORD): Integer;
var
  Ping: TPing;
begin
  Ping := TPing.Create(ASource, ADestination, RequestSize, ATimeoutMS);
  try
    try
      if Ping.SendEcho <> 0 then
        Result := Ping.GetRoundTripTime
      else
        Result := -1;
    except
      Result := -1;
    end;
  finally
    Ping.Free;
  end;
end;

class function TPing.RunSendEcho(const ASource, ADestination: TSockAddr;
  ATimeoutMS: DWORD): Integer;
var
  Ping: TPing;
begin
  Ping := TPing.Create(ASource, ADestination, ATimeoutMS);
  try
    try
      if Ping.SendEcho <> 0 then
        Result := Ping.GetRoundTripTime
      else
        Result := -1;
    except
      Result := -1;
    end;
  finally
    Ping.Free;
  end;
end;

procedure TPing.WSABegin;
var
  rCode: Integer;
begin
  rCode := WSAStartup(_WSA_Version, FWSAData);
  if rCode = 0 then
    Exit;
  rCode := WSAGetLastError;
  raise Exception.Create('WSAStartup: '+ SysErrorMessage(rCode));
end;

procedure TPing.WSAEnd;
var
  rCode: Integer;
begin
  rCode := WSACleanup();
  if rCode = 0 then // if successful.
  begin
    FillChar(FWSAData, SizeOf(FWSAData), 0);
    Exit;
  end;
  // if failed.
  rCode := WSAGetLastError;
  raise Exception.Create('WSACleanup: '+ SysErrorMessage(rCode));
end;

procedure TPing.Initial;
begin
  FError := 0;
  FTimeoutMS := _DefaultTimeout;
  FhIcmp := INVALID_HANDLE_VALUE;
  FFamily := AF_UNSPEC;

  // Object memory blocks are initially filled with 0 when creating,
  // so no need to be cleared to 0 again.
  FOptions.Ttl := 64;

  WSABegin;
end;    

function TPing.TryGetEchoReplyPtr: PIcmpEchoReplyEx;
begin
  if FError <> 0 then
    raise Exception.Create(GetErrorMessage);

  case FFamily of
    AF_INET, AF_INET6: ;
    else raise Exception.CreateFmt(errUnsupportedFamily, [GetFamilyStr(FFamily, True)]);
  end;

  Result := PIcmpEchoReplyEx(PByte(FReplyBuffer));

  if not Assigned(Result) then
    raise Exception.Create(errEchoReply);
end;

function TPing.GetLossRate: Single;
begin
  if FLost = 0 then
    Result := 0
  else
    Result := FLost / (FTimes - FFails) * 100;
end;

function TPing.GetAverage: Single;
var
  Successes: Cardinal;
begin
  Successes := FTimes - FFails;
  if Successes = 0 then
    Result := 0
  else
    Result := FEchoCumulant / Successes;
end;

function TPing.CalcReplySize(RequestSize: DWORD): DWORD;
begin
// The allocated size, in bytes, of the reply buffer. The buffer should be large
// enough to hold at least one ICMP_ECHO_REPLY structure plus RequestSize bytes of data.
// This buffer should also be large enough to also hold 8 more bytes of data
// (the size of an ICMP error message) plus space for an IO_STATUS_BLOCK structure.
  case FFamily of
    AF_INET : Result := RequestSize + _ReplyV4BufferSize;
    AF_INET6: Result := RequestSize + _ReplyV6BufferSize;
    else raise Exception.CreateFmt(errUnsupportedFamily, [GetFamilyStr(FFamily, True)]);
  end;
end;

function TPing.GetRequestSize: DWORD;
begin
  Result := Length(FRequestBuffer);
end;

function TPing.GetReplySize: DWORD;
begin
  Result := Length(FReplyBuffer);
end;

function TPing.GetEchoReplyPtr: PIcmpEchoReplyEx;
begin
  Result := PIcmpEchoReplyEx(PByte(FReplyBuffer));
end;

function TPing.GetEchoReplyV4: PIcmpEchoReply;
begin
  if FFamily = AF_INET then
    Result := PIcmpEchoReply(GetEchoReplyPtr)
  else
    raise Exception.CreateFmt(errOutpuType, [GetFamilyStr(AF_INET, True), GetFamilyStr(FFamily, True)]);
end;

function TPing.GetEchoReplyV6: PIcmpV6EchoReply;
begin
  if FFamily = AF_INET6 then
    Result := PIcmpV6EchoReply(GetEchoReplyPtr)
  else
    raise Exception.CreateFmt(errOutpuType, [GetFamilyStr(AF_INET6, True), GetFamilyStr(FFamily, True)]);
end;

function TPing.GetStatus: ULONG;
var
  IcmpEcho: PIcmpEchoReplyEx;
begin
  IcmpEcho := TryGetEchoReplyPtr;
  case FFamily of
    AF_INET : Result := IcmpEcho.v4.Status;
    AF_INET6: Result := IcmpEcho.v6.Status;
  else      Result := 0;
  end;
end;

function TPing.GetRoundTripTime: ULONG;
var
  IcmpEcho: PIcmpEchoReplyEx;
begin
  IcmpEcho := TryGetEchoReplyPtr;
  case FFamily of
    AF_INET : Result := IcmpEcho.v4.RoundTripTime;
    AF_INET6: Result := IcmpEcho.v6.RoundTripTime;
    else      Result := 0;
  end;
end;     

function TPing.GetErrorMessage: string;
var
  IcmpEcho: PIcmpEchoReplyEx;
  Code: ULONG;
begin
  IcmpEcho := GetEchoReplyPtr;
  case FFamily of
  AF_INET : Code := IcmpEcho.v4.Status;
  AF_INET6: Code := IcmpEcho.v6.Status;
  else      Exit(Format(errUnsupportedFamily, [FFamily]));
  end;

  if Code <> IP_SUCCESS then
    Exit('ICMP: [' + Code.ToString + ']' + GetReplyStatusString(Code));
  if FError <> NO_ERROR then
    Exit('API: [' + FError.ToString + ']' + SysErrorMessage(FError));

  Result := '';
end;

procedure TPing.AddStatisticsRTT(RTT: ULONG);
begin
  if RTT > FTimeoutMS then
    Inc(FTimeouts);
  Inc(FEchoCumulant, RTT);
  if RTT < FEchoMin then
    FEchoMin := RTT;
  if RTT > FEchoMax then
    FEchoMax := RTT;
end;

procedure TPing.InitialReply;
var
  ReplySize: DWORD;
begin
  ReplySize := CalcReplySize(GetRequestSize);
  if DWORD(Length(FReplyBuffer)) <> ReplySize then
    SetLength(FReplyBuffer, ReplySize);
end;

procedure TPing.CreatRequest(Size: WORD; Mode: TCreatRequestMode);
var
  I, Chars: Integer;
  p: PAnsiChar;
  function OffsetIndex(Index: Integer; Mode: TCreatRequestMode): Integer; inline;
  begin
    Result := Index;
    if not (_CRM_NumberChar in Mode) then Inc(Result, 10); // Skip index of 0-9.
    if not (_CRM_LowerCase  in Mode) then Inc(Result, 26); // Skip index of a-z.
    if not (_CRM_UpperCase  in Mode) then Inc(Result, 26); // Skip index of A-Z.
  end;
  function GetChar(Index: Integer): AnsiChar; inline;
  begin
    case Index of
       0.. 9: Result := AnsiChar(Index+(48-0));  // 0-9
      10..35: Result := AnsiChar(Index+(97-10)); // a-z
      36..61: Result := AnsiChar(Index+(65-36)); // A-Z
      else    Result := #0;
    end;
  end;
begin
  SetLength(FRequestBuffer, Size);
  p := PAnsiChar(PByte(FRequestBuffer));
  if Mode = CRM_Standard then
  begin
    for I := 0 to Size - 1 do
      p[I] := AnsiChar(Ord('a')+(I mod (Ord('w')-Ord('a'))));
  end
  else
  begin
    Chars := 0;
    if _CRM_NumberChar in Mode then Inc(Chars, 10); // [0-9] 10 chars.
    if _CRM_LowerCase  in Mode then Inc(Chars, 26); // [a-z] 26 chars.
    if _CRM_UpperCase  in Mode then Inc(Chars, 26); // [A-Z] 26 chars.
    if Chars = 0 then
      raise Exception.Create(errRequestMode);

    if _CRM_Random in Mode then
      for I := 0 to Size - 1 do
        p[I] := GetChar(Random(Chars))
    else
      for I := 0 to Size - 1 do
        p[I] := GetChar(OffsetIndex(I mod Chars, Mode));
  end;

  InitialReply;
end;

procedure TPing.SetRequest(const Buffer; Size: WORD);
begin
  SetLength(FRequestBuffer, Size);
  if Size > 0 then
    Move(Buffer, PByte(FRequestBuffer)^, Size);

  InitialReply;
end;

function TPing.GetRequest(var Buffer; Size: DWORD): DWORD;
begin
  Result := GetRequestSize;
  if Size >= Result then
    Move(PByte(FRequestBuffer)^, Buffer, Result);
end;

function TPing.GetReply(var Buffer; Size: DWORD): DWORD;
begin
  Result := GetReplySize;
  if Size >= Result then
    Move(PByte(FReplyBuffer)^, Buffer, Result);
end;         

function TPing.GetEchoReply(var EchoReply: TPingEchoReply): Boolean;
var
  IcmpEcho: PIcmpEchoReplyEx;
begin
  Result := False;
  if (FError <> 0) and (Status = IP_SUCCESS) then
    Exit;

  IcmpEcho := GetEchoReplyPtr;
  if not Assigned(IcmpEcho) then
    Exit;

  EchoReply.Family := FFamily;
  FillChar(EchoReply.Address, SizeOf(EchoReply.Address), 0);
  case FFamily of
  AF_INET: // for IPv4
    begin
      Move(IcmpEcho.v4.Address, EchoReply.Address, SizeOf(IcmpEcho.v4.Address));
      EchoReply.Status := IcmpEcho.v4.Status;
      EchoReply.RoundTripTime := IcmpEcho.v4.RoundTripTime;
    end;
  AF_INET6: // for IPv6
    begin
      Move(IcmpEcho.v6.Address, EchoReply.Address, SizeOf(IcmpEcho.v6.Address));
      EchoReply.Status := IcmpEcho.v6.Status;
      EchoReply.RoundTripTime := IcmpEcho.v6.RoundTripTime;
    end;
  else raise Exception.CreateFmt(errUnsupportedFamily, [GetFamilyStr(FFamily, True)]);
  end;
  Result := True;
end;

procedure TPing.ResetState;
begin
  FTimes    := 0;
  FLost     := 0;
  FFails    := 0;
  FTimeouts := 0;
  FEchoCumulant := 0;
  FEchoMin := FEchoMin.MaxValue;
  FEchoMax := 0;
end;

function TPing.IcmpCreated: Boolean;
begin
  Result := FhIcmp <> INVALID_HANDLE_VALUE;
end;

procedure TPing.IcmpCreate(const ASource, ADestination: TSockAddr);
var
  SrcFamily, DestFamily: Smallint;
begin
  ResetState;

  SrcFamily := ASource.family;
  DestFamily := ADestination.family;

  // Check family of source are AF_INET or AF_INET6, raise exception if not.
  case SrcFamily of
    AF_INET, AF_INET6: ;
    else raise Exception.CreateFmt(
               errPingFamily, ['source', GetFamilyStr(SrcFamily, True)]);
  end;

  // Check family of destination are AF_INET or AF_INET6, raise exception if not.
  case DestFamily of
    AF_INET, AF_INET6: ;
    else raise Exception.CreateFmt(
               errPingFamily, ['destination', GetFamilyStr(DestFamily, True)]);
  end;

  // Check family of source and destination are the same, raise exception if different.
  if SrcFamily <> DestFamily  then
    raise Exception.CreateFmt(
      errDiffeFamily, [GetFamilyStr(SrcFamily, True), GetFamilyStr(DestFamily, True)]);

  // If the family is different from the existing ICMP handle, close the handle.
  if IcmpCreated then
    if FFamily <> SrcFamily then
      IcmpClose;

  // If ICMP handle does not exist, create the ICMP handle by AF_INET or AF_INET6.
  if not IcmpCreated then
    case SrcFamily of
      AF_INET : if not IcmpCreated then FhIcmp := IcmpCreateFile;
      AF_INET6: if not IcmpCreated then FhIcmp := Icmp6CreateFile;
    end;

  // Raises an exception if the ICMP handle is invalid.
  if FhIcmp = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  // Overwrite self variable value.
  FFamily := SrcFamily;
  FSource := ASource;
  FDestination := ADestination;
end;

procedure TPing.IcmpClose;
begin
  if IcmpCreated then
    if not IcmpCloseHandle(FhIcmp) then
      RaiseLastOSError;

  FhIcmp := INVALID_HANDLE_VALUE;
  FillChar(FSource, SizeOf(FSource), 0);
  FillChar(FDestination, SizeOf(FDestination), 0);
end;

function TPing.SendEcho: DWORD;
var
  pReply: PIcmpEchoReplyEx;
  Code: ULONG;
  RTT: ULONG;
begin
  case FFamily of
    AF_INET : if Length(FReplyBuffer) < _ReplyV4BufferSize then InitialReply;
    AF_INET6: if Length(FReplyBuffer) < _ReplyV6BufferSize then InitialReply;
  end;
  
  FillChar(PByte(FReplyBuffer)^, Length(FReplyBuffer), 0);

  case FFamily of
    AF_INET: // for IPv4
    begin
      Result := IcmpSendEcho2Ex(FhIcmp, 0, nil, nil,
                FSource.v4.sin_addr, FDestination.v4.sin_addr,
                PByte(FRequestBuffer), Length(FRequestBuffer), @FOptions,
                PByte(FReplyBuffer), Length(FReplyBuffer), FTimeoutMS);
      pReply := GetEchoReplyPtr;
      Code := pReply.v4.Status;
      RTT  := pReply.v4.RoundTripTime;
    end;
    AF_INET6: // for IPv6
    begin
      Result := Icmp6SendEcho2(FhIcmp, 0, nil, nil, FSource.v6, FDestination.v6,
                PByte(FRequestBuffer), Length(FRequestBuffer), @FOptions,
                PByte(FReplyBuffer), Length(FReplyBuffer), FTimeoutMS);
      pReply := GetEchoReplyPtr;
      Code := pReply.v6.Status;
      RTT  := pReply.v6.RoundTripTime;
    end
    else raise Exception.CreateFmt(
               errUnsupportedFamily, [GetFamilyStr(FFamily, True)]);
  end;

  if Result = 0 then
    FError := GetLastError
  else
    FError := 0;

  Inc(FTimes);
  case Code of
    IP_SUCCESS:
      if FError = ERROR_SUCCESS then
        AddStatisticsRTT(RTT)
      else
        Inc(FFails);
    else
      Inc(FLost)
  end;
end;

function TPing.SendEcho(ATimeoutMS: Word): DWORD;
begin
  FTimeoutMS := ATimeoutMS;
  Result := SendEcho;
end;

function TPing.SendEcho(const ASource, ADestination: TSockAddr): DWORD;
begin
  IcmpCreate(ASource, ADestination);
  Result := SendEcho;
end;

function TPing.SendEcho(const ASource, ADestination: TSockAddr; ATimeoutMS: DWORD): DWORD;
begin
  FTimeoutMS := ATimeoutMS;
  IcmpCreate(ASource, ADestination);
  Result := SendEcho;
end;

function TPing.CheckReply: Boolean;
var
  Size: Integer;
  p: PByte;
begin
  p := PByte(FReplyBuffer);
  case FFamily of
    AF_INET : Inc(p, _EchoReplyV4Size);
    AF_INET6: Inc(p, _EchoReplyV6Size);
    else Exit(False);
  end;
  Size := Length(FRequestBuffer);
  Result := CompareMem(PByte(FRequestBuffer), p, Size);
end;     

function TPing.CheckSucceed: Boolean;
begin  
  Result := GetStatus = IP_SUCCESS;
  if Result then      
    Result := FError = NO_ERROR;
end;

initialization

finalization

end.
