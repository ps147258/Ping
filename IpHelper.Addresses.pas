// Type: Windows network API addresses.
// Author: 2022 Wei-Lun Huang
// Description: Addresses structure and some function for the Windows API structure.
//
// Features:
//   1. TAddrInfoList:
//      A list similar to the "addrinfo" structure, used to store addrinfo information,
//      so TAddrInfoList is easier to use in Delphi.
//
//   2. AddressToString: Convert Address to String.
//   3. StringToAddress: Convert String to Address.
//   4. GetHostAddress: Translation from a host name to an address.
//
//   5. CompareIpAddrFirst
//      Compare IP_ADDR_STRING information.
//
//   6. CompareIpAddrList
//      Compare multiple IP_ADDR_STRING structure information by the IP_ADDR_STRING pointer.
//
//   7. CompareAdapterInfoFirst
//      Compare first IP_ADAPTER_INFO structure information by the IP_ADAPTER_INFO.
//
//   8. CompareAdaptersAddressesFirst
//      Compare first IP_ADAPTER_ADDRESSES structure information by the IP_ADAPTER_ADDRESSES.
//
//   9. Add some structures, such as IPv6 structure.
//
// Tested in Delphi 10 Seattle.
//
// Last modified date: May 7, 2022.

unit IpHelper.Addresses;

interface

uses
  Winapi.Windows, Winapi.Winsock2, Winapi.IpTypes, Winapi.IpExport,
  System.SysUtils, System.Math, System.Generics.Collections;

{$IFDEF WIN32}
{$ALIGN 4}
{$ELSE}
{$ALIGN ON}
{$ENDIF}
{$MINENUMSIZE 4}

const
  AddressFamilyStrings: array[0..AF_MAX-1] of string = (
    'AF_UNSPEC',       //  0 unspecified
    'AF_UNIX',         //  1 local to host (pipes, portals
    'AF_INET',         //  2 internetwork: UDP, TCP, etc.
    'AF_IMPLINK',      //  3 arpanet imp addresses
    'AF_PUP',          //  4 pup protocols: e.g. BSP
    'AF_CHAOS',        //  5 mit CHAOS protocols
    'AF_NS or AF_IPX', //  6 XEROX NS protocols, or IPX protocols: IPX, SPX, etc.
    'AF_ISO',          //  7 ISO protocols
    'AF_ECMA',         //  8 european computer manufacturers
    'AF_DATAKIT',      //  9 datakit protocols
    'AF_CCITT',        // 10 CCITT protocols, X.25 etc
    'AF_SNA',          // 11 IBM SNA
    'AF_DECnet',       // 12 DECnet
    'AF_DLI',          // 13 Direct data link interface
    'AF_LAT',          // 14 LAT
    'AF_HYLINK',       // 15 NSC Hyperchannel
    'AF_APPLETALK',    // 16 AppleTalk
    'AF_NETBIOS',      // 17 NetBios-style addresses
    'AF_VOICEVIEW',    // 18 VoiceView
    'AF_FIREFOX',      // 19 Protocols from Firefox
    'AF_UNKNOWN1',     // 20 Somebody is using this!
    'AF_BAN',          // 21 Banyan
    'AF_ATM',          // 22 Native ATM Services
    'AF_INET6',        // 23 Internetwork Version 6
    'AF_CLUSTER',      // 24 Microsoft Wolfpack
    'AF_12844',        // 25 IEEE 1284.4 WG AF
    'AF_IRDA',         // 26 IrDA
    '',                // 27
    'AF_NETDES',       // 28 Network Designers OSI & gateway enabled protocols
    'AF_TCNPROCESS',   // 29
    'AF_TCNMESSAGE',   // 30
    'AF_ICLFXBM',      // 31
    'AF_BTH',          // 32 Bluetooth RFCOMM/L2CAP protocols
    'AF_LINK');        // 33

function GetFamilyStr(Family: Smallint; Detailed: Boolean = False): string;

type
  TIpAddrFlags = set of (_IAF_Next, _IAF_IpAddress, _IAF_IpMask, _IAF_Context);

  TAdapterInfoFlags = set of (
    _AIF_ComboIndex,
    _AIF_AdapterName,
    _AIF_Description,
    _AIF_AddressLength,
    _AIF_Address,
    _AIF_Index,
    _AIF_Type,
    _AIF_DhcpEnabled,
    _AIF_CurrentIpAddress,
    _AIF_IpAddressList,
    _AIF_GatewayList,
    _AIF_DhcpServer,
    _AIF_HaveWins,
    _AIF_PrimaryWinsServer,
    _AIF_SecondaryWinsServer,
    _AIF_LeaseObtained,
    _AIF_LeaseExpires
  );

//  Union: record
  TAdapterAddrFlags = set of (
    _AAF_AdapterName,
    _AAF_FirstUnicastAddress,
    _AAF_FirstAnycastAddress,
    _AAF_FirstMulticastAddress,
    _AAF_FirstDnsServerAddress,
    _AAF_DnsSuffix,
    _AAF_Description,
    _AAF_FriendlyName,
    _AAF_PhysicalAddress,
    _AAF_Flags,
    _AAF_Mtu,
    _AAF_IfType,
    _AAF_OperStatus,
    _AAF_Ipv6IfIndex,
    _AAF_ZoneIndices,
    _AAF_FirstPrefix,
    _AAF_TransmitLinkSpeed,
    _AAF_ReceiveLinkSpeed,
    _AAF_FirstWinsServerAddress,
    _AAF_FirstGatewayAddress,
    _AAF_Ipv4Metric,
    _AAF_Ipv6Metric,
    _AAF_Luid,
    _AAF_Dhcpv4Server,
    _AAF_CompartmentId,
    _AAF_NetworkGuid,
    _AAF_ConnectionType,
    _AAF_TunnelType,
    //
    // DHCP v6 Info.
    //
    _AAF_Dhcpv6Server,
    _AAF_Dhcpv6ClientDuid,
    _AAF_Dhcpv6Iaid,
    _AAF_FirstDnsSuffix
  );

  PAddrBytes = ^TAddrBytes;
  TAddrBytes = packed record
    case Integer of
      0: (v4: in_addr);
      1: (v6: IN6_ADDR);
  end;

  PIPV6_ADDRESS_EX = ^IPV6_ADDRESS_EX;
  IPV6_ADDRESS_EX = packed record
    sin6_port: USHORT;    // USHORT sin6_port;
    sin6_flowinfo: ULONG; // ULONG  sin6_flowinfo;
    sin6_addr: IN6_ADDR;  // USHORT sin6_addr[8];
    sin6_scope_id: ULONG; // ULONG  sin6_scope_id;
  end;
  TIPv6AddressEx = IPV6_ADDRESS_EX;
  PIPv6AddressEx = ^TIPv6AddressEx;

  sockaddr_in6 = packed record
    sin6_family   : Smallint; // AF_INET6
    sin6_port     : u_short;  // Transport level port number
    sin6_flowinfo : u_long;   // IPv6 flow information
    sin6_addr     : TIn6Addr; // IPv6 address
    sin6_scope_id : u_long;   // set of interfaces for a scope
  end;
  TSockaddrIn6 = sockaddr_in6;
  PSockaddrIn6 = ^TSockaddrIn6;

  PSockAddr = ^TSockAddr;
  TSockAddr = packed record
    case Integer of
      0: (family: Smallint;
          port: u_short;    ); // Transport level port number
      1: (base: sockaddr;);    // in Winapi.WinSock
      2: (v4: sockaddr_in;);   // in Winapi.Winsock2
      3: (v6: sockaddr_in6;);  // in this unit
  end;

  PSockAddrHelper = record helper for PSockAddr
  public
    class operator Implicit(Value: PSockAddrIn): PSockAddr; overload; inline;
    class operator Implicit(Value: PSockaddrIn6): PSockAddr; overload; inline;
    class operator Implicit(Value: PSockAddr): PSockAddrIn; overload; inline;
    class operator Implicit(Value: PSockAddr): PSockaddrIn6; overload; inline;
    class operator Explicit(Value: PSockAddrIn): PSockAddr; overload; inline;
    class operator Explicit(Value: PSockaddrIn6): PSockAddr; overload; inline;
    class operator Explicit(Value: PSockAddr): PSockAddrIn; overload; inline;
    class operator Explicit(Value: PSockAddr): PSockaddrIn6; overload; inline;
  end;

  PSocketAddress = ^TSocketAddress;
  TSocketAddress = record
    Address: PSockAddr;
    Length: Integer;
  end;

  PSocketAddressHelper = record helper for PSocketAddress
  public
    class operator Implicit(Value: Winapi.IpTypes.PSOCKET_ADDRESS): PSocketAddress; overload; inline;
    class operator Implicit(Value: Winapi.Winsock2.PSOCKET_ADDRESS): PSocketAddress; overload; inline;
    class operator Implicit(Value: PSocketAddress): Winapi.IpTypes.PSOCKET_ADDRESS; overload; inline;
    class operator Implicit(Value: PSocketAddress): Winapi.Winsock2.PSOCKET_ADDRESS; overload; inline;
    class operator Explicit(Value: Winapi.IpTypes.PSOCKET_ADDRESS): PSocketAddress; overload; inline;
    class operator Explicit(Value: Winapi.Winsock2.PSOCKET_ADDRESS): PSocketAddress; overload; inline;
    class operator Explicit(Value: PSocketAddress): Winapi.IpTypes.PSOCKET_ADDRESS; overload; inline;
    class operator Explicit(Value: PSocketAddress): Winapi.Winsock2.PSOCKET_ADDRESS; overload; inline;
  end;

  TSockAddrList = class(TList<TSockAddr>)
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Add(const ASockAddr: sockaddr): Integer; overload;
    function Add(const ASockAddr: sockaddr_in): Integer; overload;
    function Add(const ASockAddr: sockaddr_in6): Integer; overload;
    function Add(const ASockAddr: TSocketAddress): Integer; overload;
    function Add(const ASockAddr: Winapi.IpTypes.SOCKET_ADDRESS): Integer; overload;
  end;

  PAdapterAddress = ^TAdapterAddress;
  TAdapterAddress = packed record
    Data: Pointer;
    AdapterName: string;
    FriendlyName: string;
    Unicast: TSockAddr;
    Gateway: TSockAddr;
  end;

  TAdapterAddressList = class(TList<TAdapterAddress>)
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Find(const AdapterName: string; FromIndex: Integer = 0): Integer; overload;
    function Find(const AdapterName: string; const Unicast, Gateway: TSockAddr): Integer; overload;
    function FindByData(const AData: Pointer; FromIndex: Integer = 0): Integer;
    function Add(const AdapterName, FriendlyName: string; const Unicast, Gateway: TSockAddr): Integer; overload;
    function Add(const AdapterName, FriendlyName: string; const Unicast, Gateway: TSocketAddress): Integer; overload;
    function Add(const AdapterName, FriendlyName: string; const Unicast, Gateway: Winapi.IpTypes.SOCKET_ADDRESS): Integer; overload;
//    function Add(const Adapter: IP_ADAPTER_ADDRESSES): Integer; overload; inline;
  end;

  PAddrInfo = ^TAddrInfo;
  TAddrInfo = packed record
    flags: Integer;     // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST
    family: Integer;    // PF_xxx
    socktype: Integer;  // SOCK_xxx
    protocol: Integer;  // 0 or IPPROTO_xxx for IPv4 and IPv6
    canonname: string;  // Canonical name for nodename
    addr: TSockAddr;    // Binary address
  end;

//  {$IFDEF UNICODE}addrinfoW{$ELSE}addrinfo{$ENDIF}

  TAddrInfoList = class(TList<TAddrInfo>)
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Add(const Value: addrinfo): Integer; overload;
    function Add(const Value: addrinfoW): Integer; overload;
  end;

const
  _MaxIPv4String = 16;
  _MaxIPv6String = 46;

function AddressToString(const Address: Winapi.IpTypes.SOCKET_ADDRESS): string; overload;
function AddressToString(const Address: TSockAddr): string; overload;
function AddressToString(const Address: IPAddr): string; overload;
function AddressToString(const Address: Winapi.IpExport.in_addr): string; overload; inline;
function AddressToString(const Address: in6_addr): string; overload;
function AddressToString(const Address: IPV6_ADDRESS_EX): string; overload; inline;
function StringToAddress(const IP: string; var Address: Winapi.Winsock2.sockaddr): Boolean; overload;
function StringToAddress(const IP: string; var Address: Winapi.IpTypes.sockaddr): Boolean; overload; inline;
function StringToAddress(const IP: string; var Address: TSockAddr): Boolean; overload; inline;

function GetHostAddress(const Hints: TAddrInfoW; const HostName: string; const ServiceName: string = ''): TAddrInfoList; overload;
function GetHostAddress(const HostName: string; const ServiceName: string = ''): TAddrInfoList; overload;

// Anst 字串比對，包含大小寫差異
// 回傳：差異量，0 表示相同。
//function CompareWideStr(A, B: PWideChar): Integer; overload;
//function CompareWideStr(A, B: PWideChar; MaxCount: Integer): Integer; overload;
//function CompareAnsiStr(A, B: PAnsiChar): Integer; overload;
//function CompareAnstStr(A, B: PAnsiChar; MaxCount: Integer): Integer; overload;

// 比對 IP_ADDR_STRING 內容，但不包含 IP_ADDR_STRING.Next。
// 參數：Simplify 如果為 True 則在出現差異時立即跳出並回傳結果。
// 回傳：出現相異擇旗標會被設定。
function CompareIpAddrFirst(const A, B: IP_ADDR_STRING; Simplify: Boolean): TIpAddrFlags;

// 比對 IP_ADDR_STRING 內容，包含 IP_ADDR_STRING.Next 與其後續的所有項目。
function CompareIpAddrList(A, B: PIP_ADDR_STRING): Boolean;

// 比對 TIpAdapterInfo 內容，但不包含 TIpAdapterInfo.Next。
// 參數：Simplify 如果為 True 則在出現差異時立即跳出並回傳結果。
// 回傳：出現相異擇旗標會被設定。
function CompareAdapterInfoFirst(const A, B: IP_ADAPTER_INFO; Simplify: Boolean): TAdapterInfoFlags;

// 比對 TIpAdapterAddresses 內容，但不包含 TIpAdapterAddresses.Next。
// 參數：Simplify 如果為 True 則在出現差異時立即跳出並回傳結果。
// 回傳：出現相異擇旗標會被設定。
function CompareAdaptersAddressesFirst(const A, B: IP_ADAPTER_ADDRESSES; Simplify: Boolean): TAdapterAddrFlags;


implementation

resourcestring
  _ErrLengthOverflow = '%s length overflow, source %u, destination %u.';

function GetOffset(const Source, Target): Integer; inline;
begin
  Result := NativeInt(@Target) - NativeInt(@Source);
end;

function CompareWideStr(A, B: PWideChar): Integer; overload;
var
  CharA, CharB: WideChar;
begin
  if not Assigned(A) then
    if Assigned(B) then
      Exit(-1)
    else
      Exit(0)
  else
    if not Assigned(B) then
      Exit(1);

  CharA := A^;
  CharB := B^;
  Result := Integer(Ord(CharA)) - Ord(CharB);
  while (CharA <> #0) and (CharB <> #0) do
  begin
    Inc(A);
    Inc(B);
    CharA := A^;
    CharB := B^;
    Result := Integer(Ord(CharA)) - Ord(CharB);
    if Result <> 0 then Break;
  end;
end;

function CompareWideStr(A, B: PWideChar; MaxCount: Integer): Integer; overload;
var
  Count: Integer;
  CharA, CharB: WideChar;
begin
  if not Assigned(A) then
    if Assigned(B) then
      Exit(-1)
    else
      Exit(0)
  else
    if not Assigned(B) then
      Exit(1);

  Count := 0;
  repeat
    CharA := A^;
    CharB := B^;
    Result := Integer(Ord(CharA)) - Ord(CharB);
    if Result <> 0 then Break;
    if CharA  = #0 then Break;
    if CharB  = #0 then Break;

    Inc(A);
    Inc(B);
    Inc(Count);
  until (MaxCount = Count);
end;

function CompareAnsiStr(A, B: PAnsiChar): Integer; overload;
var
  CharA, CharB: AnsiChar;
begin
  if not Assigned(A) then
    if Assigned(B) then
      Exit(-1)
    else
      Exit(0)
  else
    if not Assigned(B) then
      Exit(1);

  CharA := A^;
  CharB := B^;
  Result := Integer(Ord(CharA)) - Ord(CharB);
  while (CharA <> #0) and (CharB <> #0) do
  begin
    Inc(A);
    Inc(B);
    CharA := A^;
    CharB := B^;
    Result := Integer(Ord(CharA)) - Ord(CharB);
    if Result <> 0 then Break;
  end;
end;

function CompareAnsiStr(A, B: PAnsiChar; MaxCount: Integer): Integer; overload;
var
  Count: Integer;
  CharA, CharB: AnsiChar;
begin
  if not Assigned(A) then
    if Assigned(B) then
      Exit(-1)
    else
      Exit(0)
  else
    if not Assigned(B) then
      Exit(1);

  Count := 0;
  repeat
    CharA := A^;
    CharB := B^;
    Result := Integer(Ord(CharA)) - Ord(CharB);
    if Result <> 0 then Break;
    if CharA  = #0 then Break;
    if CharB  = #0 then Break;

    Inc(A);
    Inc(B);
    Inc(Count);
  until (MaxCount = Count);
end;


var
  WSAStarted: Boolean = False;
  WSAData: TWSAData;
  WSAChars: Array of Char;

procedure Startup;
var
  rCode: Integer;
begin
  if not WSAStarted then
  begin
    rCode := WSAStartup($0202, WSAData);
    if rCode <> 0 then
      raise Exception.CreateFmt('WSAStartup exception code: %d', [WSAGetLastError]);
    WSAStarted := True;
  end;
end;

procedure Cleanup;
var
  rCode: Integer;
begin
  if WSAStarted then
  begin
    rCode := WSACleanup();
    if rCode <> 0 then
      raise Exception.CreateFmt('WSACleanup exception code: %d', [WSAGetLastError]);
    WSAStarted := False;
  end;
end;

function GetFamilyStr(Family: Smallint; Detailed: Boolean): string;
begin
  if (Family >= AF_UNSPEC) and (Family < AF_MAX) then
    if Detailed then
      Result := AddressFamilyStrings[Family] + '[' + Family.ToString + ']'
    else
      Result := AddressFamilyStrings[Family]
  else
    Result := '[' + Family.ToString + ']';
end;

function AddressToString(const Address: Winapi.IpTypes.SOCKET_ADDRESS): string;
var
  Error: Integer;
  Size: DWORD;
begin
  Startup;

  Size := Length(WSAChars);
  if Size < 8 then
    Size := 8;
  repeat
    if Size > DWORD(Length(WSAChars)) then
      SetLength(WSAChars, Size);
    FillChar(PChar(WSAChars)^, Length(WSAChars), 0);
    Error := WSAAddressToString(Winapi.Winsock2.PSockAddr(Address.lpSockaddr)^,
      Address.iSockaddrLength, nil, PChar(WSAChars), Size);
    if Error <> 0 then
      Error := WSAGetLastError;
  until (Error <> WSAEFAULT);
  if (Error <> 0) or (Size = 0) then
    Exit('');

  Result := WideCharLenToString(PChar(WSAChars), Size - 1);
end;

function AddressToString(const Address: TSockAddr): string;
var
  Error: Integer;
  Len: Integer;
  Size: DWORD;
begin
  case Address.base.sin_family of
    AF_INET : Len := SizeOf(sockaddr_in);
    AF_INET6: Len := SizeOf(sockaddr_in6);
    else Exit('');
  end;

  Startup;

  Size := Length(WSAChars);
  if Size < 8 then
    Size := 8;
  repeat
    if Size > DWORD(Length(WSAChars)) then
      SetLength(WSAChars, Size);
    FillChar(PChar(WSAChars)^, Length(WSAChars), 0);
    Error := WSAAddressToString(
      Winapi.Winsock2.PSockAddr(@Address.base)^, Len, nil, PChar(WSAChars), Size);
    if Error <> 0 then
      Error := WSAGetLastError;
  until (Error <> WSAEFAULT);
  if (Error <> 0) or (Size = 0) then
    Exit('');

  Result := WideCharLenToString(PChar(WSAChars), Size - 1);
end;

function AddressToString(const Address: IPAddr): string;
var
  SockAddr: TSockAddr;
begin
  FillChar(SockAddr, SizeOf(SockAddr), 0);
  SockAddr.v4.sin_family := AF_INET;
  SockAddr.v4.sin_addr := Winapi.Winsock2.in_addr(Address);
  Result := AddressToString(SockAddr);
end;

function AddressToString(const Address: Winapi.IpExport.in_addr): string;
begin
  Result := AddressToString(IPAddr(Address));
end;

function AddressToString(const Address: in6_addr): string;
var
  SockAddr: TSockAddr;
begin
  FillChar(SockAddr, SizeOf(SockAddr), 0);
  SockAddr.v6.sin6_family := AF_INET6;
  SockAddr.v6.sin6_addr := Address;
  Result := AddressToString(SockAddr);
end;

function AddressToString(const Address: IPV6_ADDRESS_EX): string;
begin
  Result := AddressToString(Address.sin6_addr);
end;

function StringToAddress(const IP: string; var Address: Winapi.Winsock2.sockaddr): Boolean;
var
  Error: Integer;
  Size: Integer;
begin
  Startup;

  Size := Length(WSAChars);
  if Size < 8 then
    Size := 8;
  repeat
    if Size > Length(WSAChars) then
      SetLength(WSAChars, Size);
    Error := WSAStringToAddress(PChar(IP), AF_INET, nil, Address, Size);
    if Error <> 0 then
      Error := WSAStringToAddress(PChar(IP), AF_INET6, nil, Address, Size);
    if Error <> 0 then
      Error := WSAGetLastError;
  until (Error <> WSAEFAULT);
  Result := (Error = 0) and (Size > 0);
end;

function StringToAddress(const IP: string; var Address: Winapi.IpTypes.sockaddr): Boolean;
begin
  Result := StringToAddress(IP, Winapi.Winsock2.sockaddr(Address));
end;

function StringToAddress(const IP: string; var Address: TSockAddr): Boolean; overload;
begin
  Result := StringToAddress(IP, Winapi.Winsock2.PSockAddr(@Address)^);
end;

function GetHostAddress(const Hints: TAddrInfoW; const HostName: string; const ServiceName: string = ''): TAddrInfoList;
var
  Res: PaddrinfoW;
  Code: Integer;
begin
  Result := nil;
  Res := nil;

  Startup;

  try
    Code := GetAddrInfoW(PChar(HostName), PChar(ServiceName), Hints, Res);
    if (Code <> 0) or not Assigned(Res) then
      Exit;

    Result := TAddrInfoList.Create;
    repeat
      Result.Add(Res^);
      Res := PaddrinfoW(Res.ai_next);
    until (not Assigned(Res));

  finally
    FreeAddrInfoW(Res^);
  end;
end;

function GetHostAddress(const HostName, ServiceName: string): TAddrInfoList;
var
  Hints: TAddrInfoW;
begin
  FillChar(Hints, SizeOf(Hints), 0);
  Hints.ai_family := AF_UNSPEC;
  Hints.ai_socktype := SOCK_STREAM;
  Hints.ai_protocol := IPPROTO_TCP;

  Result := GetHostAddress(Hints, HostName, ServiceName);
end;

function GetIPAddress: string;
type
  pu_long = ^u_long;
var
  WSAData: TWSAData;
  HostEnt: PHostEnt;
  InAddr: TInAddr;
  namebuf: array[0..255] of AnsiChar;
begin
  If WSAStartup($101, WSAData) <> 0 Then
    Result := ''
  else
  begin
    gethostname(namebuf, sizeof(namebuf));
    HostEnt := gethostbyname(namebuf);
    InAddr.S_addr := u_long(pu_long(HostEnt^.h_addr_list^)^);
    Result := string(inet_ntoa(InAddr));
  End;
  WSACleanup;
end;

//
// IP_ADAPTER_INFO
//

function CompareIpAddrFirst(const A, B: IP_ADDR_STRING; Simplify: Boolean): TIpAddrFlags;
begin
  Result := [];

//  Next: PIP_ADDR_STRING;
//  IpAddress: IP_ADDRESS_STRING;
//  IpMask: IP_MASK_STRING;
//  Context: DWORD;

  if CompareAnsiStr(A.IpAddress.S, B.IpAddress.S, SizeOf(IP_ADDRESS_STRING)) <> 0 then
  begin
    Include(Result, _IAF_IpAddress);
    if Simplify then
      Exit;
  end;

  if CompareAnsiStr(A.IpMask.S, B.IpMask.S, SizeOf(IP_ADDRESS_STRING)) <> 0 then
  begin
    Include(Result, _IAF_IpMask);
    if Simplify then
      Exit;
  end;

  if A.Context <> B.Context then
    Include(Result, _IAF_Context);
end;

function CompareIpAddrList(A, B: PIP_ADDR_STRING): Boolean;
begin
  Result := False;

  while Assigned(A) and Assigned(A) do
  begin
    if CompareIpAddrFirst(A^, B^, True) <> [] then
      Exit;

    A := A.Next;
    B := B.Next;
  end;

  if Assigned(A) or Assigned(B) then
    Exit;

  Result := True;
end;

function CompareAdapterInfoFirst(const A, B: IP_ADAPTER_INFO; Simplify: Boolean): TAdapterInfoFlags;
begin
  Result := [];

//  AdapterName: array [0..MAX_ADAPTER_NAME_LENGTH + 3] of u_char;
  if CompareAnsiStr(A.AdapterName, B.AdapterName) <> 0 then
  begin
    Include(Result, _AIF_AdapterName);
    if Simplify then
      Exit;
  end;

//  ComboIndex: DWORD;
  if A.ComboIndex <> B.ComboIndex then
  begin
    Include(Result, _AIF_ComboIndex);
    if Simplify then
      Exit;
  end;

//  Description: array [0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of u_char;
  if CompareAnsiStr(A.Description,  B.Description) <> 0 then
  begin
    Include(Result, _AIF_Description);
    if Simplify then
      Exit;
  end;

//  AddressLength: UINT;
  if A.AddressLength <> B.AddressLength then
  begin
    Include(Result, _AIF_AddressLength);
    if Simplify then
      Exit;
  end;

//  Address: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
  if not CompareMem(@A.Address, @B.Address, MAX_ADAPTER_ADDRESS_LENGTH) then
  begin
    Include(Result, _AIF_Address);
    if Simplify then
      Exit;
  end;

//  Index: DWORD;
  if A.Index <> B.Index then
  begin
    Include(Result, _AIF_Index);
    if Simplify then
      Exit;
  end;

//  Type_: UINT;
  if A.Type_ <> B.Type_ then
  begin
    Include(Result, _AIF_Type);
    if Simplify then
      Exit;
  end;

//  DhcpEnabled: UINT;
  if A.DhcpEnabled <> B.DhcpEnabled then
  begin
    Include(Result, _AIF_DhcpEnabled);
    if Simplify then
      Exit;
  end;

//  CurrentIpAddress: PIP_ADDR_STRING;
  if not CompareIpAddrList(A.CurrentIpAddress, B.CurrentIpAddress)  then
  begin
    Include(Result, _AIF_CurrentIpAddress);
    if Simplify then
      Exit;
  end;

//  IpAddressList: IP_ADDR_STRING;
  if not CompareIpAddrList(@A.IpAddressList, @B.IpAddressList) then
  begin
    Include(Result, _AIF_IpAddressList);
    if Simplify then
      Exit;
  end;

//  GatewayList: IP_ADDR_STRING;
  if not CompareIpAddrList(@A.GatewayList, @B.GatewayList) then
  begin
    Include(Result, _AIF_GatewayList);
    if Simplify then
      Exit;
  end;

//  DhcpServer: IP_ADDR_STRING;
  if not CompareIpAddrList(@A.DhcpServer, @B.DhcpServer) then
  begin
    Include(Result, _AIF_DhcpServer);
    if Simplify then
      Exit;
  end;

//  HaveWins: BOOL;
  if A.HaveWins <> B.HaveWins then
  begin
    Include(Result, _AIF_HaveWins);
    if Simplify then
      Exit;
  end;

//  PrimaryWinsServer: IP_ADDR_STRING;
  if not CompareIpAddrList(@A.PrimaryWinsServer, @B.PrimaryWinsServer) then
  begin
    Include(Result, _AIF_PrimaryWinsServer);
    if Simplify then
      Exit;
  end;

//  SecondaryWinsServer: IP_ADDR_STRING;
  if not CompareIpAddrList(@A.SecondaryWinsServer, @B.SecondaryWinsServer) then
  begin
    Include(Result, _AIF_SecondaryWinsServer);
    if Simplify then
      Exit;
  end;

//  LeaseObtained: time_t;
  if A.LeaseObtained <> B.LeaseObtained then
  begin
    Include(Result, _AIF_LeaseObtained);
    if Simplify then
      Exit;
  end;

//  LeaseExpires: time_t;
  if A.LeaseExpires <> B.LeaseExpires then
    Include(Result, _AIF_LeaseExpires);

end;


//
// IP_ADAPTER_ADDRESSES
//

//function CompareSockaddrIn(const A, B: sockaddr): Boolean; inline;
//begin
//  Result := CompareMem(@A, @B, SizeOf(sockaddr));
//end;

function CompareSocketAddress(const A, B: Winapi.IpTypes.SOCKET_ADDRESS): Boolean; overload;
var
  Len: Integer;
begin
  Len := B.iSockaddrLength;
  if A.iSockaddrLength = Len then
    Exit(CompareMem(A.lpSockaddr, B.lpSockaddr, Min(Len, SizeOf(SOCKADDR))));
  Result := False;
end;

function CompareSocketAddress(const A, B: Winapi.Winsock2.SOCKET_ADDRESS): Boolean; overload;
begin
  Result := CompareSocketAddress(Winapi.IpTypes.SOCKET_ADDRESS(A), Winapi.IpTypes.SOCKET_ADDRESS(B));
end;

function CompareUnicastAddress(A, B: PIP_ADAPTER_UNICAST_ADDRESS): Boolean;
var
  Offset: Integer;
begin
//  Union: record
//    case Integer of
//      0: (
//        Alignment: ULONGLONG);
//      1: (
//        Length: ULONG;
//        Flags: DWORD);
//  end;
//  Next: PIP_ADAPTER_UNICAST_ADDRESS;
//  Address: SOCKET_ADDRESS;
//
//  PrefixOrigin: IP_PREFIX_ORIGIN;
//  SuffixOrigin: IP_SUFFIX_ORIGIN;
//  DadState: IP_DAD_STATE;
//  ValidLifetime: ULONG;
//  PreferredLifetime: ULONG;
//  LeaseLifetime: ULONG;
//  OnLinkPrefixLength: UCHAR;
  if not (Assigned(A) or Assigned(B)) then
    Exit(True);
  Result := False;
  Offset := GetOffset(A, A.Address);
  while Assigned(A) and Assigned(B) do
  begin
    if A.Union.Alignment <> B.Union.Alignment then
      Exit;
    if not CompareMem(A, B, SizeOf(IP_ADAPTER_UNICAST_ADDRESS) - Offset) then
      Exit;
    A := A.Next;
    B := B.Next;
  end;
  if Assigned(A) or Assigned(B) then
    Exit;
  Result := True;
end;

function CompareAnycastAddress(A, B: PIP_ADAPTER_ANYCAST_ADDRESS): Boolean;
begin
//  Union: record
//    case Integer of
//      0: (
//        Alignment: ULONGLONG);
//      1: (
//        Length: ULONG;
//        Flags: DWORD);
//  end;
//  Next: PIP_ADAPTER_ANYCAST_ADDRESS;
//  Address: SOCKET_ADDRESS;
  if not (Assigned(A) or Assigned(B)) then
    Exit(True);
  Result := False;
  while Assigned(A) and Assigned(B) do
  begin
    if A.Union.Alignment <> B.Union.Alignment then
      Exit;
    if not CompareMem(@A.Address, @B.Address, SizeOf(SOCKET_ADDRESS)) then
      Exit;
    A := A.Next;
    B := B.Next;
  end;
  if Assigned(A) or Assigned(B) then
    Exit;
  Result := True;
end;

function CompareMulticastAddress(A, B: PIP_ADAPTER_MULTICAST_ADDRESS): Boolean;
begin
//  Union: record
//    case Integer of
//      0: (
//        Alignment: ULONGLONG);
//      1: (
//        Length: ULONG;
//        Flags: DWORD);
//  end;
//  Next: PIP_ADAPTER_MULTICAST_ADDRESS;
//  Address: SOCKET_ADDRESS;
  if not (Assigned(A) or Assigned(B)) then
    Exit(True);
  Result := False;
  while Assigned(A) and Assigned(B) do
  begin
    if A.Union.Alignment <> B.Union.Alignment then
      Exit;
    if not CompareSocketAddress(A.Address, B.Address) then
      Exit;
    A := A.Next;
    B := B.Next;
  end;
  if Assigned(A) or Assigned(B) then
    Exit;
  Result := True;
end;

function CompareDnsServerAddress(A, B: PIP_ADAPTER_DNS_SERVER_ADDRESS): Boolean;
begin
//  Union: record
//    case Integer of
//      0: (
//        Alignment: ULONGLONG);
//      1: (
//        Length: ULONG;
//        Reserved: DWORD);
//  end;
//  Next: PIP_ADAPTER_DNS_SERVER_ADDRESS;
//  Address: SOCKET_ADDRESS;
  if not (Assigned(A) or Assigned(B)) then
    Exit(True);
  Result := False;
  while Assigned(A) and Assigned(B) do
  begin
    if A.Union.Alignment <> B.Union.Alignment then
      Exit;
    if not CompareSocketAddress(A.Address, B.Address) then
      Exit;
    A := A.Next;
    B := B.Next;
  end;
  if Assigned(A) or Assigned(B) then
    Exit;
  Result := True;
end;

function CompareWinsServerAddress(A, B: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH): Boolean;
begin
//  Union: record
//    case Integer of
//      0: (
//        Alignment: ULONGLONG);
//      1: (
//        Length: ULONG;
//        Reserved: DWORD);
//  end;
//  Next: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;
//  Address: SOCKET_ADDRESS;
  if not (Assigned(A) or Assigned(B)) then
    Exit(True);
  Result := False;
  while Assigned(A) and Assigned(B) do
  begin
    if A.Union.Alignment <> B.Union.Alignment then
      Exit;
    if not CompareSocketAddress(A.Address, B.Address) then
      Exit;
    A := A.Next;
    B := B.Next;
  end;
  if Assigned(A) or Assigned(B) then
    Exit;
  Result := True;
end;

function CompareGatewayAddress(A, B: PIP_ADAPTER_GATEWAY_ADDRESS_LH): Boolean;
begin
//  Union: record
//    case Integer of
//      0: (
//        Alignment: ULONGLONG);
//      1: (
//        Length: ULONG;
//        Reserved: DWORD);
//  end;
//  Next: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
//  Address: SOCKET_ADDRESS;
  if not (Assigned(A) or Assigned(B)) then
    Exit(True);
  Result := False;
  while Assigned(A) and Assigned(B) do
  begin
    if A.Union.Alignment <> B.Union.Alignment then
      Exit;
    if not CompareSocketAddress(A.Address, B.Address) then
      Exit;
    A := A.Next;
    B := B.Next;
  end;
  if Assigned(A) or Assigned(B) then
    Exit;
  Result := True;
end;

function CompareAdapterPrefix(A, B: PIP_ADAPTER_PREFIX): Boolean;
begin
//  Union: record
//  case Integer of
//    0: (
//      Alignment: ULONGLONG);
//    1: (
//      Length: ULONG;
//      Flags: DWORD);
//  end;
//  Next: PIP_ADAPTER_PREFIX;
//  Address: SOCKET_ADDRESS;
//  PrefixLength: ULONG;
  if not (Assigned(A) or Assigned(B)) then
    Exit(True);
  Result := False;
  while Assigned(A) and Assigned(B) do
  begin
    if A.Union.Alignment <> B.Union.Alignment then
      Exit;
    if A.PrefixLength <> B.PrefixLength then
      Exit;
    if not CompareSocketAddress(A.Address, B.Address) then
      Exit;
    A := A.Next;
    B := B.Next;
  end;
  if Assigned(A) or Assigned(B) then
    Exit;
  Result := True;
end;

function CompareAdapterDnsSuffix(A, B: PIP_ADAPTER_DNS_SUFFIX): Boolean;
begin
//  Next: PIP_ADAPTER_DNS_SUFFIX;
//  AString: array[0..MAX_DNS_SUFFIX_STRING_LENGTH - 1] of WCHAR;
  if not (Assigned(A) or Assigned(B)) then
    Exit(True);
  Result := False;
  while Assigned(A) and Assigned(B) do
  begin
    if CompareWideStr(A.AString, B.AString, MAX_DNS_SUFFIX_STRING_LENGTH) <> 0 then
      Exit;
    A := A.Next;
    B := B.Next;
  end;
  if Assigned(A) or Assigned(B) then
    Exit;
  Result := True;
end;

function CompareAdaptersAddressesFirst(const A, B: IP_ADAPTER_ADDRESSES; Simplify: Boolean): TAdapterAddrFlags;
var
  Len: DWORD;
begin
  Result := [];
//  Union: record
//    case Integer of
//      0: (
//        Alignment: ULONGLONG);
//      1: (
//        Length: ULONG;
//        IfIndex: DWORD);
//  end;
//  Next: PIP_ADAPTER_ADDRESSES;

//  AdapterName: MarshaledAString;
  if CompareAnsiStr(A.AdapterName,  B.AdapterName) <> 0 then
  begin
    Include(Result, _AAF_AdapterName);
    if Simplify then
      Exit;
  end;
//  FirstUnicastAddress: PIP_ADAPTER_UNICAST_ADDRESS;
  if not CompareUnicastAddress(A.FirstUnicastAddress,  B.FirstUnicastAddress)  then
  begin
    Include(Result, _AAF_AdapterName);
    if Simplify then
      Exit;
  end;
//  FirstAnycastAddress: PIP_ADAPTER_ANYCAST_ADDRESS;
  if not CompareAnycastAddress(A.FirstAnycastAddress,  B.FirstAnycastAddress) then
  begin
    Include(Result, _AAF_FirstAnycastAddress);
    if Simplify then
      Exit;
  end;
//  FirstMulticastAddress: PIP_ADAPTER_MULTICAST_ADDRESS;
  if not CompareMulticastAddress(A.FirstMulticastAddress,  B.FirstMulticastAddress) then
  begin
    Include(Result, _AAF_FirstMulticastAddress);
    if Simplify then
      Exit;
  end;
//  FirstDnsServerAddress: PIP_ADAPTER_DNS_SERVER_ADDRESS;
  if not CompareDnsServerAddress(A.FirstDnsServerAddress,  B.FirstDnsServerAddress) then
  begin
    Include(Result, _AAF_FirstDnsServerAddress);
    if Simplify then
      Exit;
  end;
//  DnsSuffix: PWCHAR;
  if CompareWideStr(A.DnsSuffix, B.DnsSuffix) <> 0 then
  begin
    Include(Result, _AAF_DnsSuffix);
    if Simplify then
      Exit;
  end;
//  Description: PWCHAR;
  if CompareWideStr(A.Description, B.Description) <> 0 then
  begin
    Include(Result, _AAF_Description);
    if Simplify then
      Exit;
  end;
//  FriendlyName: PWCHAR;
  if CompareWideStr(A.FriendlyName, B.FriendlyName) <> 0 then
  begin
    Include(Result, _AAF_FriendlyName);
    if Simplify then
      Exit;
  end;
//  PhysicalAddress: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
//  PhysicalAddressLength: DWORD;
  Len := B.PhysicalAddressLength;
  if (A.PhysicalAddressLength <> Len) or
     not CompareMem(@A.PhysicalAddress, @B.PhysicalAddress, Min(Len, MAX_ADAPTER_ADDRESS_LENGTH)) then
  begin
    Include(Result, _AAF_PhysicalAddress);
    if Simplify then
      Exit;
  end;
//  Flags: DWORD;
  if A.Flags <> B.Flags then
  begin
    Include(Result, _AAF_Flags);
    if Simplify then
      Exit;
  end;
//  Mtu: DWORD;
  if A.Mtu <> B.Mtu then
  begin
    Include(Result, _AAF_Mtu);
    if Simplify then
      Exit;
  end;
//  IfType: IFTYPE;
  if A.IfType <> B.IfType then
  begin
    Include(Result, _AAF_IfType);
    if Simplify then
      Exit;
  end;
//  OperStatus: IF_OPER_STATUS;
  if A.OperStatus <> B.OperStatus then
  begin
    Include(Result, _AAF_OperStatus);
    if Simplify then
      Exit;
  end;
//  Ipv6IfIndex: IF_INDEX;
  if A.Ipv6IfIndex <> B.Ipv6IfIndex then
  begin
    Include(Result, _AAF_Ipv6IfIndex);
    if Simplify then
      Exit;
  end;
//  ZoneIndices: array [0..15] of DWORD;
  if not CompareMem(@A.ZoneIndices, @B.ZoneIndices, SizeOf(A.ZoneIndices)) then
  begin
    Include(Result, _AAF_ZoneIndices);
    if Simplify then
      Exit;
  end;
//  FirstPrefix: PIP_ADAPTER_PREFIX;
  if not CompareAdapterPrefix(A.FirstPrefix, B.FirstPrefix) then
  begin
    Include(Result, _AAF_FirstPrefix);
    if Simplify then
      Exit;
  end;
//  TransmitLinkSpeed: ULONG64;
  if A.TransmitLinkSpeed <> B.TransmitLinkSpeed then
  begin
    Include(Result, _AAF_TransmitLinkSpeed);
    if Simplify then
      Exit;
  end;
//  ReceiveLinkSpeed: ULONG64;
  if A.ReceiveLinkSpeed <> B.ReceiveLinkSpeed then
  begin
    Include(Result, _AAF_ReceiveLinkSpeed);
    if Simplify then
      Exit;
  end;
//  FirstWinsServerAddress: PIP_ADAPTER_WINS_SERVER_ADDRESS_LH;
  if not CompareWinsServerAddress(A.FirstWinsServerAddress, B.FirstWinsServerAddress) then
  begin
    Include(Result, _AAF_FirstWinsServerAddress);
    if Simplify then
      Exit;
  end;
//  FirstGatewayAddress: PIP_ADAPTER_GATEWAY_ADDRESS_LH;
  if not CompareGatewayAddress(A.FirstGatewayAddress, B.FirstGatewayAddress) then
  begin
    Include(Result, _AAF_FirstGatewayAddress);
    if Simplify then
      Exit;
  end;
//  Ipv4Metric: ULONG;
  if A.Ipv4Metric <> B.Ipv4Metric then
  begin
    Include(Result, _AAF_Ipv4Metric);
    if Simplify then
      Exit;
  end;
//  Ipv6Metric: ULONG;
  if A.Ipv6Metric <> B.Ipv6Metric then
  begin
    Include(Result, _AAF_Ipv6Metric);
    if Simplify then
      Exit;
  end;
//  Luid: IF_LUID;
  if A.Luid.Info <> B.Luid.Info then
  begin
    Include(Result, _AAF_Luid);
    if Simplify then
      Exit;
  end;
//  Dhcpv4Server: SOCKET_ADDRESS;
  if not CompareSocketAddress(A.Dhcpv4Server, B.Dhcpv4Server) then
  begin
    Include(Result, _AAF_Dhcpv4Server);
    if Simplify then
      Exit;
  end;
//  CompartmentId: NET_IF_COMPARTMENT_ID;
  if A.CompartmentId <> B.CompartmentId then
  begin
    Include(Result, _AAF_CompartmentId);
    if Simplify then
      Exit;
  end;
//  NetworkGuid: NET_IF_NETWORK_GUID;
  if A.NetworkGuid <> B.NetworkGuid then
  begin
    Include(Result, _AAF_NetworkGuid);
    if Simplify then
      Exit;
  end;
//  ConnectionType: NET_IF_CONNECTION_TYPE;
  if A.ConnectionType <> B.ConnectionType then
  begin
    Include(Result, _AAF_ConnectionType);
    if Simplify then
      Exit;
  end;
//  TunnelType: TUNNEL_TYPE;
  if A.TunnelType <> B.TunnelType then
  begin
    Include(Result, _AAF_TunnelType);
    if Simplify then
      Exit;
  end;
//  //
//  // DHCP v6 Info.
//  //
//  Dhcpv6Server: SOCKET_ADDRESS;
  if not CompareSocketAddress(A.Dhcpv6Server, B.Dhcpv6Server) then
  begin
    Include(Result, _AAF_Dhcpv6Server);
    if Simplify then
      Exit;
  end;
//  Dhcpv6ClientDuid: array [0..MAX_DHCPV6_DUID_LENGTH - 1] of Byte;
//  Dhcpv6ClientDuidLength: ULONG;
  Len := B.Dhcpv6ClientDuidLength;
  if (A.Dhcpv6ClientDuidLength <> B.Dhcpv6ClientDuidLength) or
     not CompareMem(@A.Dhcpv6ClientDuid, @B.Dhcpv6ClientDuid, Min(Len, MAX_DHCPV6_DUID_LENGTH)) then
  begin
    Include(Result, _AAF_Dhcpv6ClientDuid);
    if Simplify then
      Exit;
  end;
//  Dhcpv6Iaid: ULONG;
  if A.Dhcpv6Iaid <> B.Dhcpv6Iaid then
  begin
    Include(Result, _AAF_Dhcpv6Iaid);
    if Simplify then
      Exit;
  end;
//  FirstDnsSuffix: PIP_ADAPTER_DNS_SUFFIX;
  if not CompareAdapterDnsSuffix(A.FirstDnsSuffix, B.FirstDnsSuffix) then
    Include(Result, _AAF_FirstDnsSuffix);

end;

{ PSockAddrHelper }

class operator PSockAddrHelper.Implicit(Value: PSockAddrIn): PSockAddr;
begin
  Result := PSockAddr(Value);
end;

class operator PSockAddrHelper.Implicit(Value: PSockaddrIn6): PSockAddr;
begin
  Result := PSockAddr(Value);
end;

class operator PSockAddrHelper.Implicit(Value: PSockAddr): PSockAddrIn;
begin
  Result := PSockAddrIn(Value);
end;

class operator PSockAddrHelper.Implicit(Value: PSockAddr): PSockaddrIn6;
begin
  Result := PSockaddrIn6(Value);
end;

class operator PSockAddrHelper.Explicit(Value: PSockAddrIn): PSockAddr;
begin
  Result := PSockAddr(Value);
end;

class operator PSockAddrHelper.Explicit(Value: PSockaddrIn6): PSockAddr;
begin
  Result := PSockAddr(Value);
end;

class operator PSockAddrHelper.Explicit(Value: PSockAddr): PSockAddrIn;
begin
  Result := PSockAddrIn(Value);
end;

class operator PSockAddrHelper.Explicit(Value: PSockAddr): PSockaddrIn6;
begin
  Result := PSockaddrIn6(Value);
end;

{ PSocketAddressHelper }

class operator PSocketAddressHelper.Implicit(Value: Winapi.IpTypes.PSOCKET_ADDRESS): PSocketAddress;
begin
  Result := PSocketAddress(Value);
end;

class operator PSocketAddressHelper.Implicit(Value: Winapi.Winsock2.PSOCKET_ADDRESS): PSocketAddress;
begin
  Result := PSocketAddress(Value);
end;

class operator PSocketAddressHelper.Implicit(Value: PSocketAddress): Winapi.IpTypes.PSOCKET_ADDRESS;
begin
  Result := Winapi.IpTypes.PSOCKET_ADDRESS(Value);
end;

class operator PSocketAddressHelper.Implicit(Value: PSocketAddress): Winapi.Winsock2.PSOCKET_ADDRESS;
begin
  Result := Winapi.Winsock2.PSOCKET_ADDRESS(Value);
end;

class operator PSocketAddressHelper.Explicit(Value: Winapi.IpTypes.PSOCKET_ADDRESS): PSocketAddress;
begin
  Result := PSocketAddress(Value);
end;

class operator PSocketAddressHelper.Explicit(Value: Winapi.Winsock2.PSOCKET_ADDRESS): PSocketAddress;
begin
  Result := PSocketAddress(Value);
end;

class operator PSocketAddressHelper.Explicit(Value: PSocketAddress): Winapi.IpTypes.PSOCKET_ADDRESS;
begin
  Result := Winapi.IpTypes.PSOCKET_ADDRESS(Value);
end;

class operator PSocketAddressHelper.Explicit(Value: PSocketAddress): Winapi.Winsock2.PSOCKET_ADDRESS;
begin
  Result := Winapi.Winsock2.PSOCKET_ADDRESS(Value);
end;

{ TSockAddrList }

constructor TSockAddrList.Create;
begin
  inherited;
end;

destructor TSockAddrList.Destroy;
begin
  inherited;
end;

function TSockAddrList.Add(const ASockAddr: sockaddr): Integer;
var
  p: PSockAddr;
begin
  Result := Count;
  Count := Result + 1;
  p := @List[Result];
  FillChar(p^, SizeOf(TSockAddr), 0);
  System.Move(ASockAddr, p^, SizeOf(sockaddr));
end;

function TSockAddrList.Add(const ASockAddr: sockaddr_in): Integer;
var
  p: PSockAddr;
begin
  Result := Count;
  Count := Result + 1;
  p := @List[Result];
  FillChar(p^, SizeOf(TSockAddr), 0);
  System.Move(ASockAddr, p^, SizeOf(sockaddr_in));
end;

function TSockAddrList.Add(const ASockAddr: sockaddr_in6): Integer;
var
  p: PSockAddr;
begin
  Result := Count;
  Count := Result + 1;
  p := @List[Result];
  FillChar(p^, SizeOf(TSockAddr), 0);
  System.Move(ASockAddr, p^, SizeOf(sockaddr_in6));
end;


function TSockAddrList.Add(const ASockAddr: TSocketAddress): Integer;
var
  p: PSockAddr;
begin
  if ASockAddr.Length > SizeOf(TSockAddr) then
    raise Exception.CreateFmt(_ErrLengthOverflow, ['SockAddr', ASockAddr.Length, SizeOf(TSockAddr)]);

  Result := Count;
  Count := Result + 1;
  p := @List[Result];
  FillChar(p^, SizeOf(TSockAddr), 0);
  System.Move(ASockAddr.Address^, p^, ASockAddr.Length);
end;

function TSockAddrList.Add(const ASockAddr: Winapi.IpTypes.SOCKET_ADDRESS): Integer;
var
  p: PSockAddr;
begin
  if ASockAddr.iSockaddrLength > SizeOf(TSockAddr) then
    raise Exception.CreateFmt(_ErrLengthOverflow, ['SOCKET_ADDRESS', ASockAddr.iSockaddrLength, SizeOf(TSockAddr)]);

  Result := Count;
  Count := Result + 1;
  p := @List[Result];
  FillChar(p^, SizeOf(TSockAddr), 0);
  System.Move(ASockAddr.lpSockaddr^, p^, ASockAddr.iSockaddrLength);
end;

{ TAdapterAddressList }

constructor TAdapterAddressList.Create;
begin
  inherited;
end;

destructor TAdapterAddressList.Destroy;
begin
  inherited;
end;

function TAdapterAddressList.Find(const AdapterName: string; FromIndex: Integer): Integer;
var
  I: Integer;
begin
  for I := FromIndex to Count - 1 do
    if List[I].AdapterName = AdapterName then
      Exit(I);
  Result := -1;
end;

function TAdapterAddressList.FindByData(const AData: Pointer; FromIndex: Integer): Integer;
var
  I: Integer;
begin
  for I := FromIndex to Count - 1 do
    if List[I].Data = AData then
      Exit(I);
  Result := -1;
end;

function TAdapterAddressList.Find(const AdapterName: string; const Unicast, Gateway: TSockAddr): Integer;
var
  I: Integer;
  p: PAdapterAddress;
  family: u_short;
  Size: Integer;
begin
  I := 0;
  I := Find(AdapterName, I);
  while I >= 0 do
  begin
    p := @List[I];
    family := Gateway.base.sin_family;
    if Unicast.base.sin_family = family then
    begin
      case family of
        AF_INET : Size := SizeOf(sockaddr_in);
        AF_INET6: Size := SizeOf(sockaddr_in6);
        else Size := 0;
      end;
      if Size > 0 then
        if CompareMem(@p.Unicast, @Unicast, Size) then
          if CompareMem(@p.Gateway, @Gateway, Size) then
            Break;
    end;
    I := Find(AdapterName, I);
  end;
  Result := I;
end;

function TAdapterAddressList.Add(const AdapterName, FriendlyName: string; const Unicast, Gateway: TSockAddr): Integer;
var
  p: PAdapterAddress;
begin
  Result := Count;
  Count := Result + 1;
  p := @List[Result];
  FillChar(p^, SizeOf(TAdapterAddress), 0);
  p.AdapterName := AdapterName;
  p.FriendlyName := FriendlyName;
  p.Unicast := Unicast;
  p.Gateway := Gateway;
end;

function TAdapterAddressList.Add(const AdapterName, FriendlyName: string; const Unicast, Gateway: TSocketAddress): Integer;
var
  p: PAdapterAddress;
begin
  if Unicast.Length > SizeOf(TSockAddr) then
    raise Exception.CreateFmt(_ErrLengthOverflow, ['Unicast', Unicast.Length, SizeOf(TSockAddr)]);

  if Gateway.Length > SizeOf(TSockAddr) then
    raise Exception.CreateFmt(_ErrLengthOverflow, ['Gateway', Gateway.Length, SizeOf(TSockAddr)]);

  Result := Count;
  Count := Result + 1;
  p := @List[Result];
  p.Data := nil;
  p.AdapterName := AdapterName;
  p.FriendlyName := FriendlyName;
  FillChar(p.Unicast, SizeOf(TSockAddr), 0);
  System.Move(Unicast.Address^, p.Unicast, Unicast.Length);
  FillChar(p.Gateway, SizeOf(TSockAddr), 0);
  System.Move(Gateway.Address^, p.Gateway, Gateway.Length);
end;

function TAdapterAddressList.Add(const AdapterName, FriendlyName: string; const Unicast, Gateway: Winapi.IpTypes.SOCKET_ADDRESS): Integer;
begin
  Result := Add(AdapterName, FriendlyName, TSocketAddress(Unicast), TSocketAddress(Gateway));
end;

{ TAddrInfoList }

constructor TAddrInfoList.Create;
begin
  inherited;
end;

destructor TAddrInfoList.Destroy;
begin
  inherited;
end;

function TAddrInfoList.Add(const Value: addrinfo): Integer;
var
  p: PAddrInfo;
begin
  Result := Count;
  Count := Result + 1;
  p := @List[Result];
  p.flags := Value.ai_flags;
  p.family := Value.ai_family;
  p.socktype := Value.ai_socktype;
  p.protocol := Value.ai_protocol;
  p.canonname := string(Value.ai_canonname);
  FillChar(p.addr, SizeOf(TSockAddr), 0);
  System.Move(Value.ai_addr^, p.addr, Value.ai_addrlen);
end;

function TAddrInfoList.Add(const Value: addrinfoW): Integer;
var
  p: PAddrInfo;
begin
  Result := Count;
  Count := Result + 1;
  p := @List[Result];
  p.flags := Value.ai_flags;
  p.family := Value.ai_family;
  p.socktype := Value.ai_socktype;
  p.protocol := Value.ai_protocol;
  p.canonname := WideCharToString(Value.ai_canonname);
  FillChar(p.addr, SizeOf(TSockAddr), 0);
  System.Move(Value.ai_addr^, p.addr, Value.ai_addrlen);
end;


initialization

finalization
  Cleanup;

end.
