{
               ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2023, Isaque Pinheiro
                          All rights reserved.

                    GNU Lesser General Public License
                      Versão 3, 29 de junho de 2007

       Copyright (C) 2007 Free Software Foundation, Inc. <http://fsf.org/>
       A todos é permitido copiar e distribuir cópias deste documento de
       licença, mas mudá-lo não é permitido.

       Esta versão da GNU Lesser General Public License incorpora
       os termos e condições da versão 3 da GNU General Public License
       Licença, complementado pelas permissões adicionais listadas no
       arquivo LICENSE na pasta principal.
}

{
  @abstract(ECLBr Library)
  @created(23 Abr 2023)
  @author(Isaque Pinheiro <isaquepsp@gmail.com>)
  @Discord(https://discord.gg/T2zJC8zX)
}

unit eclbr.std;

interface

uses
  Rtti,
  Math,
  Classes,
  Windows,
  TypInfo,
  SysUtils,
  DateUtils,
  Generics.Collections;

type
  TArrayString = array of String;
  TListString = TList<String>;
  Tuple = array of TValue;

  PPacket = ^TPacket;
  TPacket = packed record
    case Integer of
      0: (b0, b1, b2, b3: Byte);
      1: (i: Integer);
      2: (a: array[0..3] of Byte);
      3: (c: array[0..3] of AnsiChar);
  end;

  IObserverEx = interface
    ['{5887CDFF-DA23-4466-A5CB-FBA1DFEAF907}']
    procedure Update(const Progress: Integer);
  end;

  TArrayHelper = class helper for TArray
  public
    class procedure ForEach<T>(const AValues: array of T; AAction: TProc<T>); static;
    class function Copy<T>(const AValues: array of T): TArray<T>; static;
    class function Reduce<T>(const AValues: array of T; AReducer: TFunc<T, T, T>): T; static;
    class function Map<T, TResult>(const AValues: array of T; AFunc: TFunc<T, TResult>): TArray<TResult>; static;
    class function Filter<T>(const AValues: array of T; APredicate: TFunc<T, Boolean>): TArray<T>; static;
    class function Any<T>(const AValues: array of T; APredicate: TFunc<T, Boolean>): Boolean; static;
    class function All<T>(const AValues: array of T; APredicate: TFunc<T, Boolean>): Boolean; static;
  end;

  TFuture = record
  private
    FValue: TValue;
    FErr: String;
    FIsOK: Boolean;
    FIsErr: Boolean;
  public
    function IsOk: Boolean;
    function IsErr: Boolean;
    function Ok<T>: T;
    function Err: String;
    procedure SetOk(const AValue: TValue);
    procedure SetErr(const AErr: String);
  end;

  TStd = class
  strict private
    class function _DecodePacket(AInBuf: PAnsiChar; var nChars: Integer): TPacket; static;
    class procedure _EncodePacket(const APacket: TPacket; NumChars: Integer;
      AOutBuf: PAnsiChar); static;
  public
    class var FormatSettings: TFormatSettings;
    class function ArrayMerge<T>(const AArray1: TArray<T>;
      const AArray2: TArray<T>): TArray<T>; inline;
    class function ArrayCopy(const ASource: TArrayString; const AIndex: Integer;
      const ACount: Integer): TArrayString; inline;
    class function IfThen<T>(AValue: Boolean; const ATrue: T; const AFalse: T): T; inline;
    class function AsList<T>(const AArray: TArray<T>): TList<T>; inline;
    class function JoinStrings(const AStrings: TArrayString;
      const ASeparator: String): String; overload; inline;
    class function JoinStrings(const AStrings: TListString;
      const ASeparator: String): String; overload; inline;
    class function RemoveTrailingChars(const AStr: String; const AChars: TSysCharSet): String; inline;
    class function Iso8601ToDateTime(const AValue: String;
      const AUseISO8601DateFormat: Boolean): TDateTime; inline;
    class function DateTimeToIso8601(const AValue: TDateTime;
      const AUseISO8601DateFormat: Boolean): String; inline;
    class function DecodeBase64(const AInput: String): TBytes; inline;
    class function EncodeBase64(const AInput: Pointer; const ASize: Integer): String; inline;
    class function EncodeString(const AInput: String): String; inline;
    class function DecodeString(const AInput: String): String; inline;
    class function Min(const A, B: Integer): Integer; overload; inline;
    class function Min(const A, B: Double): Double; overload; inline;
    class function Min(const A, B: Currency): Currency; overload; inline;
    class function Min(const A, B: Int64): Int64; overload; inline;
    class function Max(const A, B: Integer): Integer; overload; inline;
    class function Max(const A, B: Double): Double; overload; inline;
    class function Max(const A, B: Currency): Currency; overload; inline;
    class function Sum(const Data: array of Single): Single; overload;
    class function Sum(const Data: array of Double): Double; overload;
    class function Sum(const Data: array of Extended): Extended; overload;
    class function Total(const Data: array of Single): Single; overload;
    class function Total(const Data: array of Double): Double; overload;
    class function Total(const Data: array of Extended): Extended; overload;
    class function Split(const S: String): TArray<String>; inline;
    class function Clone<T>(const AFirst: Pointer; ASize: Cardinal; var Return): Pointer; inline;
    class function Hash(const AValue: MarshaledAString): Cardinal;
    class procedure EncodeStream(const AInput, AOutput: TStream);
    class procedure DecodeStream(const AInput, AOutput: TStream);
    class procedure Fill<T>(const AFirst: Pointer; ASize: Cardinal; const Value: T); inline;
  end;

{$IFDEF DEBUG}
procedure DebugPrint(const AMessage: String);
{$ENDIF}

implementation

{$IFDEF DEBUG}
procedure DebugPrint(const AMessage: String);
begin
  TThread.Queue(nil,
          procedure
          begin
            OutputDebugString(PWideChar('[ECLBr] - ' + FormatDateTime('mm/dd/yyyy, hh:mm:ss am/pm', Now) + ' LOG ' + AMessage));
          end);
end;
{$ENDIF}

const
  C_BUFFERSIZE = 510;
  C_LINEBREAKINTERVAL = 75;

  C_ENCODETABLE: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  C_DECODETABLE: array[#0..#127] of Integer = (
    Byte('='), 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
           64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
           64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
           52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
           64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
           15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
           64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
           41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64);

type
  TPointerStream = class(TCustomMemoryStream)
  public
    constructor Create(P: Pointer; ASize: Integer);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

{ TSysLib }

class function TStd.ArrayCopy(const ASource: TArrayString; const AIndex,
  ACount: Integer): TArrayString;
var
  LFor: Integer;
begin
  SetLength(Result, ACount);
  for LFor := 0 to ACount - 1 do
    Result[LFor] := ASource[AIndex + LFor];
end;

class function TStd.ArrayMerge<T>(const AArray1,
  AArray2: TArray<T>): TArray<T>;
var
  LLength1: Integer;
  LLength2: Integer;
begin
  LLength1 := Length(AArray1);
  LLength2 := Length(AArray2);
  if (LLength1 = 0) and (LLength2 = 0) then
  begin
    Result := [];
    exit;
  end;
  SetLength(Result, LLength1 + LLength2);
  if LLength1 > 0 then
    Move(AArray1[0], Result[0], LLength1 * SizeOf(T));
  if LLength2 > 0 then
    Move(AArray2[0], Result[LLength1], LLength2 * SizeOf(T));
end;

class function TStd.AsList<T>(const AArray: TArray<T>): TList<T>;
var
  LFor: Integer;
begin
  Result := TList<T>.Create;
  for LFor := 0 to High(AArray) do
    Result.Add(AArray[LFor]);
end;

class function TStd.DateTimeToIso8601(const AValue: TDateTime;
  const AUseISO8601DateFormat: Boolean): String;
var
  LDatePart: String;
  LTimePart: String;
begin
  Result := '';
  if AValue = 0 then
    exit;

  if AUseISO8601DateFormat then
    LDatePart := FormatDateTime('yyyy-mm-dd', AValue)
  else
    LDatePart := DateToStr(AValue, FormatSettings);

  if Frac(AValue) = 0 then
    Result := ifThen<String>(AUseISO8601DateFormat, LDatePart, TimeToStr(AValue, FormatSettings))
  else
  begin
    LTimePart := FormatDateTime('hh:nn:ss', AValue);
    Result := ifThen<String>(AUseISO8601DateFormat, LDatePart + 'T' + LTimePart, LDatePart + ' ' + LTimePart);
  end;
end;

class function TStd.IfThen<T>(AValue: Boolean; const ATrue, AFalse: T): T;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

class function TStd.Iso8601ToDateTime(const AValue: String;
  const AUseISO8601DateFormat: Boolean): TDateTime;
var
  LYYYY: Integer;
  LMM: Integer;
  LDD: Integer;
  LHH: Integer;
  LMI: Integer;
  LSS: Integer;
  LMS: Integer;
begin
  if not AUseISO8601DateFormat then
  begin
    Result := StrToDateTimeDef(AValue, 0);
    exit;
  end;
  LYYYY := 0; LMM := 0; LDD := 0; LHH := 0; LMI := 0; LSS := 0; LMS := 0;
  if TryStrToInt(Copy(AValue, 1, 4), LYYYY) and
     TryStrToInt(Copy(AValue, 6, 2), LMM) and
     TryStrToInt(Copy(AValue, 9, 2), LDD) and
     TryStrToInt(Copy(AValue, 12, 2), LHH) and
     TryStrToInt(Copy(AValue, 15, 2), LMI) and
     TryStrToInt(Copy(AValue, 18, 2), LSS) then
  begin
    Result := EncodeDateTime(LYYYY, LMM, LDD, LHH, LMI, LSS, LMS);
  end
  else
    Result := 0;
end;

class function TStd.JoinStrings(const AStrings: TListString;
  const ASeparator: String): String;
var
  LFor: Integer;
begin
  Result := '';
  if AStrings.Count = 0 then
    Exit;
  for LFor := 0 to AStrings.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ASeparator;
    Result := Result + AStrings[LFor];
  end;
end;

class function TStd.Min(const A, B: Integer): Integer;
begin
  Result := Math.Min(A, B);
end;

class function TStd.Min(const A, B: Double): Double;
begin
  Result := Math.Min(A, B);
end;

class function TStd.Max(const A, B: Integer): Integer;
begin
  Result := Math.Max(A, B);
end;

class function TStd.Max(const A, B: Double): Double;
begin
  Result := Math.Max(A, B);
end;

class function TStd.Max(const A, B: Currency): Currency;
begin
  Result := Math.Max(A, B);
end;

class function TStd.Min(const A, B: Int64): Int64;
begin
  Result := Math.Min(A, B);
end;

class function TStd.Min(const A, B: Currency): Currency;
begin
  Result := Math.Min(A, B);
end;

class function TStd.RemoveTrailingChars(const AStr: String;
  const AChars: TSysCharSet): String;
var
  LLastCharIndex: Integer;
begin
  LLastCharIndex := Length(AStr);
  while (LLastCharIndex > 0) and CharInSet(AStr[LLastCharIndex], AChars) do
    Dec(LLastCharIndex);
  Result := Copy(AStr, 1, LLastCharIndex);
end;

class function TStd.Split(const S: String): TArray<String>;
var
  LFor: Integer;
begin
  SetLength(Result, Length(S));
  for LFor := 1 to Length(S) do
    Result[LFor - 1] := S[LFor];
end;

class function TStd.Sum(const Data: array of Single): Single;
begin
  Result := Math.Sum(Data);
end;

class function TStd.Sum(const Data: array of Double): Double;
begin
  Result := Math.Sum(Data);
end;

class function TStd.Sum(const Data: array of Extended): Extended;
begin
  Result := Math.Sum(Data);
end;

class function TStd.Total(const Data: array of Single): Single;
begin
  Result := Math.TotalVariance(Data);
end;

class function TStd.Total(const Data: array of Double): Double;
begin
  Result := Math.TotalVariance(Data);
end;

class function TStd.Total(const Data: array of Extended): Extended;
begin
  Result := Math.TotalVariance(Data);
end;

class function TStd.JoinStrings(const AStrings: TArrayString;
  const ASeparator: String): String;
var
  LFor: Integer;
begin
  Result := '';
  for LFor := Low(AStrings) to High(AStrings) do
  begin
    if LFor > Low(AStrings) then
      Result := Result + ASeparator;
    Result := Result + AStrings[LFor];
  end;
end;

class function TStd.DecodeBase64(const AInput: String): TBytes;
var
  LInStr: TMemoryStream;
  LOutStr: TBytesStream;
  LStream: TStringStream;
  LSize: Integer;
begin
  LInStr := TMemoryStream.Create;
  LStream := TStringStream.Create(AInput, TEncoding.ASCII);
  try
    LInStr.LoadFromStream(LStream);
    LOutStr := TBytesStream.Create;
    try
      DecodeStream(LInStr, LOutStr);
      LSize := LOutStr.Size;
      SetLength(Result, LSize);
      LOutStr.Position := 0;
      LOutStr.Read(Result[0], LSize);
    finally
      LOutStr.Free;
    end;
  finally
    LStream.Free;
    LInStr.Free;
  end;
end;

class procedure TStd._EncodePacket(const APacket: TPacket; NumChars: Integer;
 AOutBuf: PAnsiChar);
begin
  AOutBuf[0] := C_ENCODETABLE[APacket.a[0] shr 2];
  AOutBuf[1] := C_ENCODETABLE[((APacket.a[0] shl 4) or (APacket.a[1] shr 4)) and $0000003f];
  if NumChars < 2 then
    AOutBuf[2] := '='
  else AOutBuf[2] := C_ENCODETABLE[((APacket.a[1] shl 2) or (APacket.a[2] shr 6)) and $0000003f];
  if NumChars < 3 then
    AOutBuf[3] := '='
  else AOutBuf[3] := C_ENCODETABLE[APacket.a[2] and $0000003f];
end;

class function TStd._DecodePacket(AInBuf: PAnsiChar; var nChars: Integer): TPacket;
begin
  Result.a[0] := (C_DECODETABLE[AInBuf[0]] shl 2) or
    (C_DECODETABLE[AInBuf[1]] shr 4);
  NChars := 1;
  if AInBuf[2] <> '=' then
  begin
    Inc(NChars);
    Result.a[1] := Byte((C_DECODETABLE[AInBuf[1]] shl 4) or (C_DECODETABLE[AInBuf[2]] shr 2));
  end;
  if AInBuf[3] <> '=' then
  begin
    Inc(NChars);
    Result.a[2] := Byte((C_DECODETABLE[AInBuf[2]] shl 6) or C_DECODETABLE[AInBuf[3]]);
  end;
end;

class procedure TStd.EncodeStream(const AInput, AOutput: TStream);
var
  LInBuffer: array[0..C_BUFFERSIZE] of Byte;
  LOutBuffer: array[0..1023] of AnsiChar;
  LBufferPtr: PAnsiChar;
  LI: Integer;
  LJ: Integer;
  BytesRead: Integer;
  LPacket: TPacket;

  procedure WriteLineBreak;
  begin
    LOutBuffer[0] := #$0D;
    LOutBuffer[1] := #$0A;
    LBufferPtr := @LOutBuffer[2];
  end;

begin
  LBufferPtr := @LOutBuffer[0];
  repeat
    BytesRead := AInput.Read(LInBuffer, SizeOf(LInBuffer));
    LI := 0;
    while LI < BytesRead do
    begin
      LJ := Min(3, BytesRead - LI);
      FillChar(LPacket, SizeOf(LPacket), 0);
      Move(LInBuffer[LI], LPacket, LJ);
      _EncodePacket(LPacket, LJ, LBufferPtr);
      Inc(LI, 3);
      Inc(LBufferPtr, 4);
      if LBufferPtr - @LOutBuffer[0] > SizeOf(LOutBuffer) - C_LINEBREAKINTERVAL then
      begin
        WriteLineBreak;
        AOutput.Write(LOutBuffer, LBufferPtr - @LOutBuffer[0]);
        LBufferPtr := @LOutBuffer[0];
      end;
    end;
  until BytesRead = 0;
  if LBufferPtr <> @LOutBuffer[0] then
    AOutput.Write(LOutBuffer, LBufferPtr - @LOutBuffer[0]);
end;

class procedure TStd.DecodeStream(const AInput, AOutput: TStream);
var
  LInBuf: array[0..75] of AnsiChar;
  LOutBuf: array[0..60] of Byte;
  LInBufPtr, LOutBufPtr: PAnsiChar;
  LI: Integer;
  LJ: Integer;
  LK: Integer;
  BytesRead: Integer;
  LPacket: TPacket;

  procedure SkipWhite;
  var
    LC: AnsiChar;
    LNumRead: Integer;
  begin
    while True do
    begin
      LNumRead := AInput.Read(LC, 1);
      if LNumRead = 1 then
      begin
        if LC in ['0'..'9','A'..'Z','a'..'z','+','/','='] then
        begin
          AInput.Position := AInput.Position - 1;
          break;
        end;
      end
      else
        break;
    end;
  end;

  function ReadInput: Integer;
  var
    LWhiteFound: Boolean;
    LEndReached: Boolean;
    LCntRead: Integer;
    LIdx: Integer;
    LIdxEnd: Integer;
  begin
    LIdxEnd:= 0;
    repeat
      LWhiteFound := False;
      LCntRead := AInput.Read(LInBuf[LIdxEnd], (SizeOf(LInBuf)-LIdxEnd));
      LEndReached := LCntRead < (SizeOf(LInBuf)-LIdxEnd);
      LIdx := LIdxEnd;
      LIdxEnd := LCntRead + LIdxEnd;
      while (LIdx < LIdxEnd) do
      begin
        if not (LInBuf[LIdx] in ['0'..'9','A'..'Z','a'..'z','+','/','=']) then
        begin
          Dec(LIdxEnd);
          if LIdx < LIdxEnd then
            Move(LInBuf[LIdx+1], LInBuf[LIdx], LIdxEnd-LIdx);
          LWhiteFound := True;
        end
        else
          Inc(LIdx);
      end;
    until (not LWhiteFound) or (LEndReached);
    Result := LIdxEnd;
  end;

begin
  repeat
    SkipWhite;
    BytesRead := ReadInput;
    LInBufPtr := LInBuf;
    LOutBufPtr := @LOutBuf;
    LI := 0;
    while LI < BytesRead do
    begin
      LPacket := _DecodePacket(LInBufPtr, LJ);
      LK := 0;
      while LJ > 0 do
      begin
        LOutBufPtr^ := AnsiChar(LPacket.a[LK]);
        Inc(LOutBufPtr);
        Dec(LJ);
        Inc(LK);
      end;
      Inc(LInBufPtr, 4);
      Inc(LI, 4);
    end;
    AOutput.Write(LOutBuf, LOutBufPtr - PAnsiChar(@LOutBuf));
  until BytesRead = 0;
end;

class function TStd.EncodeString(const AInput: String): String;
var
  LInStr: TStringStream;
  LOutStr: TStringStream;
begin
  LInStr := TStringStream.Create(AInput);
  try
    LOutStr := TStringStream.Create('');
    try
      EncodeStream(LInStr, LOutStr);
      Result := LOutStr.DataString;
    finally
      LOutStr.Free;
    end;
  finally
    LInStr.Free;
  end;
end;

class function TStd.DecodeString(const AInput: String): String;
var
  LInStr: TStringStream;
  LOutStr: TStringStream;
begin
  LInStr := TStringStream.Create(AInput);
  try
    LOutStr := TStringStream.Create('');
    try
      DecodeStream(LInStr, LOutStr);
      Result := LOutStr.DataString;
    finally
      LOutStr.Free;
    end;
  finally
    LInStr.Free;
  end;
end;

class function TStd.EncodeBase64(const AInput: Pointer; const ASize: Integer): String;
var
  LInStream: TMemoryStream;
  LOutStream: TMemoryStream;
begin
  LInStream := TMemoryStream.Create;
  try
    LInStream.WriteBuffer(AInput^, ASize);
    LInStream.Position := 0;
    LOutStream := TMemoryStream.Create;
    try
      EncodeStream(LInStream, LOutStream);
      SetString(Result, PAnsiChar(LOutStream.Memory), LOutStream.Size);
    finally
      LOutStream.Free;
    end;
  finally
    LInStream.Free;
  end;
end;

class function TStd.Clone<T>(const AFirst: Pointer; ASize: Cardinal; var Return): Pointer;
var
  LSource: ^T;
  LTarget: ^T;
begin
  if (ASize <= 0) or (AFirst = nil) then
    raise Exception.Create('Invalid parameters in TStd.Clone');

  LSource := AFirst;
  LTarget := @Return;
  while ASize > 0 do
  begin
    LTarget^ := LSource^;
    Inc(PByte(LSource), sizeof(T));
    Inc(PByte(LTarget), sizeof(T));
    Dec(ASize);
  end;
  Result := @Return;
end;

class procedure TStd.Fill<T>(const AFirst: Pointer; ASize: Cardinal; const Value: T);
var
  LPointer: ^T;
begin
  if (ASize <= 0) or (AFirst = nil) then
    raise Exception.Create('Invalid parameters in TStd.Fill');

  LPointer := AFirst;
  repeat
    LPointer^ := Value;
    Inc(PByte(LPointer), sizeof(T));
    Dec(ASize);
  until ASize = 0;
end;

class function TStd.Hash(const AValue: MarshaledAString): Cardinal;
begin
  Result := SysUtils.HashName(AValue);
end;

{ TPointerStream }

constructor TPointerStream.Create(P: Pointer; ASize: Integer);
begin
  SetPointer(P, ASize);
end;

function TPointerStream.Write(const Buffer; Count: Longint): Longint;
var
  LPos: Longint;
  LEndPos: Longint;
  LSize: Longint;
  LMem: Pointer;
begin
  LPos := Self.Position;
  if (LPos >= 0) and (Count > 0) then
  begin
    LEndPos := LPos + Count;
    LSize := Self.Size;
    if LEndPos > LSize then
      raise EStreamError.Create('Out of memory while expanding memory stream');
    LMem := Self.Memory;
    System.Move(Buffer, Pointer(Longint(LMem) + LPos)^, Count);
    Self.Position := LPos;
    Result := Count;
    exit;
  end;
  Result := 0;
end;

{ TArrayHelper }

class function TArrayHelper.Copy<T>(const AValues: array of T): TArray<T>;
var
  LFor: Integer;
begin
  SetLength(Result, Length(AValues));
  for LFor := Low(AValues) to High(AValues) do
    Result[LFor] := AValues[LFor];
end;

class function TArrayHelper.Filter<T>(const AValues: array of T;
  APredicate: TFunc<T, Boolean>): TArray<T>;
var
  LItem: T;
begin
  for LItem in AValues do
  begin
    if APredicate(LItem) then
      Result := Result + [LItem];
  end;
end;

class procedure TArrayHelper.ForEach<T>(const AValues: array of T;
  AAction: TProc<T>);
var
  LItem: T;
begin
  for LItem in AValues do
    AAction(LItem);
end;

class function TArrayHelper.Any<T>(const AValues: array of T;
  APredicate: TFunc<T, Boolean>): Boolean;
var
  LItem: T;
begin
  for LItem in AValues do
    if APredicate(LItem) then
      Exit(True);

  Result := False;
end;

class function TArrayHelper.All<T>(const AValues: array of T;
  APredicate: TFunc<T, Boolean>): Boolean;
var
  LItem: T;
begin
  for LItem in AValues do
    if not APredicate(LItem) then
      Exit(False);

  Result := True;
end;

class function TArrayHelper.Map<T, TResult>(const AValues: array of T;
  AFunc: TFunc<T, TResult>): TArray<TResult>;
var
  LIndex: Integer;
begin
  SetLength(Result, Length(AValues));
  for LIndex := 0 to High(AValues) do
    Result[LIndex] := AFunc(AValues[LIndex]);
end;

class function TArrayHelper.Reduce<T>(const AValues: array of T;
  AReducer: TFunc<T, T, T>): T;
var
  LValue: T;
  LItem: T;
begin
  if Length(AValues) = 0 then
    raise EArgumentException.Create('Cannot reduce an empty array');

  LValue := Default(T);
  for LItem in AValues do
    LValue := AReducer(LValue, LItem);

  Result := LValue;
end;

{ TFuture }

function TFuture.Err: String;
begin
  Result := FErr;
end;

function TFuture.IsErr: Boolean;
begin
  Result := FIsErr;
end;

function TFuture.IsOk: Boolean;
begin
  Result := FIsOK;
end;

function TFuture.Ok<T>: T;
begin
  Result := FValue.AsType<T>;
end;

procedure TFuture.SetErr(const AErr: String);
begin
  FErr := AErr;
  FIsErr := True;
  FIsOK := False;
end;

procedure TFuture.SetOk(const AValue: TValue);
begin
  FValue := AValue;
  FIsOK := True;
  FIsErr := False;
end;

initialization
  TStd.FormatSettings := TFormatSettings.Create('en_US');

end.









































