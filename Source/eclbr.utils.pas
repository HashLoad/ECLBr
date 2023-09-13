{
             ECL Brasil - Essential Core Library for Delphi

                   Copyright (c) 2022, Isaque Pinheiro
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
  @Telegram(https://t.me/ormbr)
}

unit eclbr.utils;

interface

uses
  Rtti,
  Classes,
  SysUtils,
  Generics.Collections;

type
  TArrayString = array of string;
  TListString = TList<string>;

  PPacket = ^TPacket;
  TPacket = packed record
    case Integer of
      0: (b0, b1, b2, b3: Byte);
      1: (i: Integer);
      2: (a: array[0..3] of Byte);
      3: (c: array[0..3] of AnsiChar);
  end;

  TUtils = class
  private
    class function _DecodePacket(InBuf: PAnsiChar; var nChars: Integer): TPacket; static;
    class procedure _EncodePacket(const Packet: TPacket; NumChars: Integer;
      OutBuf: PAnsiChar); static;
  public
    /// <summary>
    ///   Gets or sets the global format settings used for various formatting operations.
    /// </summary>
    class var FormatSettings: TFormatSettings;

    /// <summary>
    ///   Merges two arrays of the same type and returns the merged array.
    /// </summary>
    /// <typeparam name="T">The type of elements in the arrays.</typeparam>
    /// <param name="AArray1">The first array to merge.</param>
    /// <param name="AArray2">The second array to merge.</param>
    /// <returns>The merged array containing elements from both input arrays.</returns>
    class function ArrayMerge<T>(const AArray1: TArray<T>;
      const AArray2: TArray<T>): TArray<T>; inline;

    /// <summary>
    ///   Copies a portion of a string array to a new array, starting from the specified index and up to the specified count.
    /// </summary>
    /// <param name="ASource">The source string array.</param>
    /// <param name="AIndex">The starting index of the copy operation.</param>
    /// <param name="ACount">The number of elements to copy.</param>
    /// <returns>A new string array containing the copied elements.</returns>
    class function ArrayCopy(const ASource: TArrayString; const AIndex: integer;
      const ACount: integer): TArrayString; inline;

    /// <summary>
    ///   Returns one of two values based on a specified condition.
    /// </summary>
    /// <typeparam name="T">The type of values to return.</typeparam>
    /// <param name="AValue">The condition to evaluate.</param>
    /// <param name="ATrue">The value to return if the condition is true.</param>
    /// <param name="AFalse">The value to return if the condition is false.</param>
    /// <returns>The value specified by either <paramref name="ATrue"/> or <paramref name="AFalse"/> based on the condition <paramref name="AValue"/>.</returns>
    class function IfThen<T>(AValue: boolean; const ATrue: T; const AFalse: T): T; inline;

    /// <summary>
    ///   Converts an array of elements into a generic list.
    /// </summary>
    /// <typeparam name="T">The type of elements in the array and list.</typeparam>
    /// <param name="AArray">The array to convert into a list.</param>
    /// <returns>A generic list containing the elements from the input array.</returns>
    class function AsList<T>(const AArray: TArray<T>): TList<T>; inline;

    /// <summary>
    ///   Joins an array of strings into a single string using the specified separator.
    /// </summary>
    /// <param name="AStrings">The array of strings to join.</param>
    /// <param name="ASeparator">The separator to use between joined strings.</param>
    /// <returns>The concatenated string with elements separated by <paramref name="ASeparator"/>.</returns>
    class function JoinStrings(const AStrings: TArrayString;
      const ASeparator: string): string; overload; inline;

    /// <summary>
    ///   Joins a list of strings into a single string using the specified separator.
    /// </summary>
    /// <param name="AStrings">The list of strings to join.</param>
    /// <param name="ASeparator">The separator to use between joined strings.</param>
    /// <returns>The concatenated string with elements separated by <paramref name="ASeparator"/>.</returns>
    class function JoinStrings(const AStrings: TListString;
      const ASeparator: string): string; overload; inline;

    /// <summary>
    ///   Removes trailing characters from a string.
    /// </summary>
    /// <param name="AStr">The input string to remove trailing characters from.</param>
    /// <param name="AChars">The set of characters to remove from the end of the string.</param>
    /// <returns>The input string with trailing characters removed.</returns>
    class function RemoveTrailingChars(const AStr: string; const AChars: TSysCharSet): string; inline;

    /// <summary>
    ///   Converts an ISO 8601 date/time string to a Delphi TDateTime value.
    /// </summary>
    /// <param name="AValue">The ISO 8601 date/time string to convert.</param>
    /// <param name="AUseISO8601DateFormat">
    ///   True to use ISO 8601 date format (YYYY-MM-DD), False to use the default format.
    /// </param>
    /// <returns>The corresponding TDateTime value.</returns>
    class function Iso8601ToDateTime(const AValue: string;
      const AUseISO8601DateFormat: Boolean): TDateTime; inline;

    /// <summary>
    ///   Converts a Delphi TDateTime value to an ISO 8601 date/time string.
    /// </summary>
    /// <param name="AValue">The TDateTime value to convert.</param>
    /// <param name="AUseISO8601DateFormat">
    ///   True to use ISO 8601 date format (YYYY-MM-DD), False to use the default format.
    /// </param>
    /// <returns>The ISO 8601 date/time string.</returns>
    class function DateTimeToIso8601(const AValue: TDateTime;
      const AUseISO8601DateFormat: Boolean): string; inline;

    /// <summary>
    ///   Decodes a base64-encoded string into a byte array.
    /// </summary>
    /// <param name="AInput">The base64-encoded input string.</param>
    /// <returns>The decoded byte array.</returns>
    class function DecodeBase64(const AInput: string): TBytes;

    /// <summary>
    ///   Encodes a byte array into a base64-encoded string.
    /// </summary>
    /// <param name="AInput">The byte array to encode.</param>
    /// <param name="ASize">The size of the byte array.</param>
    /// <returns>The base64-encoded string.</returns>
    class function EncodeBase64(const AInput: Pointer; const ASize: Integer): string;

    /// <summary>
    ///   Encodes a string to make it safe for storage or transmission.
    /// </summary>
    /// <param name="AInput">The input string to encode.</param>
    /// <returns>The encoded string.</returns>
    class function EncodeString(const AInput: string): string;

    /// <summary>
    ///   Decodes an encoded string to its original form.
    /// </summary>
    /// <param name="AInput">The encoded input string.</param>
    /// <returns>The decoded string.</returns>
    class function DecodeString(const AInput: string): string;

    /// <summary>
    ///   Returns the minimum of two integers.
    /// </summary>
    /// <param name="A">The first integer value.</param>
    /// <param name="B">The second integer value.</param>
    /// <returns>The smaller of the two integer values.</returns>
    class function Min(const A, B: Integer): Integer; overload;

    /// <summary>
    ///   Returns the minimum of two floating-point numbers.
    /// </summary>
    /// <param name="A">The first floating-point value.</param>
    /// <param name="B">The second floating-point value.</param>
    /// <returns>The smaller of the two floating-point values.</returns>
    class function Min(const A, B: Double): Double; overload;

    /// <summary>
    ///   Returns the minimum of two currency values.
    /// </summary>
    /// <param name="A">The first currency value.</param>
    /// <param name="B">The second currency value.</param>
    /// <returns>The smaller of the two currency values.</returns>
    class function Min(const A, B: Currency): Currency; overload;

    /// <summary>
    ///   Encodes a stream using base64 encoding and writes the result to another stream.
    /// </summary>
    /// <param name="AInput">The input stream to encode.</param>
    /// <param name="AOutput">The output stream to write the encoded result.</param>
    class procedure EncodeStream(const AInput, AOutput: TStream);

    /// <summary>
    ///   Decodes a base64-encoded stream and writes the result to another stream.
    /// </summary>
    /// <param name="AInput">The input stream to decode.</param>
    /// <param name="AOutput">The output stream to write the decoded result.</param>
    class procedure DecodeStream(const AInput, AOutput: TStream);
  end;

implementation

const
  BufferSize = 510; // Tamanho do buffer de leitura
  LineBreakInterval = 75; // Intervalo para quebra de linha

  EncodeTable: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  DecodeTable: array[#0..#127] of Integer = (
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

class function TUtils.ArrayCopy(const ASource: TArrayString; const AIndex,
  ACount: integer): TArrayString;
var
  LFor: integer;
begin
  SetLength(Result, ACount);
  for LFor := 0 to ACount - 1 do
    Result[LFor] := ASource[AIndex + LFor];
end;

class function TUtils.ArrayMerge<T>(const AArray1,
  AArray2: TArray<T>): TArray<T>;
var
  LLength1: integer;
  LLength2: integer;
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

class function TUtils.AsList<T>(const AArray: TArray<T>): TList<T>;
var
  LFor: integer;
begin
  Result := TList<T>.Create;
  for LFor := 0 to High(AArray) do
    Result.Add(AArray[LFor]);
end;

class function TUtils.DateTimeToIso8601(const AValue: TDateTime;
  const AUseISO8601DateFormat: Boolean): string;
var
  LDatePart, LTimePart: string;
begin
  Result := '';
  if AValue = 0 then
    exit;
  if AUseISO8601DateFormat then
    LDatePart := FormatDateTime('yyyy-mm-dd', AValue)
  else
    LDatePart := DateToStr(AValue, FormatSettings);
  if Frac(AValue) = 0 then
    Result := ifThen(AUseISO8601DateFormat, LDatePart, TimeToStr(AValue, FormatSettings))
  else
  begin
    LTimePart := FormatDateTime('hh:nn:ss', AValue);
    Result := ifThen(AUseISO8601DateFormat, LDatePart + 'T' + LTimePart, LDatePart + ' ' + LTimePart);
  end;
end;

class function TUtils.IfThen<T>(AValue: boolean; const ATrue, AFalse: T): T;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

class function TUtils.Iso8601ToDateTime(const AValue: string;
  const AUseISO8601DateFormat: Boolean): TDateTime;
var
  LYYYY, LMM, LDD, LHH, LMI, LSS: Cardinal;
begin
  if AUseISO8601DateFormat then
    Result := StrToDateTimeDef(AValue, 0)
  else
    Result := StrToDateTimeDef(AValue, 0, FormatSettings);

  if Length(AValue) = 19 then
  begin
    LYYYY := StrToIntDef(Copy(AValue, 1, 4), 0);
    LMM := StrToIntDef(Copy(AValue, 6, 2), 0);
    LDD := StrToIntDef(Copy(AValue, 9, 2), 0);
    LHH := StrToIntDef(Copy(AValue, 12, 2), 0);
    LMI := StrToIntDef(Copy(AValue, 15, 2), 0);
    LSS := StrToIntDef(Copy(AValue, 18, 2), 0);
    if (LYYYY <= 9999) and (LMM <= 12) and (LDD <= 31) and
       (LHH < 24) and (LMI < 60) and (LSS < 60) then
      Result := EncodeDate(LYYYY, LMM, LDD) + EncodeTime(LHH, LMI, LSS, 0);
  end;
end;

class function TUtils.JoinStrings(const AStrings: TListString;
  const ASeparator: string): string;
var
  LFor: integer;
begin
  Result := '';
  for LFor := 0 to AStrings.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ASeparator;
    Result := Result + AStrings[LFor];
  end;
end;

class function TUtils.Min(const A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

class function TUtils.Min(const A, B: Double): Double;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

class function TUtils.Min(const A, B: Currency): Currency;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

class function TUtils.RemoveTrailingChars(const AStr: string;
  const AChars: TSysCharSet): string;
var
  LLastCharIndex: integer;
begin
  LLastCharIndex := Length(AStr);
  while (LLastCharIndex > 0) and not CharInSet(AStr[LLastCharIndex], AChars) do
    Dec(LLastCharIndex);
  Result := Copy(AStr, 1, LLastCharIndex);
end;

class function TUtils.JoinStrings(const AStrings: TArrayString;
  const ASeparator: string): string;
var
  LFor: integer;
begin
  Result := '';
  for LFor := Low(AStrings) to High(AStrings) do
  begin
    if LFor > Low(AStrings) then
      Result := Result + ASeparator;
    Result := Result + AStrings[LFor];
  end;
end;

class function TUtils.DecodeBase64(const AInput: string): TBytes;
var
  LInStr: TPointerStream;
  LOutStr: TBytesStream;
  LSize: Integer;
begin
  LInStr := TPointerStream.Create(PAnsiChar(AInput), Length(AInput));
  try
    LOutStr := TBytesStream.Create;
    try
      DecodeStream(LInStr, LOutStr);
      Result := LOutStr.Bytes;
      LSize := LOutStr.Size;
    finally
      LOutStr.Free;
    end;
  finally
    LInStr.Free;
  end;
  SetLength(Result, LSize);
end;

class procedure TUtils._EncodePacket(const Packet: TPacket; NumChars: Integer; OutBuf: PAnsiChar);
begin
  OutBuf[0] := EnCodeTable[Packet.a[0] shr 2];
  OutBuf[1] := EnCodeTable[((Packet.a[0] shl 4) or (Packet.a[1] shr 4)) and $0000003f];
  if NumChars < 2 then
    OutBuf[2] := '='
  else OutBuf[2] := EnCodeTable[((Packet.a[1] shl 2) or (Packet.a[2] shr 6)) and $0000003f];
  if NumChars < 3 then
    OutBuf[3] := '='
  else OutBuf[3] := EnCodeTable[Packet.a[2] and $0000003f];
end;

class function TUtils._DecodePacket(InBuf: PAnsiChar; var nChars: Integer): TPacket;
begin
  Result.a[0] := (DecodeTable[InBuf[0]] shl 2) or
    (DecodeTable[InBuf[1]] shr 4);
  NChars := 1;
  if InBuf[2] <> '=' then
  begin
    Inc(NChars);
    Result.a[1] := Byte((DecodeTable[InBuf[1]] shl 4) or (DecodeTable[InBuf[2]] shr 2));
  end;
  if InBuf[3] <> '=' then
  begin
    Inc(NChars);
    Result.a[2] := Byte((DecodeTable[InBuf[2]] shl 6) or DecodeTable[InBuf[3]]);
  end;
end;

class procedure TUtils.EncodeStream(const AInput, AOutput: TStream);
type
  PInteger = ^Integer;
var
  LInBuffer: array[0..BufferSize] of Byte;
  LOutBuffer: array[0..1023] of AnsiChar;
  LBufferPtr: PAnsiChar;
  LI, LJ, BytesRead: Integer;
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

      if LBufferPtr - @LOutBuffer[0] > LineBreakInterval then
        WriteLineBreak;
    end;
    AOutput.Write(LOutBuffer, LBufferPtr - @LOutBuffer[0]);
  until BytesRead = 0;
end;

class procedure TUtils.DecodeStream(const AInput, AOutput: TStream);
var
  LInBuf: array[0..75] of AnsiChar;
  LOutBuf: array[0..60] of Byte;
  LInBufPtr, LOutBufPtr: PAnsiChar;
  LI, LJ, LK, BytesRead: Integer;
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
    LWhiteFound, LEndReached : Boolean;
    LCntRead, LIdx, LIdxEnd: Integer;
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

class function TUtils.EncodeString(const AInput: string): string;
var
  LInStr, LOutStr: TStringStream;
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

class function TUtils.DecodeString(const AInput: string): string;
var
  LInStr, LOutStr: TStringStream;
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

class function TUtils.EncodeBase64(const AInput: Pointer; const ASize: Integer): string;
var
  LInStr: TPointerStream;
  LOutStr: TBytesStream;
begin
  LInStr := TPointerStream.Create(AInput, ASize);
  try
    LOutStr := TBytesStream.Create;
    try
      EncodeStream(LInStr, LOutStr);
      SetString(Result, PAnsiChar(LOutStr.Memory), LOutStr.Size);
    finally
      LOutStr.Free;
    end;
  finally
    LInStr.Free;
  end;
end;

{ TPointerStream }

constructor TPointerStream.Create(P: Pointer; ASize: Integer);
begin
  SetPointer(P, ASize);
end;

function TPointerStream.Write(const Buffer; Count: Longint): Longint;
var
  LPos, LEndPos, LSize: Longint;
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

initialization
  FormatSettings := TFormatSettings.Create('en_US');

end.
