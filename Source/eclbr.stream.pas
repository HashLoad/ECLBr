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

unit eclbr.stream;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  eclbr.map,
  eclbr.vector;

type
  TStreamReader = Classes.TStreamReader;
  TStringStream = Classes.TStringStream;

  TStreamReaderEx = class
  private
    FDataInternal: TStreamReader;
    FDataReader: TStreamReader;
    FDataString: TStringStream;
  protected
    procedure _SetDataInternal;
  public
    constructor Create(const Stream: TStream); reintroduce; overload;
    constructor Create(const Stream: TStream; const DetectBOM: Boolean); reintroduce; overload;
    constructor Create(const Stream: TStream; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096); reintroduce; overload;
    constructor Create(const Filename: string); reintroduce; overload;
    constructor Create(const Filename: string; const DetectBOM: Boolean); reintroduce; overload;
    constructor Create(const Filename: string; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096); reintroduce; overload;
    destructor Destroy; override;
    class function New(const Stream: TStream): TStreamReaderEx; overload;
    class function New(const Stream: TStream; const DetectBOM: Boolean): TStreamReaderEx; overload;
    class function New(const Stream: TStream; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096): TStreamReaderEx; overload;
    class function New(const Filename: string): TStreamReaderEx; overload;
    class function New(const Filename: string; const DetectBOM: Boolean): TStreamReaderEx; overload;
    class function New(const Filename: string; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096): TStreamReaderEx; overload;
    function BaseStream: TStream;
    function CurrentEncoding: TEncoding;
    function Map(const AFunc: TFunc<string, string>): TStreamReaderEx; overload;
    function Map<TResult>(const AMappingFunc: TFunc<string, TResult>): TVector<TResult>; overload;
    function Filter(const APredicate: TPredicate<string>): TStreamReaderEx;
    function Reduce(const AFunc: TFunc<integer, string, integer>;
      const AInitialValue: integer): integer;
    function ForEach(const AAction: TProc<string>): TStreamReaderEx;
    function GroupBy(const AKeySelector: TFunc<string, string>): TMap<string, TVector<string>>;
    function Distinct: TStreamReaderEx;
    function Skip(const ACount: Integer): TStreamReaderEx;
    function Sort: TStreamReaderEx;
    function Take(const ACount: Integer): TStreamReaderEx;
    function Concat(const AStreamReader: TStreamReaderEx): TStreamReaderEx;
    function Partition(const APredicate: TPredicate<string>): TPair<TStreamReaderEx, TStreamReaderEx>;
    function Join(const ASeparator: string): string;
    function AsLine: string;
    function AsString: string;
  end;

implementation

{ TStreamReaderHelper }

constructor TStreamReaderEx.Create(const Filename: string);
begin
  FDataInternal := TStreamReader.Create(Filename);
end;

destructor TStreamReaderEx.Destroy;
begin
  FDataInternal.Free;
  if Assigned(FDataReader) then
    FDataReader.Free;
  if Assigned(FDataString) then
    FDataString.Free;
  inherited;
end;

function TStreamReaderEx.Filter(const APredicate: TPredicate<string>): TStreamReaderEx;
var
  LResultBuilder: TStringBuilder;
  LLine: string;
begin
  LResultBuilder := TStringBuilder.Create;
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := FDataInternal.ReadLine;
      if APredicate(LLine) then
        LResultBuilder.AppendLine(LLine);
    end;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LResultBuilder.ToString, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    LResultBuilder.Free;
  end;
  Result := Self;
end;

function TStreamReaderEx.Distinct: TStreamReaderEx;
var
  LUniqueLines: TVector<string>;
  LResultBuilder: TStringBuilder;
  LLine: string;
begin
  LUniqueLines := TVector<string>.Create([]);
  LResultBuilder := TStringBuilder.Create;
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := FDataInternal.ReadLine;
      if not LUniqueLines.Contains(LLine) then
      begin
        LUniqueLines.Add(LLine);
        LResultBuilder.AppendLine(LLine);
      end;
    end;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LResultBuilder.ToString, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    LResultBuilder.Free;
  end;
  Result := Self;
end;

function TStreamReaderEx.Map(const AFunc: TFunc<string, string>): TStreamReaderEx;
var
  LResultBuilder: TStringBuilder;
  LLine: string;
begin
  LResultBuilder := TStringBuilder.Create;
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := TrimRight(FDataInternal.ReadLine);
      LResultBuilder.AppendLine(AFunc(LLine));
    end;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LResultBuilder.ToString, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    LResultBuilder.Free;
  end;
  Result := Self;
end;

function TStreamReaderEx.Map<TResult>(
  const AMappingFunc: TFunc<string, TResult>): TVector<TResult>;
var
  LItem: string;
begin
  Result := TVector<TResult>.Create([]);
  while not FDataInternal.EndOfStream do
  begin
    LItem := FDataInternal.ReadLine;
    Result.Add(AMappingFunc(LItem));
  end;
end;

class function TStreamReaderEx.New(const Stream: TStream): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Stream);
end;

class function TStreamReaderEx.New(const Stream: TStream;
  const DetectBOM: Boolean): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Stream, DetectBOM);
end;

class function TStreamReaderEx.New(const Stream: TStream; const Encoding: TEncoding;
  const DetectBOM: Boolean; const BufferSize: Integer): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Stream, Encoding, DetectBOM, BufferSize);
end;

class function TStreamReaderEx.New(const Filename: string;
  const DetectBOM: Boolean): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Filename, DetectBOM);
end;

class function TStreamReaderEx.New(const Filename: string;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: Integer): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Filename, Encoding, DetectBOM, BufferSize);
end;

class function TStreamReaderEx.New(const Filename: string): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Filename);
end;

function TStreamReaderEx.AsLine: string;
begin
  Result := FDataReader.ReadLine;
end;

function TStreamReaderEx.Reduce(const AFunc: TFunc<Integer, string, Integer>;
  const AInitialValue: Integer): Integer;
var
  LLine: string;
begin
  Result := AInitialValue;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    Result := AFunc(Result, LLine);
  end;
end;

constructor TStreamReaderEx.Create(const Stream: TStream);
begin
  FDataInternal := TStreamReader.Create(Stream);
//  FDataString := TStringStream.Create('', TEncoding.UTF8);
//  FDataString.CopyFrom(Stream, Stream.Size);
end;

constructor TStreamReaderEx.Create(const Stream: TStream;
  const DetectBOM: Boolean);
begin
  FDataInternal := TStreamReader.Create(Stream, DetectBOM);
//  FDataString := TStringStream.Create('', TEncoding.UTF8);
//  FDataString.CopyFrom(Stream, Stream.Size);
end;

constructor TStreamReaderEx.Create(const Stream: TStream;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: Integer);
begin
  FDataInternal := TStreamReader.Create(Stream, Encoding, DetectBOM, BufferSize);
//  FDataString := TStringStream.Create('', TEncoding.UTF8);
//  FDataString.CopyFrom(Stream, Stream.Size);
end;

constructor TStreamReaderEx.Create(const Filename: string;
  const DetectBOM: Boolean);
begin
  FDataInternal := TStreamReader.Create(Filename, DetectBOM);
end;

constructor TStreamReaderEx.Create(const Filename: string;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: Integer);
begin
  FDataInternal := TStreamReader.Create(Filename, Encoding, DetectBOM, BufferSize);
end;

function TStreamReaderEx.CurrentEncoding: TEncoding;
begin
  Result := FDataReader.CurrentEncoding;
end;

function TStreamReaderEx.AsString: string;
begin
  Result := FDataString.DataString;
end;

function TStreamReaderEx.BaseStream: TStream;
begin
  Result := FDataReader.BaseStream;
end;

function TStreamReaderEx.ForEach(const AAction: TProc<string>): TStreamReaderEx;
var
  LLine: string;
begin
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    AAction(LLine);
  end;
  Result := Self;
end;

function TStreamReaderEx.GroupBy(const AKeySelector: TFunc<string, string>): TMap<string, TVector<string>>;
var
  LLine: string;
  LList: TVector<string>;
  LKey: string;
begin
  Result := [];
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := FDataInternal.ReadLine;
      LKey := AKeySelector(LLine);
      if not Result.TryGetValue(LKey, LList) then
      begin
        LList := TVector<string>.Create([]);
        Result.Add(LKey, LList)
      end;
      LList.Add(LLine);
      Result[LKey] := LList;
    end;
  except
    raise;
  end;
end;

function TStreamReaderEx.Skip(const ACount: Integer): TStreamReaderEx;
var
  LSkippedCount: Integer;
  LResultBuilder: TStringBuilder;
  LLine: string;
begin
  LResultBuilder := TStringBuilder.Create;
  LSkippedCount := 1;
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := FDataInternal.ReadLine;
      if LSkippedCount > ACount then
        LResultBuilder.AppendLine(LLine);
      Inc(LSkippedCount);
    end;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LResultBuilder.ToString, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    LResultBuilder.Free;
  end;
  Result := Self;
end;

function TStreamReaderEx.Sort: TStreamReaderEx;
var
  LLines: TStringList;
begin
  LLines := TStringList.Create;
  try
    while not FDataInternal.EndOfStream do
      LLines.Add(FDataInternal.ReadLine);

    LLines.Sort;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LLines.Text, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    LLines.Free;
  end;
  Result := Self;
end;

function TStreamReaderEx.Take(const ACount: Integer): TStreamReaderEx;
var
  LResultBuilder: TStringBuilder;
  LLine: string;
  LLineCount: Integer;
begin
  LResultBuilder := TStringBuilder.Create;
  LLineCount := 0;
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := FDataInternal.ReadLine;
      Inc(LLineCount);
      if LLineCount <= ACount then
        LResultBuilder.AppendLine(LLine)
      else
        break;
    end;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LResultBuilder.ToString, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    LResultBuilder.Free;
  end;
  Result := Self;
end;

procedure TStreamReaderEx._SetDataInternal;
var
  LResultBuilder: TStringBuilder;
  LLine: string;
begin
  LResultBuilder := TStringBuilder.Create;
  try
    FDataInternal.BaseStream.Seek(0, soBeginning);
    while not FDataInternal.EndOfStream do
    begin
      LLine := TrimRight(FDataInternal.ReadLine);
      LResultBuilder.AppendLine(LLine);
    end;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LResultBuilder.ToString, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    LResultBuilder.Free;
    FDataInternal.Close;
  end;
end;

function TStreamReaderEx.Concat(const AStreamReader: TStreamReaderEx): TStreamReaderEx;
var
  LResultBuilder: TStringBuilder;
  LLine: string;
begin
  LResultBuilder := TStringBuilder.Create;
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := FDataInternal.ReadLine;
      LResultBuilder.AppendLine(LLine);
    end;
    while not AStreamReader.FDataInternal.EndOfStream do
    begin
      LLine := AStreamReader.FDataInternal.ReadLine;
      LResultBuilder.AppendLine(LLine);
    end;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LResultBuilder.ToString, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    LResultBuilder.Free;
  end;
  Result := Self;
end;

function TStreamReaderEx.Partition(const APredicate: TPredicate<string>): TPair<TStreamReaderEx, TStreamReaderEx>;
var
  LLeftStream: TStringStream;
  LRightStream: TStringStream;
  LLeftStreamReader: TStreamReaderEx;
  LRightStreamReader: TStreamReaderEx;
  LLine: string;
begin
  LLeftStream := TStringStream.Create('', TEncoding.UTF8);
  LRightStream := TStringStream.Create('', TEncoding.UTF8);
  try
    try
      while not FDataInternal.EndOfStream do
      begin
        LLine := FDataInternal.ReadLine;
        if APredicate(LLine) then
          LLeftStream.WriteString(LLine + sLineBreak)
        else
          LRightStream.WriteString(LLine + sLineBreak);
      end;
      LLeftStream.DataString;
      LLeftStreamReader := TStreamReaderEx.New(LLeftStream);
      LLeftStreamReader._SetDataInternal;

      LRightStream.DataString;
      LRightStreamReader := TStreamReaderEx.New(LRightStream);
      LRightStreamReader._SetDataInternal;
    except
      if Assigned(LLeftStreamReader) then
        LLeftStreamReader.Free;
      if Assigned(LRightStreamReader) then
        LRightStreamReader.Free;
      raise;
    end;
    Result.Create(LLeftStreamReader, LRightStreamReader);
  finally
    LLeftStream.Free;
    LRightStream.Free;
  end;
end;

function TStreamReaderEx.Join(const ASeparator: string): string;
var
  LLine: string;
begin
  Result := '';
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    Result := Result + LLine;
    if not FDataInternal.EndOfStream then
      Result := Result + ASeparator;
  end;
end;

end.

