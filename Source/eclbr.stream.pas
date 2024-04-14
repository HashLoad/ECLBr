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

unit eclbr.stream;

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  eclbr.map,
  eclbr.objects,
  eclbr.vector;

type
  TStreamReader = Classes.TStreamReader;
  TStringStream = Classes.TStringStream;

  TStreamReaderListenerEvent = procedure(const Line: String) of object;

  TStreamReaderEx = class
  strict private
    FDataInternal: TStreamReader;
    FDataReader: TStreamReader;
    FDataString: TStringStream;
    FListeners: TList<TStreamReaderListenerEvent>;
    procedure _SetDataInternal;
    procedure _NotifyListeners(const Line: String);
  public
    constructor Create(const Stream: TStream); reintroduce; overload;
    constructor Create(const Stream: TStream; const DetectBOM: Boolean); reintroduce; overload;
    constructor Create(const Stream: TStream; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096); reintroduce; overload;
    constructor Create(const Filename: String); reintroduce; overload;
    constructor Create(const Filename: String; const DetectBOM: Boolean); reintroduce; overload;
    constructor Create(const Filename: String; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096); reintroduce; overload;
    constructor Create; overload;
    destructor Destroy; override;
    class function New(const Stream: TStream): TStreamReaderEx; overload;
    class function New(const Stream: TStream; const DetectBOM: Boolean): TStreamReaderEx; overload;
    class function New(const Stream: TStream; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096): TStreamReaderEx; overload;
    class function New(const Filename: String): TStreamReaderEx; overload;
    class function New(const Filename: String; const DetectBOM: Boolean): TStreamReaderEx; overload;
    class function New(const Filename: String; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096): TStreamReaderEx; overload;
    function BaseStream: TStream;
    function CurrentEncoding: TEncoding;
    function Map(const AFunc: TFunc<String, String>): TStreamReaderEx; overload;
    function Map<TResult>(const AMappingFunc: TFunc<String, TResult>): TVector<TResult>; overload;
    function Filter(const APredicate: TPredicate<String>): TStreamReaderEx;
    function Reduce(const AFunc: TFunc<Integer, String, Integer>;
      const AInitialValue: Integer): Integer;
    function ForEach(const AAction: TProc<String>): TStreamReaderEx;
    function GroupBy(const AKeySelector: TFunc<String, String>): TMap<String, TVector<String>>;
    function Distinct: TStreamReaderEx;
    function Skip(const ACount: Integer): TStreamReaderEx;
    function Sort: TStreamReaderEx;
    function Take(const ACount: Integer): TStreamReaderEx;
    function Concat(const AStreamReader: TStreamReaderEx): TStreamReaderEx;
    function Partition(const APredicate: TPredicate<String>): TPair<TStreamReaderEx, TStreamReaderEx>;
    function Join(const ASeparator: String): String;
    function AsLine: String;
    function AsString: String;
    procedure AddListener(const Listener: TStreamReaderListenerEvent);
    procedure RemoveListener(const Listener: TStreamReaderListenerEvent);
  end;

implementation

{ TStreamReaderHelper }

constructor TStreamReaderEx.Create(const Filename: String);
begin
  Create;
  FDataInternal := TStreamReader.Create(Filename);
end;

destructor TStreamReaderEx.Destroy;
begin
  FDataInternal.Free;
  if Assigned(FListeners) then
    FListeners.Free;
  if Assigned(FDataReader) then
    FDataReader.Free;
  if Assigned(FDataString) then
    FDataString.Free;
  inherited;
end;

function TStreamReaderEx.Filter(const APredicate: TPredicate<String>): TStreamReaderEx;
var
  LResultBuilder: AutoRef<TStringBuilder>;
  LLine: String;
begin
  LResultBuilder := TStringBuilder.Create;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    // Predicate
    if APredicate(LLine) then
    begin
      LResultBuilder.AsRef.AppendLine(LLine);
      // Listeners
      _NotifyListeners(LLine);
    end;
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.AsRef.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Distinct: TStreamReaderEx;
var
  LResultBuilder: AutoRef<TStringBuilder>;
  LUniqueLines: TVector<String>;
  LLine: String;
begin
  LUniqueLines := TVector<String>.Create([]);
  LResultBuilder := TStringBuilder.Create;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    if not LUniqueLines.Contains(LLine) then
    begin
      LUniqueLines.Add(LLine);
      LResultBuilder.AsRef.AppendLine(LLine);
      // Listeners
      _NotifyListeners(LLine);
    end;
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.AsRef.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Map(const AFunc: TFunc<String, String>): TStreamReaderEx;
var
  LResultBuilder: AutoRef<TStringBuilder>;
  LLine: String;
begin
  LResultBuilder := TStringBuilder.Create;
  while not FDataInternal.EndOfStream do
  begin
    LLine := TrimRight(FDataInternal.ReadLine);
    LResultBuilder.AsRef.AppendLine(AFunc(LLine));
    // Listeners
    _NotifyListeners(LLine);
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.AsRef.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Map<TResult>(
  const AMappingFunc: TFunc<String, TResult>): TVector<TResult>;
var
  LLine: String;
begin
  Result := TVector<TResult>.Create([]);
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    Result.Add(AMappingFunc(LLine));
    // Listeners
    _NotifyListeners(LLine);
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

class function TStreamReaderEx.New(const Filename: String;
  const DetectBOM: Boolean): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Filename, DetectBOM);
end;

class function TStreamReaderEx.New(const Filename: String;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: Integer): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Filename, Encoding, DetectBOM, BufferSize);
end;

class function TStreamReaderEx.New(const Filename: String): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Filename);
end;

procedure TStreamReaderEx.AddListener(
  const Listener: TStreamReaderListenerEvent);
begin
  if not Assigned(FListeners) then
    FListeners := TList<TStreamReaderListenerEvent>.Create;
  FListeners.Add(Listener);
end;

function TStreamReaderEx.AsLine: String;
begin
  Result := FDataReader.ReadLine;
end;

function TStreamReaderEx.Reduce(const AFunc: TFunc<Integer, String, Integer>;
  const AInitialValue: Integer): Integer;
var
  LLine: String;
begin
  Result := AInitialValue;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    Result := AFunc(Result, LLine);
    // Listeners
    _NotifyListeners(LLine);
  end;
end;

procedure TStreamReaderEx.RemoveListener(
  const Listener: TStreamReaderListenerEvent);
begin
  if Assigned(FListeners) then
    FListeners.Remove(Listener);
end;

constructor TStreamReaderEx.Create(const Stream: TStream);
begin
  Create;
  FDataInternal := TStreamReader.Create(Stream);
end;

constructor TStreamReaderEx.Create(const Stream: TStream;
  const DetectBOM: Boolean);
begin
  Create;
  FDataInternal := TStreamReader.Create(Stream, DetectBOM);
end;

constructor TStreamReaderEx.Create(const Stream: TStream;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: Integer);
begin
  Create;
  FDataInternal := TStreamReader.Create(Stream, Encoding, DetectBOM, BufferSize);
end;

constructor TStreamReaderEx.Create(const Filename: String;
  const DetectBOM: Boolean);
begin
  Create;
  FDataInternal := TStreamReader.Create(Filename, DetectBOM);
end;

constructor TStreamReaderEx.Create(const Filename: String;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: Integer);
begin
  Create;
  FDataInternal := TStreamReader.Create(Filename, Encoding, DetectBOM, BufferSize);
end;

constructor TStreamReaderEx.Create;
begin
  inherited Create;
end;

function TStreamReaderEx.CurrentEncoding: TEncoding;
begin
  Result := FDataReader.CurrentEncoding;
end;

function TStreamReaderEx.AsString: String;
begin
  Result := FDataString.DataString;
end;

function TStreamReaderEx.BaseStream: TStream;
begin
  Result := FDataReader.BaseStream;
end;

function TStreamReaderEx.ForEach(const AAction: TProc<String>): TStreamReaderEx;
var
  LLine: String;
begin
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    AAction(LLine);
    // Listeners
    _NotifyListeners(LLine);
  end;
  Result := Self;
end;

function TStreamReaderEx.GroupBy(const AKeySelector: TFunc<String, String>): TMap<String, TVector<String>>;
var
  LList: TVector<String>;
  LLine: String;
  LKey: String;
begin
  Result := [];
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := FDataInternal.ReadLine;
      LKey := AKeySelector(LLine);
      if not Result.TryGetValue(LKey, LList) then
      begin
        LList := TVector<String>.Create([]);
        Result.Add(LKey, LList);
      end;
      LList.Add(LLine);
      Result[LKey] := LList;
      // Listeners
      _NotifyListeners(LLine);
    end;
  except
    raise;
  end;
end;

function TStreamReaderEx.Skip(const ACount: Integer): TStreamReaderEx;
var
  LSkippedCount: Integer;
  LResultBuilder: AutoRef<TStringBuilder>;
  LLine: String;
begin
  LResultBuilder := TStringBuilder.Create;
  LSkippedCount := 1;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    if LSkippedCount > ACount then
    begin
      LResultBuilder.AsRef.AppendLine(LLine);
      // Listeners
      _NotifyListeners(LLine);
    end;
    Inc(LSkippedCount);
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.AsRef.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Sort: TStreamReaderEx;
var
  LLines: AutoRef<TStringList>;
begin
  LLines := TStringList.Create;
  while not FDataInternal.EndOfStream do
    LLines.AsRef.Add(FDataInternal.ReadLine);

  LLines.AsRef.Sort;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LLines.AsRef.Text, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Take(const ACount: Integer): TStreamReaderEx;
var
  LResultBuilder: AutoRef<TStringBuilder>;
  LLine: String;
  LLineCount: Integer;
begin
  LResultBuilder := TStringBuilder.Create;
  LLineCount := 0;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    Inc(LLineCount);
    if LLineCount <= ACount then
    begin
      LResultBuilder.AsRef.AppendLine(LLine);
      // Listeners
      _NotifyListeners(LLine);
    end
    else
      break;
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.AsRef.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

procedure TStreamReaderEx._NotifyListeners(const Line: String);
var
  LListener: TStreamReaderListenerEvent;
begin
  if Assigned(FListeners) then
  begin
    for LListener in FListeners do
      LListener(Line);
  end;
end;

procedure TStreamReaderEx._SetDataInternal;
var
  LResultBuilder: AutoRef<TStringBuilder>;
  LLine: String;
begin
  LResultBuilder := TStringBuilder.Create;
  try
    FDataInternal.BaseStream.Seek(0, soBeginning);
    while not FDataInternal.EndOfStream do
    begin
      LLine := TrimRight(FDataInternal.ReadLine);
      LResultBuilder.AsRef.AppendLine(LLine);
    end;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LResultBuilder.AsRef.ToString, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    FDataInternal.Close;
  end;
end;

function TStreamReaderEx.Concat(const AStreamReader: TStreamReaderEx): TStreamReaderEx;
var
  LResultBuilder: AutoRef<TStringBuilder>;
  LLine: String;
begin
  LResultBuilder := TStringBuilder.Create;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    LResultBuilder.AsRef.AppendLine(LLine);
    // Listeners
    _NotifyListeners(LLine);
  end;
  while not AStreamReader.FDataInternal.EndOfStream do
  begin
    LLine := AStreamReader.FDataInternal.ReadLine;
    LResultBuilder.AsRef.AppendLine(LLine);
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.AsRef.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Partition(const APredicate: TPredicate<String>): TPair<TStreamReaderEx, TStreamReaderEx>;
var
  LLeftStream: AutoRef<TStringStream>;
  LRightStream: AutoRef<TStringStream>;
  LLeftStreamReader: TStreamReaderEx;
  LRightStreamReader: TStreamReaderEx;
  LLine: String;
begin
  LLeftStream := TStringStream.Create('', TEncoding.UTF8);
  LRightStream := TStringStream.Create('', TEncoding.UTF8);
  LLeftStreamReader := nil;
  LRightStreamReader := nil;
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := FDataInternal.ReadLine;
      if APredicate(LLine) then
        LLeftStream.AsRef.WriteString(LLine + sLineBreak)
      else
        LRightStream.AsRef.WriteString(LLine + sLineBreak);
      // Listeners
      _NotifyListeners(LLine);
    end;
    LLeftStream.AsRef.DataString;
    LLeftStreamReader := TStreamReaderEx.New(LLeftStream.AsRef);
    LLeftStreamReader._SetDataInternal;

    LRightStream.AsRef.DataString;
    LRightStreamReader := TStreamReaderEx.New(LRightStream.AsRef);
    LRightStreamReader._SetDataInternal;
  except
    if Assigned(LLeftStreamReader) then
      LLeftStreamReader.Free;
    if Assigned(LRightStreamReader) then
      LRightStreamReader.Free;
    raise;
  end;
  Result.Create(LLeftStreamReader, LRightStreamReader);
end;

function TStreamReaderEx.Join(const ASeparator: String): String;
var
  LLine: String;
begin
  Result := '';
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    Result := Result + LLine;
    if not FDataInternal.EndOfStream then
      Result := Result + ASeparator;
    // Listeners
    _NotifyListeners(LLine);
  end;
end;

end.

