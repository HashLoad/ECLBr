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
  @Discord(https://discord.gg/S5yvvGu7)
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

  TStreamReaderListenerEvent = procedure(const Line: string) of object;

  TStreamReaderEx = class
  private
    FDataInternal: TStreamReader;
    FDataReader: TStreamReader;
    FDataString: TStringStream;
    FListeners: TList<TStreamReaderListenerEvent>;
    procedure _SetDataInternal;
    procedure _NotifyListeners(const Line: string);
  public
    constructor Create(const Stream: TStream); reintroduce; overload;
    constructor Create(const Stream: TStream; const DetectBOM: Boolean); reintroduce; overload;
    constructor Create(const Stream: TStream; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: integer = 4096); reintroduce; overload;
    constructor Create(const Filename: string); reintroduce; overload;
    constructor Create(const Filename: string; const DetectBOM: Boolean); reintroduce; overload;
    constructor Create(const Filename: string; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: integer = 4096); reintroduce; overload;
    constructor Create; overload;
    destructor Destroy; override;
    class function New(const Stream: TStream): TStreamReaderEx; overload;
    class function New(const Stream: TStream; const DetectBOM: Boolean): TStreamReaderEx; overload;
    class function New(const Stream: TStream; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: integer = 4096): TStreamReaderEx; overload;
    class function New(const Filename: string): TStreamReaderEx; overload;
    class function New(const Filename: string; const DetectBOM: Boolean): TStreamReaderEx; overload;
    class function New(const Filename: string; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: integer = 4096): TStreamReaderEx; overload;
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
    function Skip(const ACount: integer): TStreamReaderEx;
    function Sort: TStreamReaderEx;
    function Take(const ACount: integer): TStreamReaderEx;
    function Concat(const AStreamReader: TStreamReaderEx): TStreamReaderEx;
    function Partition(const APredicate: TPredicate<string>): TPair<TStreamReaderEx, TStreamReaderEx>;
    function Join(const ASeparator: string): string;
    function AsLine: string;
    function AsString: string;
    procedure AddListener(const Listener: TStreamReaderListenerEvent);
    procedure RemoveListener(const Listener: TStreamReaderListenerEvent);
  end;

implementation

{ TStreamReaderHelper }

constructor TStreamReaderEx.Create(const Filename: string);
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

function TStreamReaderEx.Filter(const APredicate: TPredicate<string>): TStreamReaderEx;
var
  LResultBuilder: IAutoRef<TStringBuilder>;
  LLine: string;
begin
  LResultBuilder := TAutoRef<TStringBuilder>.New;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    // Predicate
    if APredicate(LLine) then
    begin
      LResultBuilder.Get.AppendLine(LLine);
      // Listeners
      _NotifyListeners(LLine);
    end;
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.Get.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Distinct: TStreamReaderEx;
var
  LResultBuilder: IAutoRef<TStringBuilder>;
  LUniqueLines: TVector<string>;
  LLine: string;
begin
  LUniqueLines := TVector<string>.Create([]);
  LResultBuilder := TAutoRef<TStringBuilder>.New;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    if not LUniqueLines.Contains(LLine) then
    begin
      LUniqueLines.Add(LLine);
      LResultBuilder.Get.AppendLine(LLine);
      // Listeners
      _NotifyListeners(LLine);
    end;
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.Get.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Map(const AFunc: TFunc<string, string>): TStreamReaderEx;
var
  LResultBuilder: IAutoRef<TStringBuilder>;
  LLine: string;
begin
  LResultBuilder := TAutoRef<TStringBuilder>.New;
  while not FDataInternal.EndOfStream do
  begin
    LLine := TrimRight(FDataInternal.ReadLine);
    LResultBuilder.Get.AppendLine(AFunc(LLine));
    // Listeners
    _NotifyListeners(LLine);
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.Get.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Map<TResult>(
  const AMappingFunc: TFunc<string, TResult>): TVector<TResult>;
var
  LLine: string;
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
  const DetectBOM: Boolean; const BufferSize: integer): TStreamReaderEx;
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
  const BufferSize: integer): TStreamReaderEx;
begin
  Result := TStreamReaderEx.Create(Filename, Encoding, DetectBOM, BufferSize);
end;

class function TStreamReaderEx.New(const Filename: string): TStreamReaderEx;
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

function TStreamReaderEx.AsLine: string;
begin
  Result := FDataReader.ReadLine;
end;

function TStreamReaderEx.Reduce(const AFunc: TFunc<integer, string, integer>;
  const AInitialValue: integer): integer;
var
  LLine: string;
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
  const BufferSize: integer);
begin
  Create;
  FDataInternal := TStreamReader.Create(Stream, Encoding, DetectBOM, BufferSize);
end;

constructor TStreamReaderEx.Create(const Filename: string;
  const DetectBOM: Boolean);
begin
  Create;
  FDataInternal := TStreamReader.Create(Filename, DetectBOM);
end;

constructor TStreamReaderEx.Create(const Filename: string;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: integer);
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
    // Listeners
    _NotifyListeners(LLine);
  end;
  Result := Self;
end;

function TStreamReaderEx.GroupBy(const AKeySelector: TFunc<string, string>): TMap<string, TVector<string>>;
var
  LList: TVector<string>;
  LLine: string;
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

function TStreamReaderEx.Skip(const ACount: integer): TStreamReaderEx;
var
  LSkippedCount: integer;
  LResultBuilder: IAutoRef<TStringBuilder>;
  LLine: string;
begin
  LResultBuilder := TAutoRef<TStringBuilder>.New;
  LSkippedCount := 1;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    if LSkippedCount > ACount then
    begin
      LResultBuilder.Get.AppendLine(LLine);
      // Listeners
      _NotifyListeners(LLine);
    end;
    Inc(LSkippedCount);
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.Get.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Sort: TStreamReaderEx;
var
  LLines: IAutoRef<TStringList>;
begin
  LLines := TAutoRef<TStringList>.New;
  while not FDataInternal.EndOfStream do
    LLines.Get.Add(FDataInternal.ReadLine);

  LLines.Get.Sort;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LLines.Get.Text, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Take(const ACount: integer): TStreamReaderEx;
var
  LResultBuilder: IAutoRef<TStringBuilder>;
  LLine: string;
  LLineCount: integer;
begin
  LResultBuilder := TAutoRef<TStringBuilder>.New;
  LLineCount := 0;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    Inc(LLineCount);
    if LLineCount <= ACount then
    begin
      LResultBuilder.Get.AppendLine(LLine);
      // Listeners
      _NotifyListeners(LLine);
    end
    else
      break;
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.Get.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

procedure TStreamReaderEx._NotifyListeners(const Line: string);
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
  LResultBuilder: IAutoRef<TStringBuilder>;
  LLine: string;
begin
  LResultBuilder := TAutoRef<TStringBuilder>.New;
  try
    FDataInternal.BaseStream.Seek(0, soBeginning);
    while not FDataInternal.EndOfStream do
    begin
      LLine := TrimRight(FDataInternal.ReadLine);
      LResultBuilder.Get.AppendLine(LLine);
    end;
    if Assigned(FDataString) then
      FDataString.Free;
    FDataString := TStringStream.Create(LResultBuilder.Get.ToString, TEncoding.UTF8);

    if Assigned(FDataReader) then
      FDataReader.Free;
    FDataReader := TStreamReader.Create(FDataString);
  finally
    FDataInternal.Close;
  end;
end;

function TStreamReaderEx.Concat(const AStreamReader: TStreamReaderEx): TStreamReaderEx;
var
  LResultBuilder: IAutoRef<TStringBuilder>;
  LLine: string;
begin
  LResultBuilder := TAutoRef<TStringBuilder>.New;
  while not FDataInternal.EndOfStream do
  begin
    LLine := FDataInternal.ReadLine;
    LResultBuilder.Get.AppendLine(LLine);
    // Listeners
    _NotifyListeners(LLine);
  end;
  while not AStreamReader.FDataInternal.EndOfStream do
  begin
    LLine := AStreamReader.FDataInternal.ReadLine;
    LResultBuilder.Get.AppendLine(LLine);
  end;
  if Assigned(FDataString) then
    FDataString.Free;
  FDataString := TStringStream.Create(LResultBuilder.Get.ToString, TEncoding.UTF8);

  if Assigned(FDataReader) then
    FDataReader.Free;
  FDataReader := TStreamReader.Create(FDataString);
  Result := Self;
end;

function TStreamReaderEx.Partition(const APredicate: TPredicate<string>): TPair<TStreamReaderEx, TStreamReaderEx>;
var
  LLeftStream: IAutoRef<TStringStream>;
  LRightStream: IAutoRef<TStringStream>;
  LLeftStreamReader: TStreamReaderEx;
  LRightStreamReader: TStreamReaderEx;
  LLine: string;
begin
  LLeftStream := TAutoRef<TStringStream>.New(TStringStream.Create('', TEncoding.UTF8));
  LRightStream := TAutoRef<TStringStream>.New(TStringStream.Create('', TEncoding.UTF8));
  LLeftStreamReader := nil;
  LRightStreamReader := nil;
  try
    while not FDataInternal.EndOfStream do
    begin
      LLine := FDataInternal.ReadLine;
      if APredicate(LLine) then
        LLeftStream.Get.WriteString(LLine + sLineBreak)
      else
        LRightStream.Get.WriteString(LLine + sLineBreak);
      // Listeners
      _NotifyListeners(LLine);
    end;
    LLeftStream.Get.DataString;
    LLeftStreamReader := TStreamReaderEx.New(LLeftStream.Get);
    LLeftStreamReader._SetDataInternal;

    LRightStream.Get.DataString;
    LRightStreamReader := TStreamReaderEx.New(LRightStream.Get);
    LRightStreamReader._SetDataInternal;
  except
    if Assigned(LLeftStreamReader) then LLeftStreamReader.Free;
    if Assigned(LRightStreamReader) then LRightStreamReader.Free;
    raise;
  end;
  Result.Create(LLeftStreamReader, LRightStreamReader);
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
    // Listeners
    _NotifyListeners(LLine);
  end;
end;

end.
