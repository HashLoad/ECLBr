unit eclbr.stream;

interface

uses
  Generics.Collections,
  Classes,
  SysUtils,
  eclbr.map,
  eclbr.vector;

type
  TStreamReader = Classes.TStreamReader;
  TStringStream = Classes.TStringStream;

  TStreamReaderHelper = class
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
    class function New(const Stream: TStream): TStreamReaderHelper; overload;
    class function New(const Stream: TStream; const DetectBOM: Boolean): TStreamReaderHelper; overload;
    class function New(const Stream: TStream; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096): TStreamReaderHelper; overload;
    class function New(const Filename: string): TStreamReaderHelper; overload;
    class function New(const Filename: string; const DetectBOM: Boolean): TStreamReaderHelper; overload;
    class function New(const Filename: string; const Encoding: TEncoding;
      const DetectBOM: Boolean = False; const BufferSize: Integer = 4096): TStreamReaderHelper; overload;
    function Map(const AFunc: TFunc<string, string>): TStreamReaderHelper;
    function Filter(const APredicate: TPredicate<string>): TStreamReaderHelper;
    function Reduce(const AFunc: TFunc<integer, string, integer>;
      const AInitialValue: integer): integer;
    function ForEach(const AAction: TProc<string>): TStreamReaderHelper;
    function GroupBy(const AKeySelector: TFunc<string, string>): TMap<string, TVector<string>>;
    function Distinct: TStreamReaderHelper;
    function Skip(const ACount: Integer): TStreamReaderHelper;
    function Sort: TStreamReaderHelper;
    function Take(const ACount: Integer): TStreamReaderHelper;
    function Concat(const AStreamReader: TStreamReaderHelper): TStreamReaderHelper;
    function Partition(const APredicate: TPredicate<string>): TPair<TStreamReaderHelper, TStreamReaderHelper>;
    function Join(const ASeparator: string): string;
    function AsLine: string;
    function AsString: string;
  end;

implementation

{ TStreamReaderHelper }

constructor TStreamReaderHelper.Create(const Filename: string);
begin
  FDataInternal := TStreamReader.Create(Filename);
end;

destructor TStreamReaderHelper.Destroy;
begin
  FDataInternal.Free;
  if Assigned(FDataReader) then
    FDataReader.Free;
  if Assigned(FDataString) then
    FDataString.Free;
  inherited;
end;

function TStreamReaderHelper.Filter(const APredicate: TPredicate<string>): TStreamReaderHelper;
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

function TStreamReaderHelper.Distinct: TStreamReaderHelper;
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

function TStreamReaderHelper.Map(const AFunc: TFunc<string, string>): TStreamReaderHelper;
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

class function TStreamReaderHelper.New(const Stream: TStream): TStreamReaderHelper;
begin
  Result := TStreamReaderHelper.Create(Stream);
end;

class function TStreamReaderHelper.New(const Stream: TStream;
  const DetectBOM: Boolean): TStreamReaderHelper;
begin
  Result := TStreamReaderHelper.Create(Stream, DetectBOM);
end;

class function TStreamReaderHelper.New(const Stream: TStream; const Encoding: TEncoding;
  const DetectBOM: Boolean; const BufferSize: Integer): TStreamReaderHelper;
begin
  Result := TStreamReaderHelper.Create(Stream, Encoding, DetectBOM, BufferSize);
end;

class function TStreamReaderHelper.New(const Filename: string;
  const DetectBOM: Boolean): TStreamReaderHelper;
begin
  Result := TStreamReaderHelper.Create(Filename, DetectBOM);
end;

class function TStreamReaderHelper.New(const Filename: string;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: Integer): TStreamReaderHelper;
begin
  Result := TStreamReaderHelper.Create(Filename, Encoding, DetectBOM, BufferSize);
end;

class function TStreamReaderHelper.New(const Filename: string): TStreamReaderHelper;
begin
  Result := TStreamReaderHelper.Create(Filename);
end;

function TStreamReaderHelper.AsLine: string;
begin
  Result := FDataReader.ReadLine;
end;

function TStreamReaderHelper.Reduce(const AFunc: TFunc<Integer, string, Integer>;
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

constructor TStreamReaderHelper.Create(const Stream: TStream);
begin
  FDataInternal := TStreamReader.Create(Stream);
//  FDataString := TStringStream.Create('', TEncoding.UTF8);
//  FDataString.CopyFrom(Stream, Stream.Size);
end;

constructor TStreamReaderHelper.Create(const Stream: TStream;
  const DetectBOM: Boolean);
begin
  FDataInternal := TStreamReader.Create(Stream, DetectBOM);
//  FDataString := TStringStream.Create('', TEncoding.UTF8);
//  FDataString.CopyFrom(Stream, Stream.Size);
end;

constructor TStreamReaderHelper.Create(const Stream: TStream;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: Integer);
begin
  FDataInternal := TStreamReader.Create(Stream, Encoding, DetectBOM, BufferSize);
//  FDataString := TStringStream.Create('', TEncoding.UTF8);
//  FDataString.CopyFrom(Stream, Stream.Size);
end;

constructor TStreamReaderHelper.Create(const Filename: string;
  const DetectBOM: Boolean);
begin
  FDataInternal := TStreamReader.Create(Filename, DetectBOM);
end;

constructor TStreamReaderHelper.Create(const Filename: string;
  const Encoding: TEncoding; const DetectBOM: Boolean;
  const BufferSize: Integer);
begin
  FDataInternal := TStreamReader.Create(Filename, Encoding, DetectBOM, BufferSize);
end;

function TStreamReaderHelper.AsString: string;
begin
  Result := FDataString.DataString;
end;

function TStreamReaderHelper.ForEach(const AAction: TProc<string>): TStreamReaderHelper;
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

function TStreamReaderHelper.GroupBy(const AKeySelector: TFunc<string, string>): TMap<string, TVector<string>>;
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

function TStreamReaderHelper.Skip(const ACount: Integer): TStreamReaderHelper;
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

function TStreamReaderHelper.Sort: TStreamReaderHelper;
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

function TStreamReaderHelper.Take(const ACount: Integer): TStreamReaderHelper;
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

procedure TStreamReaderHelper._SetDataInternal;
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

function TStreamReaderHelper.Concat(const AStreamReader: TStreamReaderHelper): TStreamReaderHelper;
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

function TStreamReaderHelper.Partition(const APredicate: TPredicate<string>): TPair<TStreamReaderHelper, TStreamReaderHelper>;
var
  LLeftStream: TStringStream;
  LRightStream: TStringStream;
  LLeftStreamReader: TStreamReaderHelper;
  LRightStreamReader: TStreamReaderHelper;
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
      LRightStream.DataString;
      LLeftStreamReader := TStreamReaderHelper.New(LLeftStream);
      LLeftStreamReader._SetDataInternal;
      LRightStreamReader := TStreamReaderHelper.New(LRightStream);
      LRightStreamReader._SetDataInternal;
    except
      LLeftStreamReader.Free;
      LRightStreamReader.Free;
      raise;
    end;
    Result.Create(LLeftStreamReader, LRightStreamReader);
  finally
    LLeftStream.Free;
    LRightStream.Free;
  end;
end;

function TStreamReaderHelper.Join(const ASeparator: string): string;
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
