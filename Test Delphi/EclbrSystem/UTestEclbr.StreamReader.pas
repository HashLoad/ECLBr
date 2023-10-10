unit UTestEclbr.StreamReader;

interface

uses
  DUnitX.TestFramework,
  Generics.Collections,
  SysUtils,
  Classes,
  eclbr.map,
  eclbr.vector,
  eclbr.stream;

type
  TPerson = class
  private
    FName: string;
    FAge: Integer;
    FDate: TDateTime;
  public
    constructor Create(const AName: string; const AAge: Integer; const ADate: TDateTime);
    function GetName: string;
    function GetAge: Integer;
    function GetDate: TDateTime;
  end;

  [TestFixture]
  TestStreamReader = class
  private
    FStreamReader: TStreamReaderEx;
    FSampleFile: TStringStream;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestMapLines;
    [Test]
    procedure TestFilterByGender;
    [Test]
    procedure TestReduce;
    [Test]
    procedure TestForEach;
    [Test]
    procedure TestGroupBy;
    [Test]
    procedure TestDistinct;
    [Test]
    procedure TestSkip;
    [Test]
    procedure TestSort;
    [Test]
    procedure TestTake;
    [Test]
    procedure TestConcat;
    [Test]
    procedure TestPartition;
    [Test]
    procedure TestMapListObject;
  end;

implementation

{ TestTIfThen }

procedure TestStreamReader.Setup;
begin
  FSampleFile := TStringStream.Create(
    'This is a sample text file.' + sLineBreak +
    'It contains multiple lines.' + sLineBreak +
    'Each line has some text.'
  );
  FStreamReader := TStreamReaderEx.New(FSampleFile);
end;

procedure TestStreamReader.TearDown;
begin
  FStreamReader.Free;
  FSampleFile.Free;
end;

procedure TestStreamReader.TestDistinct;
var
  LDistinctStream: TStreamReaderEx;
begin
  LDistinctStream := FStreamReader.Distinct;
  try
    // Realize as asserções para verificar se o resultado é o esperado
    Assert.AreEqual(
      'This is a sample text file.' + sLineBreak +
      'It contains multiple lines.' + sLineBreak +
      'Each line has some text.' + sLineBreak,
      LDistinctStream.AsString,
      'Distinct should return distinct lines'
    );
  finally
    LDistinctStream := nil;
  end;
end;

procedure TestStreamReader.TestFilterByGender;
var
  LDataStream: TStringStream;
  LReader: TStreamReaderEx;
begin
  // Crie uma sequência de dados com informações de nome e sexo
  LDataStream := TStringStream.Create(
    'nome 1 masculino'#13#10 +
    'nome 2 feminino'#13#10 +
    'nome 3 masculino'#13#10 +
    'nome 4 feminino'#13#10 +
    'nome 5 masculino'#13#10 +
    'nome 6 feminino'#13#10 +
    'nome 7 masculino'#13#10 +
    'nome 8 feminino'#13#10
  );

  // Crie um leitor de fluxo a partir dos dados
  LReader := TStreamReaderEx.New(LDataStream)
                                .Filter(function(Line: string): Boolean
                                        begin
                                          Result := Pos('feminino', Line) > 0;
                                        end
                                      );

  // Verifique se as linhas filtradas estão corretas
  Assert.AreEqual(
    'nome 2 feminino'#13#10 +
    'nome 4 feminino'#13#10 +
    'nome 6 feminino'#13#10 +
    'nome 8 feminino'#13#10,
    LReader.AsString
  );

  // Libere os recursos
  LReader.Free;
  LDataStream.Free;
end;

procedure TestStreamReader.TestForEach;
var
  LLines: TStringList;
begin
  LLines := TStringList.Create;
  try
    FStreamReader.ForEach(
      procedure(Line: string)
      begin
        LLines.Add(Line);
      end
    );

    Assert.AreEqual(3, LLines.Count);
    Assert.AreEqual('This is a sample text file.', LLines[0]);
    Assert.AreEqual('It contains multiple lines.', LLines[1]);
    Assert.AreEqual('Each line has some text.', LLines[2]);
  finally
    LLines.Free;
  end;
end;

procedure TestStreamReader.TestGroupBy;
var
  LSampleFile: TStringStream;
  LStreamReader: TStreamReaderEx;
  LGroups: TMap<string, TVector<string>>;
begin
  LSampleFile := TStringStream.Create(
    'Apple' + sLineBreak +
    'Banana' + sLineBreak +
    'Cherry' + sLineBreak +
    'Banana' + sLineBreak +
    'Date' + sLineBreak +
    'Apple'
  );
  LStreamReader := TStreamReaderEx.New(LSampleFile);
  try
    LGroups := LStreamReader.GroupBy(
      function(Line: string): string
      begin
        if Line <> '' then
          Result := Line[1]
        else
          Result := 'Outros';
      end
    );

    Assert.AreEqual(4, LGroups.Count); // Espera-se 3 grupos (A, B, D)
    Assert.AreEqual(2, LGroups['A'].Count); // Espera-se 2 palavras começadas com A
    Assert.AreEqual(2, LGroups['B'].Count); // Espera-se 2 palavras começadas com B
    Assert.AreEqual(1, LGroups['D'].Count); // Espera-se 1 palavra começada com D
    Assert.AreEqual(0, LGroups['Outros'].Count); // Espera-se 0 palavra em "Outros"
  finally
    LStreamReader.Free;
    LSampleFile.Free;
  end;
end;

procedure TestStreamReader.TestMapLines;
var
  LReader: TStreamReaderEx;
begin
  // streams.txt
  // linha 1 de teste
  // linha 2 de teste
  // linha 3 de teste

  LReader := TStreamReaderEx.New('streams.txt')
                            .Map(function(Line: string): string
                                 begin
                                   Result := UpperCase(Line);
                                 end);
  try
    Assert.AreEqual('LINHA 1 DE TESTE', LReader.AsLine);
    Assert.AreEqual('LINHA 2 DE TESTE', LReader.AsLine);
  finally
    LReader.Free;
  end;
end;

procedure TestStreamReader.TestMapListObject;
var
  LSampleFile: TStringStream;
  LStreamReader: TStreamReaderEx;
  LPersons: TList<TPerson>;
  LPerson: TPerson;
begin
  LSampleFile := TStringStream.Create('Name 1;15;09-11-2023' + sLineBreak +
                                      'Name 2;22;09-11-2023' + sLineBreak +
                                      'Name 3;33;09-11-2023' + sLineBreak +
                                      'Name 4;44;09-11-2023' + sLineBreak);
  LStreamReader := TStreamReaderEx.New(LSampleFile);
  try
    LPersons := LStreamReader.Map<TVector<string>>(function(Line: string): TVector<string>
                                                   begin
                                                     Result := TVector<string>.Create(Line.Split([';']))
                                                   end)
                             .Map<TPerson>(function(Data: TVector<string>): TPerson
                                                   begin
                                                     Result := TPerson.Create(Data[0],
                                                                              StrToIntDef(Data[1], 0),
                                                                              StrToDateDef(Data[2], Date))
                                                   end)
                             .AsList;

    Assert.IsTrue(LPersons.Count = 4);

    Assert.AreEqual(15, LPersons[0].GetAge);
    Assert.AreEqual(44, LPersons[3].GetAge);

    Assert.AreEqual('Name 2', LPersons[1].GetName);
  finally
    LStreamReader.Free;
    LSampleFile.Free;
    for LPerson in LPersons do
      LPerson.Free;
    LPersons.Free;
  end;
end;

procedure TestStreamReader.TestPartition;
var
  LSampleFile: TStringStream;
  LStreamReader: TStreamReaderEx;
  LLeftStreamReader, LRightStreamReader: TStreamReaderEx;
  LPartitionResult: TPair<TStreamReaderEx, TStreamReaderEx>;
begin
  // Criar um TStringStream com algumas linhas de exemplo.
  LSampleFile := TStringStream.Create(
    'Line 01' + sLineBreak +
    'Line 2' + sLineBreak +
    'Line 03' + sLineBreak +
    'Line 4' + sLineBreak +
    'Line 05' + sLineBreak
  );

  LStreamReader := TStreamReaderEx.New(LSampleFile);
  LPartitionResult := LStreamReader.Partition(function(Line: string): Boolean
                                              begin
                                                Result := Length(Line) mod 2 = 0;
                                              end);
  // Obter o TStreamReaderHelper à esquerda e à direita.
  LLeftStreamReader := LPartitionResult.Key;
  LRightStreamReader := LPartitionResult.Value;
  try
    // Verificar se o conteúdo do TStreamReaderHelper à esquerda é o esperado.
    Assert.AreEqual('Line 2', LLeftStreamReader.AsLine);
    Assert.AreEqual('Line 4', LLeftStreamReader.AsLine);

    // Verificar se o conteúdo do TStreamReaderHelper à direita é o esperado.
    Assert.AreEqual('Line 01', LRightStreamReader.AsLine);
    Assert.AreEqual('Line 03', LRightStreamReader.AsLine);
    Assert.AreEqual('Line 05', LRightStreamReader.AsLine);
  finally
    LStreamReader.Free;
    LSampleFile.Free;
    LLeftStreamReader.Free;
    LRightStreamReader.Free;
  end;
end;

procedure TestStreamReader.TestReduce;
var
  TotalLength: Integer;
begin
  // Teste para calcular o comprimento total das linhas no arquivo de amostra
  TotalLength := FStreamReader.Reduce(
    function(Accumulator: Integer; Line: string): Integer
    begin
      Result := Accumulator + Length(Line);
    end,
    0);

  // Verificar se o resultado é o esperado
  Assert.AreEqual(78, TotalLength, 'Comprimento total das linhas incorreto');
end;

procedure TestStreamReader.TestSkip;
var
  LStreamReader: TStreamReaderEx;
  LSampleFile: TStringStream;
begin
  LSampleFile := TStringStream.Create(
    'Line 1' + sLineBreak +
    'Line 2' + sLineBreak +
    'Line 3' + sLineBreak +
    'Line 4' + sLineBreak +
    'Line 5'
  );
  LStreamReader := TStreamReaderEx.New(LSampleFile);
  try
    // Ler e descartar as três primeiras linhas
    LStreamReader.Skip(3);
    Assert.AreEqual('Line 4', LStreamReader.AsLine);
    Assert.AreEqual('Line 5', LStreamReader.AsLine);
  finally
    LStreamReader.Free;
    LSampleFile.Free;
  end;
end;

procedure TestStreamReader.TestSort;
var
  LStreamReader: TStreamReaderEx;
  LSampleFile: TStringStream;
begin
  LSampleFile := TStringStream.Create(
    'C' + sLineBreak +
    'A' + sLineBreak +
    'B'
  );
  LStreamReader := TStreamReaderEx.New(LSampleFile);
  try
    LStreamReader.Sort;
    Assert.AreEqual('A', LStreamReader.AsLine);
    Assert.AreEqual('B', LStreamReader.AsLine);
    Assert.AreEqual('C', LStreamReader.AsLine);
  finally
    LStreamReader.Free;
    LSampleFile.Free;
  end;
end;

procedure TestStreamReader.TestTake;
var
  LStreamReader: TStreamReaderEx;
begin
  LStreamReader := TStreamReaderEx.New(FSampleFile);
  try
    LStreamReader.Take(2);
    Assert.AreEqual('This is a sample text file.', LStreamReader.AsLine);
    Assert.AreEqual('It contains multiple lines.', LStreamReader.AsLine);
  finally
    LStreamReader.Free;
  end;
end;

[Test]
procedure TestStreamReader.TestConcat;
var
  LSampleFile2: TStringStream;
  LStreamReader: TStreamReaderEx;
  LStreamReader2: TStreamReaderEx;
begin
  LStreamReader := TStreamReaderEx.New(FSampleFile);
  LSampleFile2 := TStringStream.Create(
    'Another line of text.' + sLineBreak +
    'Yet another line of text.'
  );
  LStreamReader2 := TStreamReaderEx.New(LSampleFile2);
  try
    // Verifique se o método Concat funciona corretamente.
    LStreamReader.Concat(LStreamReader2);

    Assert.AreEqual('This is a sample text file.', LStreamReader.AsLine);
    Assert.AreEqual('It contains multiple lines.', LStreamReader.AsLine);
    Assert.AreEqual('Each line has some text.', LStreamReader.AsLine);
    Assert.AreEqual('Another line of text.', LStreamReader.AsLine);
    Assert.AreEqual('Yet another line of text.', LStreamReader.AsLine);
  finally
    LStreamReader.Free;
    LStreamReader2.Free;
    LSampleFile2.Free;
  end;
end;


{ TPerson }

constructor TPerson.Create(const AName: string; const AAge: Integer;
  const ADate: TDateTime);
begin
  FName := AName;
  FAge := AAge;
  FDate := ADate;
end;

function TPerson.GetAge: Integer;
begin
  Result := FAge;
end;

function TPerson.GetDate: TDateTime;
begin
  Result := FDate;
end;

function TPerson.GetName: string;
begin
  Result := FName;
end;

initialization
  TDUnitX.RegisterTestFixture(TestStreamReader);

end.
