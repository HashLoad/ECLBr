unit UTestEclbr.Str;

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  StrUtils,
  DateUtils,
  Classes,
  Generics.Collections;

type
  [TestFixture]
  TTesTStd = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestFilter;
    [Test]
    procedure TestCollect;
    [Test]
    procedure TestMap;
    [Test]
    procedure TestSum;
    [Test]
    procedure TestFirst;
    [Test]
    procedure TestLast;
    [Test]
    procedure TestReduce;
    [Test]
    procedure TestExists;
    [Test]
    procedure TestAll;
    [Test]
    procedure TestAny;
    [Test]
    procedure TestSort;
    [Test]
    procedure TestPartition;
  end;

implementation

uses
  eclbr.str, eclbr.vector;

procedure TTesTStd.Setup;
begin

end;

procedure TTesTStd.TearDown;
begin

end;

procedure TTesTStd.TestAll;
var
  LStr: string;
  LResultAll: Boolean;
begin
  // Caso de teste 1: verificar se todos os caracteres são letras minúsculas
  LStr := 'helloworld';
  LResultAll := LStr.All(function(C: Char): Boolean
                         begin
                           Result := CharInSet(C, ['a'..'z']);
                         end);
  Assert.IsTrue(LResultAll, 'Caso de teste 1');

  // Caso de teste 2: verificar se todos os caracteres são letras maiúsculas
  LStr := 'HELLOWORLD';
  LResultAll := LStr.All(function(C: Char): Boolean
                         begin
                           Result := CharInSet(C, ['A'..'Z']);
                         end);
  Assert.IsTrue(LResultAll, 'Caso de teste 2');

  // Caso de teste 3: verificar se todos os caracteres são dígitos
  LStr := '1234567890';
  LResultAll := LStr.All(function(C: Char): Boolean
                         begin
                           Result := CharInSet(C, ['0'..'9']);
                         end);
  Assert.IsTrue(LResultAll, 'Caso de teste 3');

  // Caso de teste 4: verificar se todos os caracteres são letras
  LStr := 'Hello World';
  LResultAll := LStr.All(function(C: Char): Boolean
                         begin
                           Result := C.IsLetter;
                         end);
  Assert.IsFalse(LResultAll, 'Caso de teste 4');
end;

procedure TTesTStd.TestAny;
var
  LStr: string;
  LResultAny: Boolean;
begin
  // Caso de teste 1: verificar se há pelo menos um caractere na string
  LStr := 'Hello World';
  LResultAny := LStr.Any(function(C: Char): Boolean
                       begin
                         Result := C = 'o';
                       end);
  Assert.IsTrue(LResultAny);

  // Caso de teste 2: verificar se há pelo menos um caractere na string que seja um dígito
  LStr := 'Hello World';
  LResultAny := LStr.Any(function(C: Char): Boolean
                       begin
                         Result := CharInSet(C, ['0'..'9']);
                       end);
  Assert.IsFalse(LResultAny);
end;

procedure TTesTStd.TestCollect;
var
  LStr: string;
  LCollected: TVector<string>;
begin
  // Caso de teste 1: coletar todos os caracteres em uma lista
  LStr := 'Hello World';
  LCollected := LStr.Collect;

  Assert.AreEqual(11, LCollected.Count);
  Assert.AreEqual('H', LCollected[0]);
  Assert.AreEqual('e', LCollected[1]);
  Assert.AreEqual('l', LCollected[2]);
  Assert.AreEqual('l', LCollected[3]);
  Assert.AreEqual('o', LCollected[4]);
  Assert.AreEqual(' ', LCollected[5]);
  Assert.AreEqual('W', LCollected[6]);
  Assert.AreEqual('o', LCollected[7]);
  Assert.AreEqual('r', LCollected[8]);
  Assert.AreEqual('l', LCollected[9]);
  Assert.AreEqual('d', LCollected[10]);
end;

procedure TTesTStd.TestExists;
var
  LStr: string;
  LResultExists: Boolean;
begin
  // Caso de teste 1: verificar se a letra 'o' existe na string
  LStr := 'Hello World';
  LResultExists := LStr.Exists(function(C: Char): Boolean
                               begin
                                 Result := C = 'o';
                               end);
  Assert.IsTrue(LResultExists);
end;

procedure TTesTStd.TestFilter;
var
  LStr: string;
  LFilteredStr: string;
begin
  // Caso de teste 1: filtrar apenas vogais
  LStr := 'Hello World';
  LFilteredStr := LStr.Filter(function(C: Char): Boolean begin
                              Result := CharInSet(C, ['A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u']);
                            end);
  Assert.AreEqual('eoo', LFilteredStr);

  // Caso de teste 2: filtrar apenas letras maiúsculas
  LStr := 'Hello World';
  LFilteredStr := LStr.Filter(function(C: Char): Boolean begin
                              Result := CharInSet(C, ['A'..'Z']);
                            end);
  Assert.AreEqual('HW', LFilteredStr);

  // Caso de teste 3: filtrar apenas letras minúsculas
  LStr := 'Hello World';
  LFilteredStr := LStr.Filter(function(C: Char): Boolean begin
                              Result := CharInSet(C, ['a'..'z']);
                            end);
  Assert.AreEqual('elloorld', LFilteredStr);
end;

procedure TTesTStd.TestFirst;
var
  LStr: string;
  LFirstChar: Char;
begin
  // Caso de teste 1: obter o primeiro caractere da string
  LStr := 'Hello World';
  LFirstChar := LStr.First;

  Assert.AreEqual('H', LFirstChar);
end;

procedure TTesTStd.TestLast;
var
  LStr: string;
  LLastChar: Char;
begin
  // Caso de teste 1: obter o último caractere da string
  LStr := 'Hello World';
  LLastChar := LStr.Last;

  Assert.AreEqual('d', LLastChar);
end;

procedure TTesTStd.TestMap;
var
  LStr: String;
  LMappedStr: String;
begin
  // Caso de teste 1: transformar todas as letras em maiúsculas
  LStr := 'Hello World';
  LMappedStr := LStr.Map(function(C: Char): Char begin
                          Result := C.ToUpper;
                        end);
  Assert.AreEqual('HELLO WORLD', LMappedStr);

  // Caso de teste 2: transformar todas as letras em minúsculas
  LStr := 'HELLO WORLD';
  LMappedStr := LStr.Map(function(C: Char): Char begin
                          Result :=  C.ToLower;
                        end);
  Assert.AreEqual('hello world', LMappedStr);

  // Caso de teste 3: transformar todas as letras em asteriscos
  LStr := 'Hello World';
  LMappedStr := LStr.Map(function(C: Char): Char begin
                          Result := '*';
                        end);
  Assert.AreEqual('***********', LMappedStr);
end;

procedure TTesTStd.TestPartition;
var
  LStr, LLeft, LRight: string;
begin
  // Caso de teste 1: particionar a string com base em caracteres numéricos
  LStr := 'Hello123World';
  LStr.Partition(
    function(C: Char): Boolean
    begin
      Result := CharInSet(C, ['0'..'9']);
    end,
    LLeft, LRight
  );

  Assert.AreEqual('HelloWorld', LLeft);
  Assert.AreEqual('123', LRight);
end;

procedure TTesTStd.TestReduce;
var
  LStr: string;
  LReducedResult: string;
begin
  // Caso de teste 1: concatenar todos os caracteres da string
  LStr := 'Hello';
  LReducedResult := LStr.Reduce<String>('',
    function(Accumulator: string; CharToAdd: Char): string
    begin
      Result := Accumulator + CharToAdd;
    end
  );

  Assert.AreEqual('Hello', LReducedResult);
end;

procedure TTesTStd.TestSort;
var
  LStr: string;
  LSortedStr: string;
begin
  // Caso de teste 1: ordenar os caracteres em ordem alfabética
  LStr := 'Hello World';
  LSortedStr := LStr.Sort;

  Assert.AreEqual(' HWdellloor', LSortedStr);
end;

procedure TTesTStd.TestSum;
var
  LStr: string;
  LSumResult: Integer;
begin
  // Caso de teste 1: somar todos os números na string
  LStr := '12345';
  LSumResult := LStr.Sum;

  Assert.AreEqual(15, LSumResult);
end;

initialization
  TDUnitX.RegisterTestFixture(TTesTStd);

end.

