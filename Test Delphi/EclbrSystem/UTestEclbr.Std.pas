unit UTestEclbr.Std;

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  DateUtils,
  Classes,
  Generics.Collections,
  eclbr.objects,
  eclbr.std;

type
  [TestFixture]
  TTesTStd = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestArrayMerge_IntegerArrays;
    [Test]
    procedure TestArrayMerge_StringArrays;
    [Test]
    procedure TestArrayCopy_StringArrays;
    [Test]
    procedure TestAsList_IntegerArray;
    [Test]
    procedure TestJoinStrings_StringArray;
    [Test]
    procedure TestJoinStrings_StringList;
    [Test]
    procedure TestRemoveTrailingChars;
    [Test]
    procedure TestIso8601ToDateTime;
    [Test]
    procedure TestDateTimeToIso8601;
    [Test]
    procedure TestDecodeBase64;
    [Test]
    procedure TestEncodeBase64;
    [Test]
    procedure TestEncodeString;
    [Test]
    procedure TestDecodeString;
    [Test]
    procedure TestMinInteger;
    [Test]
    procedure TestMinDouble;
    [Test]
    procedure TestMinCurrency;
    [Test]
    procedure TestSplit;
    [Test]
    procedure TestArrayReduce;
    [Test]
    procedure TestArrayMap;
    [Test]
    procedure TestArrayFilter;
    [Test]
    procedure TestForEach;
    [Test]
    procedure TestAny;
    [Test]
    procedure TestAll;
  end;

implementation

procedure TTesTStd.Setup;
begin

end;

procedure TTesTStd.TearDown;
begin

end;

procedure TTesTStd.TestAll;
begin
  // Act & Assert
  Assert.IsTrue(TArray.All<Integer>([1, 2, 3, 4, 5],
    function(AValue: Integer): Boolean
    begin
      Result := AValue < 6;
    end));

  Assert.IsFalse(TArray.All<Integer>([1, 2, 3, 4, 5],
    function(AValue: Integer): Boolean
    begin
      Result := AValue < 4;
    end));
end;

procedure TTesTStd.TestAny;
begin
  // Act & Assert
  Assert.IsTrue(TArray.Any<Integer>([1, 2, 3, 4, 5],
    function(AValue: Integer): Boolean
    begin
      Result := AValue > 3;
    end));

  Assert.IsFalse(TArray.Any<Integer>([1, 2, 3, 4, 5],
    function(AValue: Integer): Boolean
    begin
      Result := AValue > 5;
    end));
end;

procedure TTesTStd.TestArrayCopy_StringArrays;
var
  LSourceArray, LCopiedArray: TArrayString;
  LIndex, LCount: Integer;
const
  LExpectedArray: TArrayString = ['Item1', 'Item2', 'Item3'];
begin
  // Arrange
  LSourceArray := TArrayString.Create('Item0', 'Item1', 'Item2', 'Item3', 'Item4');
  LIndex := 1;  // Index to start copying
  LCount := 3;  // Number of elements to copy

  // Act
  LCopiedArray := TStd.ArrayCopy(LSourceArray, LIndex, LCount);

  // Assert
  Assert.AreEqual(Length(LExpectedArray), Length(LCopiedArray));
  for LIndex := Low(LExpectedArray) to High(LExpectedArray) do
    Assert.AreEqual(LExpectedArray[LIndex], LCopiedArray[LIndex]);
end;

procedure TTesTStd.TestArrayFilter;
var
  LEvenNumbers: TArray<Integer>;
begin
  LEvenNumbers := TArray.Filter<Integer>([1, 2, 3, 4, 5],
    function(AValue: Integer): Boolean
    begin
      Result := AValue mod 2 = 0;
    end);

  // Assert
  Assert.AreEqual<Integer>(2, Length(LEvenNumbers));
  Assert.AreEqual(2, LEvenNumbers[0]);
  Assert.AreEqual(4, LEvenNumbers[1]);
end;

procedure TTesTStd.TestArrayMap;
var
  LDoubledArray: TArray<Integer>;
begin
  LDoubledArray := TArray.Map<Integer, Integer>([1, 2, 3, 4, 5],
    function(AValue: Integer): Integer
    begin
      Result := AValue * 2;
    end);
    // Assert
  Assert.AreEqual<Integer>(2, LDoubledArray[0]);
  Assert.AreEqual<Integer>(4, LDoubledArray[1]);
  Assert.AreEqual<Integer>(6, LDoubledArray[2]);
  Assert.AreEqual<Integer>(8, LDoubledArray[3]);
  Assert.AreEqual<Integer>(10, LDoubledArray[4]);
end;

procedure TTesTStd.TestArrayMerge_IntegerArrays;
var
  LArray1, LArray2, LMergedArray: TArray<Integer>;
  LFor: Integer;
const
  LExpectedArray: TArray<Integer> = [1, 2, 3, 4, 5, 6];
begin
  // Arrange
  SetLength(LArray1, 3);
  LArray1[0] := 1;
  LArray1[1] := 2;
  LArray1[2] := 3;
  SetLength(LArray2, 3);
  LArray2[0] := 4;
  LArray2[1] := 5;
  LArray2[2] := 6;
  // Act
  LMergedArray := TStd.ArrayMerge<Integer>(LArray1, LArray2);
  // Assert
  Assert.AreEqual(Length(LExpectedArray), Length(LMergedArray));
  for LFor := Low(LExpectedArray) to High(LExpectedArray) do
    Assert.AreEqual(LExpectedArray[LFor], LMergedArray[LFor]);
end;

procedure TTesTStd.TestArrayMerge_StringArrays;
var
  LArray1, LArray2, LMergedArray: TArray<String>;
  LFor: Integer;
const
  LExpectedArray: TArray<String> = ['Hello', 'Word', 'DUnitX', 'Testing'];
begin
  // Arrange
  SetLength(LArray1, 2);
  LArray1[0] := 'Hello';
  LArray1[1] := 'Word';
  SetLength(LArray2, 2);
  LArray2[0] := 'DUnitX';
  LArray2[1] := 'Testing';
  // Act
  LMergedArray := TStd.ArrayMerge<String>(LArray1, LArray2);
  // Assert
  // Assert
  Assert.AreEqual(Length(LExpectedArray), Length(LMergedArray));
  for LFor := Low(LExpectedArray) to High(LExpectedArray) do
    Assert.AreEqual(LExpectedArray[LFor], LMergedArray[LFor]);
end;

procedure TTesTStd.TestArrayReduce;
var
  LSum: Integer;
begin
  LSum := TArray.Reduce<Integer>([1, 2, 3, 4, 5],
    function(accumulated, current: Integer): Integer
    begin
      Result := accumulated + current;
    end);
  Assert.AreEqual(15, LSum);
end;

procedure TTesTStd.TestAsList_IntegerArray;
var
  LInputArray: TArray<Integer>;
  LList: TList<Integer>;
  LLength: Integer;
begin
  // Arrange
  LInputArray := TArray<Integer>.Create(1, 2, 3, 4, 5);
  // Act
  LList := TStd.AsList<Integer>(LInputArray);
  try
    // Assert
    LLength := Length(LInputArray);
    Assert.AreEqual(LLength, Integer(LList.Count));
    for var i := 0 to High(LInputArray) do
      Assert.AreEqual(LInputArray[i], LList[i]);
  finally
    LList.Free;
  end;
end;

procedure TTesTStd.TestDateTimeToIso8601;
var
  LInputDateTime: TDateTime;
  LResultString: String;
begin
  // Arrange
  LInputDateTime := EncodeDateTime(2023, 9, 26, 14, 30, 0, 0);
  // Act
  LResultString := TStd.DateTimeToIso8601(LInputDateTime, True);

  // Assert
  // Verifique se o ResultString corresponde à String ISO 8601 esperada
  Assert.AreEqual('2023-09-26T14:30:00', LResultString);
end;

procedure TTesTStd.TestDecodeBase64;
var
  LInputBase64: String;
  LResultBytes: TBytes;
  LDecodedString: String;
  LExpectedString: String;
  LLength: Integer;
  LResult: Integer;
begin
  // Arrange
  LInputBase64 := 'SGVsbG8sIFdvcmxkIQ==';
  LExpectedString := 'Hello, World!';

  // Act
  LResultBytes := TStd.DecodeBase64(LInputBase64);

  // Assert
  LLength := Length(LExpectedString);
  LResult := Length(LResultBytes);
  Assert.AreEqual(LLength, LResult);
  LDecodedString := StringOf(LResultBytes);
  Assert.AreEqual(LExpectedString, LDecodedString);
end;

procedure TTesTStd.TestDecodeString;
var
  LInputString: String;
  LResultString: String;
begin
  // Arrange
  LInputString := 'SGVsbG8sIFdvcmxkIQ=='; // Coloque a String Base64 que deseja decodificar aqui

  // Act
  LResultString := TStd.DecodeString(LInputString);

  // Assert
  // Verifique se o ResultString corresponde à String decodificada esperada
  // Ajuste a String esperada conforme necessário
  Assert.AreEqual('Hello, World!', LResultString);
end;

procedure TTesTStd.TestEncodeBase64;
var
  LInputData: TBytes;
  LResultString: String;
begin
  // Arrange
  SetLength(LInputData, 13);
  // Preencha InputData com os dados que você deseja codificar
  LInputData[0] := Ord('H');
  LInputData[1] := Ord('e');
  LInputData[2] := Ord('l');
  LInputData[3] := Ord('l');
  LInputData[4] := Ord('o');
  LInputData[5] := Ord(',');
  LInputData[6] := Ord(' ');
  LInputData[7] := Ord('W');
  LInputData[8] := Ord('o');
  LInputData[9] := Ord('r');
  LInputData[10] := Ord('l');
  LInputData[11] := Ord('d');
  LInputData[12] := Ord('!');

  // Act
  LResultString := TStd.EncodeBase64(@LInputData[0], Length(LInputData));

  // Assert
  // Verifique se o ResultString corresponde à String Base64 esperada
  Assert.AreEqual('SGVsbG8sIFdvcmxkIQ==', LResultString);
end;

procedure TTesTStd.TestEncodeString;
var
  LInputString: String;
  LResultString: String;
begin
  // Arrange
  LInputString := 'Hello, World!'; // Coloque a String que você deseja codificar aqui

  // Act
  LResultString := TStd.EncodeString(LInputString);

  // Assert
  // Verifique se o ResultString corresponde à String codificada esperada
  // Ajuste a String esperada conforme necessário
  Assert.AreEqual('SGVsbG8sIFdvcmxkIQ==', LResultString);
end;

procedure TTesTStd.TestForEach;
var
  LSum: Integer;
begin
  // Arrange
  LSum := 0;

  // Act
  TArray.ForEach<Integer>([1, 2, 3, 4, 5],
    procedure(AValue: Integer)
    begin
      LSum := LSum + AValue;
    end);

  // Assert
  Assert.AreEqual<Integer>(15, LSum);
end;

procedure TTesTStd.TestIso8601ToDateTime;
var
  LIso8601DateString: String;
  LResultDateTime: TDateTime;
begin
  // Arrange
  LIso8601DateString := '2023-09-26T14:30:00Z';
  // UseISO8601DateFormat é um valor Booleano que determina o formato da data.
  // A implementação real deve ser configurada de acordo com a sua lógica.

  // Act
  LResultDateTime := TStd.Iso8601ToDateTime(LIso8601DateString, True);

  // Assert
  // Verifique se o ResultDateTime está correto com base no valor da data de entrada.
  // Você pode usar a função CompareDateTime ou outras comparações apropriadas.
  Assert.AreEqual(2023, YearOf(LResultDateTime));
  Assert.AreEqual(9, MonthOf(LResultDateTime));
  Assert.AreEqual(26, DayOf(LResultDateTime));
  Assert.AreEqual(14, HourOf(LResultDateTime));
  Assert.AreEqual(30, MinuteOf(LResultDateTime));
  Assert.AreEqual(0, SecondOf(LResultDateTime));
end;

procedure TTesTStd.TestJoinStrings_StringArray;
var
  LStrings: TArrayString;
  LSeparator, LResultString: String;
begin
  // Arrange
  LStrings := TArrayString.Create('Hello', 'World', 'DUnitX', 'Testing');
  LSeparator := ', ';
  // Act
  LResultString := TStd.JoinStrings(LStrings, LSeparator);
  // Assert
  Assert.AreEqual('Hello, World, DUnitX, Testing', LResultString);
end;

procedure TTesTStd.TestJoinStrings_StringList;
var
  LSeparator, LResultString: String;
  LIAutoRef: IAutoRef<TListString>;
begin
  // Arrange
  LIAutoRef := TAutoRef<TListString>.New;
  LIAutoRef.AsRef.Add('Hello');
  LIAutoRef.AsRef.Add('World');
  LIAutoRef.AsRef.Add('DUnitX');
  LIAutoRef.AsRef.Add('Testing');

  LSeparator := ', ';
  // Act
  LResultString := TStd.JoinStrings(LIAutoRef.AsRef, LSeparator);
  // Assert
  Assert.AreEqual('Hello, World, DUnitX, Testing', LResultString);
end;

procedure TTesTStd.TestMinCurrency;
var
  LA, LB: Currency;
  LResultValue: Currency;
  LExpectativa: Currency;
begin
  // Arrange
  LA := 100.50;
  LB := 99.99;
  // Act
  LResultValue := TStd.Min(LA, LB);

  // Assert
  LExpectativa := 99.99;
  Assert.AreEqual(LExpectativa, LResultValue, 'Min(A, B) should return 99.99 for A = 100.50 and B = 99.99.');end;

procedure TTesTStd.TestMinDouble;
var
  LA, LB: Double;
  LResultValue: Double;
  LExpectativa: Double;
begin
  // Arrange
  LA := 3.14;
  LB := 2.71;
  // Act
  LResultValue := TStd.Min(LA, LB);

  // Assert
  LExpectativa := 2.71;
  Assert.AreEqual(LExpectativa, LResultValue, 'Min(A, B) should return 2.71 for A = 3.14 and B = 2.71.');end;

procedure TTesTStd.TestMinInteger;
var
  LA, LB, LResultValue: Integer;
begin
  // Arrange
  LA := 5;
  LB := 10;
  // Act
  LResultValue := TStd.Min(LA, LB);

  // Assert
  Assert.AreEqual(5, LResultValue, 'Min(A, B) should return 5 for A = 5 and B = 10.');
end;

procedure TTesTStd.TestRemoveTrailingChars;
var
  LInputString, LResultString: String;
  LTrailingChars: TSysCharSet;
begin
  // Arrange
  LInputString := 'Hello, World!!!';
  LTrailingChars := ['!', ','];
  // Act
  LResultString := TStd.RemoveTrailingChars(LInputString, LTrailingChars);
  // Assert
  Assert.AreEqual('Hello, World', LResultString);
end;

procedure TTesTStd.TestSplit;
var
  LS: String;
  LResultArray: TArray<String>;
  LLength: Integer;
begin
  // Arrange
  LS := 'Hello,World';
  // Act
  LResultArray := TStd.Split(LS);

  LLength := Length(LResultArray);
  // Assert
  Assert.AreEqual(11, LLength, 'Split should return an array with 2 elements.');
  Assert.AreEqual('H', LResultArray[0], 'element should be "Hello".');
  Assert.AreEqual('e', LResultArray[1], 'element should be "Hello".');
  Assert.AreEqual('l', LResultArray[2], 'element should be "Hello".');
  Assert.AreEqual('l', LResultArray[3], 'element should be "Hello".');
  Assert.AreEqual('o', LResultArray[4], 'element should be "Hello".');
  Assert.AreEqual(',', LResultArray[5], 'element ",".');
  Assert.AreEqual('W', LResultArray[6], 'element should be "World".');
  Assert.AreEqual('o', LResultArray[7], 'element should be "World".');
  Assert.AreEqual('r', LResultArray[8], 'element should be "World".');
  Assert.AreEqual('l', LResultArray[9], 'element should be "World".');
  Assert.AreEqual('d', LResultArray[10], 'element should be "World".');
end;

initialization
  TDUnitX.RegisterTestFixture(TTesTStd);

end.

