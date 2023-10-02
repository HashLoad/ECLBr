unit UTestEclbr.Utils;

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  DateUtils,
  Classes,
  Generics.Collections,
  eclbr.objects,
  eclbr.utils;

type
  [TestFixture]
  TTestUtils = class
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
  end;

implementation

procedure TTestUtils.Setup;
begin

end;

procedure TTestUtils.TearDown;
begin

end;

procedure TTestUtils.TestArrayCopy_StringArrays;
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
  LCopiedArray := TUtils.ArrayCopy(LSourceArray, LIndex, LCount);

  // Assert
  Assert.AreEqual(Length(LExpectedArray), Length(LCopiedArray));
  for LIndex := Low(LExpectedArray) to High(LExpectedArray) do
    Assert.AreEqual(LExpectedArray[LIndex], LCopiedArray[LIndex]);
end;

procedure TTestUtils.TestArrayMerge_IntegerArrays;
var
  LArray1, LArray2, LMergedArray: TArray<Integer>;
  LFor: integer;
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
  LMergedArray := TUtils.ArrayMerge<integer>(LArray1, LArray2);
  // Assert
  Assert.AreEqual(Length(LExpectedArray), Length(LMergedArray));
  for LFor := Low(LExpectedArray) to High(LExpectedArray) do
    Assert.AreEqual(LExpectedArray[LFor], LMergedArray[LFor]);
end;

procedure TTestUtils.TestArrayMerge_StringArrays;
var
  LArray1, LArray2, LMergedArray: TArray<String>;
  LFor: integer;
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
  LMergedArray := TUtils.ArrayMerge<String>(LArray1, LArray2);
  // Assert
  // Assert
  Assert.AreEqual(Length(LExpectedArray), Length(LMergedArray));
  for LFor := Low(LExpectedArray) to High(LExpectedArray) do
    Assert.AreEqual(LExpectedArray[LFor], LMergedArray[LFor]);
end;

procedure TTestUtils.TestAsList_IntegerArray;
var
  LInputArray: TArray<Integer>;
  LList: TList<Integer>;
  LLength: integer;
begin
  // Arrange
  LInputArray := TArray<Integer>.Create(1, 2, 3, 4, 5);
  // Act
  LList := TUtils.AsList<Integer>(LInputArray);
  try
    // Assert
    LLength := Length(LInputArray);
    Assert.AreEqual(LLength, LList.Count);
    for var i := 0 to High(LInputArray) do
      Assert.AreEqual(LInputArray[i], LList[i]);
  finally
    LList.Free;
  end;
end;

procedure TTestUtils.TestDateTimeToIso8601;
var
  LInputDateTime: TDateTime;
  LResultString: string;
begin
  // Arrange
  LInputDateTime := EncodeDateTime(2023, 9, 26, 14, 30, 0, 0);
  // Act
  LResultString := TUtils.DateTimeToIso8601(LInputDateTime, True);

  // Assert
  // Verifique se o ResultString corresponde à string ISO 8601 esperada
  Assert.AreEqual('2023-09-26T14:30:00', LResultString);
end;

procedure TTestUtils.TestDecodeBase64;
var
  LInputBase64: string;
  LResultBytes: TBytes;
  LDecodedString: string;
  LExpectedString: string;
  LLength: integer;
  LResult: integer;
begin
  // Arrange
  LInputBase64 := 'SGVsbG8sIFdvcmxkIQ==';
  LExpectedString := 'Hello, World!';

  // Act
  LResultBytes := TUtils.DecodeBase64(LInputBase64);

  // Assert
  LLength := Length(LExpectedString);
  LResult := Length(LResultBytes);
  Assert.AreEqual(LLength, LResult);
  LDecodedString := StringOf(LResultBytes);
  Assert.AreEqual(LExpectedString, LDecodedString);
end;

procedure TTestUtils.TestDecodeString;
var
  LInputString: string;
  LResultString: string;
begin
  // Arrange
  LInputString := 'SGVsbG8sIFdvcmxkIQ=='; // Coloque a string Base64 que deseja decodificar aqui

  // Act
  LResultString := TUtils.DecodeString(LInputString);

  // Assert
  // Verifique se o ResultString corresponde à string decodificada esperada
  // Ajuste a string esperada conforme necessário
  Assert.AreEqual('Hello, World!', LResultString);
end;

procedure TTestUtils.TestEncodeBase64;
var
  LInputData: TBytes;
  LResultString: string;
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
  LResultString := TUtils.EncodeBase64(@LInputData[0], Length(LInputData));

  // Assert
  // Verifique se o ResultString corresponde à string Base64 esperada
  Assert.AreEqual('SGVsbG8sIFdvcmxkIQ==', LResultString);
end;

procedure TTestUtils.TestEncodeString;
var
  LInputString: string;
  LResultString: string;
begin
  // Arrange
  LInputString := 'Hello, World!'; // Coloque a string que você deseja codificar aqui

  // Act
  LResultString := TUtils.EncodeString(LInputString);

  // Assert
  // Verifique se o ResultString corresponde à string codificada esperada
  // Ajuste a string esperada conforme necessário
  Assert.AreEqual('SGVsbG8sIFdvcmxkIQ==', LResultString);
end;

procedure TTestUtils.TestIso8601ToDateTime;
var
  LIso8601DateString: string;
  LResultDateTime: TDateTime;
begin
  // Arrange
  LIso8601DateString := '2023-09-26T14:30:00Z';
  // UseISO8601DateFormat é um valor booleano que determina o formato da data.
  // A implementação real deve ser configurada de acordo com a sua lógica.

  // Act
  LResultDateTime := TUtils.Iso8601ToDateTime(LIso8601DateString, True);

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

procedure TTestUtils.TestJoinStrings_StringArray;
var
  LStrings: TArrayString;
  LSeparator, LResultString: string;
begin
  // Arrange
  LStrings := TArrayString.Create('Hello', 'World', 'DUnitX', 'Testing');
  LSeparator := ', ';
  // Act
  LResultString := TUtils.JoinStrings(LStrings, LSeparator);
  // Assert
  Assert.AreEqual('Hello, World, DUnitX, Testing', LResultString);
end;

procedure TTestUtils.TestJoinStrings_StringList;
var
  LStringList: TListString;
  LSeparator, LResultString: string;
  LIAutoRef: IAutoRef<TListString>;
begin
  // Arrange
  LIAutoRef := TAutoRef<TListString>.New;
  LIAutoRef.Get.Add('Hello');
  LIAutoRef.Get.Add('World');
  LIAutoRef.Get.Add('DUnitX');
  LIAutoRef.Get.Add('Testing');

  LSeparator := ', ';
  // Act
  LResultString := TUtils.JoinStrings(LIAutoRef.Get, LSeparator);
  // Assert
  Assert.AreEqual('Hello, World, DUnitX, Testing', LResultString);
end;

procedure TTestUtils.TestMinCurrency;
var
  LA, LB: Currency;
  LResultValue: Currency;
  LExpectativa: Currency;
begin
  // Arrange
  LA := 100.50;
  LB := 99.99;
  // Act
  LResultValue := TUtils.Min(LA, LB);

  // Assert
  LExpectativa := 99.99;
  Assert.AreEqual(LExpectativa, LResultValue, 'Min(A, B) should return 99.99 for A = 100.50 and B = 99.99.');end;

procedure TTestUtils.TestMinDouble;
var
  LA, LB: Double;
  LResultValue: Double;
  LExpectativa: Double;
begin
  // Arrange
  LA := 3.14;
  LB := 2.71;
  // Act
  LResultValue := TUtils.Min(LA, LB);

  // Assert
  LExpectativa := 2.71;
  Assert.AreEqual(LExpectativa, LResultValue, 'Min(A, B) should return 2.71 for A = 3.14 and B = 2.71.');end;

procedure TTestUtils.TestMinInteger;
var
  LA, LB, LResultValue: Integer;
begin
  // Arrange
  LA := 5;
  LB := 10;
  // Act
  LResultValue := TUtils.Min(LA, LB);

  // Assert
  Assert.AreEqual(5, LResultValue, 'Min(A, B) should return 5 for A = 5 and B = 10.');
end;

procedure TTestUtils.TestRemoveTrailingChars;
var
  LInputString, LResultString: string;
  LTrailingChars: TSysCharSet;
begin
  // Arrange
  LInputString := 'Hello, World!!!';
  LTrailingChars := ['!', ','];
  // Act
  LResultString := TUtils.RemoveTrailingChars(LInputString, LTrailingChars);
  // Assert
  Assert.AreEqual('Hello, World', LResultString);
end;

procedure TTestUtils.TestSplit;
var
  LS: string;
  LResultArray: TArray<string>;
  LLength: integer;
begin
  // Arrange
  LS := 'Hello,World';
  // Act
  LResultArray := TUtils.Split(LS);

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
  TDUnitX.RegisterTestFixture(TTestUtils);

end.
