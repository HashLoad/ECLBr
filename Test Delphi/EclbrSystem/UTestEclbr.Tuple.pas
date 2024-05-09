unit UTestEclbr.Tuple;

interface

uses
  DUnitX.TestFramework,
  Rtti,
  eclbr.tuple,
  eclbr.match,
  eclbr.result.pair;

type
  [TestFixture]
  TestTuple = class
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestStrIntTupla;
    [Test]
    procedure TestIntDoubleTupla;
    [Test]
    procedure TestStrIntTuplaNew;
    [Test]
    procedure TestIntDoubleTuplaNew;
    [Test]
    procedure TestMatchTupla;
    [Test]
    procedure TestMatchTuplaAsterisco;
    [Test]
    procedure TestNewTuple;
    [Test]
    procedure TestTupleGet;
    [Test]
    procedure TestTupleEquality;
    [Test]
    procedure TestTupleEquality_2;
  end;

implementation

{ TestTuple }

procedure TestTuple.Setup;
begin

end;

procedure TestTuple.TearDown;
begin

end;

procedure TestTuple.TestIntDoubleTupla;
var
  LTuplaIntDouble: TTuple<Integer>;
  LValueDouble: Double;
begin
  LTuplaIntDouble := TTuple<Integer>.New([1, 2, 3], [False, 2.2, 3.3]);

  Assert.IsTrue(LTuplaIntDouble.Get<Double>(2) > 0);

  LValueDouble := LTuplaIntDouble.Get<Double>(2);
  Assert.AreEqual(2.2, LValueDouble, 0.01);
end;

procedure TestTuple.TestStrIntTupla;
var
  LTuplaStrInt: TTuple<String>;
  LValueInt: Integer;
begin
  LTuplaStrInt := TTuple<String>.New(['A', 'B', 'C'], [1, 2, '3']);

  Assert.IsTrue(LTuplaStrInt.Get<Integer>('B') > 0);

  LValueInt := LTuplaStrInt.Get<Integer>('B');
  Assert.AreEqual(2, LValueInt);
end;

procedure TestTuple.TestIntDoubleTuplaNew;
var
  LTuplaIntDouble: TTuple<Integer>;
begin
  LTuplaIntDouble := LTuplaIntDouble.SetTuple([1, 2, 3], [1.1, 2.2, True]);

  Assert.IsTrue(LTuplaIntDouble.Get<Double>(2) > 0);
  Assert.AreEqual(2.2, LTuplaIntDouble[2].AsExtended);
end;

procedure TestTuple.TestMatchTupla;
var
  LTuple: Tuple;
  LResult: TResultPair<String, String>;
begin
  LTuple := ['Idade', 25];
  LResult := TMatch<Tuple>.Value(LTuple)
    .CaseEq(['_', 'Alice'], function(Value: Tuple): TValue begin Result := 'Personagem'; end)
    .CaseEq(['_', 25],      function(Value: Tuple): TValue begin Result := 'Jovem'; end)
    .CaseEq(['_', False],   function(Value: Tuple): TValue begin Result := 'Fria'; end)
    .Default(               function:               TValue begin Result := 'Default'; end)
    .Execute<String>;
  try
    Assert.AreEqual('Jovem', LResult.ValueSuccess);
    Assert.AreEqual(LTuple[0].AsString, 'Idade');
    Assert.AreEqual(LTuple[1].AsInteger, 25);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTuple.TestMatchTuplaAsterisco;
var
  LTuple: Tuple;
  LResult: TResultPair<String, String>;
begin
  LTuple := ['Idade', 25];
  LResult := TMatch<Tuple>.Value(LTuple)
    .CaseEq(['Nome', '_*'],   function(Value: Tuple): TValue begin Result := 'Personagem'; end)
    .CaseEq(['Idade', '_*'],  function(Value: Tuple): TValue begin Result := 'Jovem'; end)
    .CaseEq(['Cidade', '_*'], function(Value: Tuple): TValue begin Result := 'Fria'; end)
    .Default(                 function:               TValue begin Result := 'Default'; end)
    .Execute<String>;
  try
    Assert.AreEqual('Jovem', LResult.ValueSuccess);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTuple.TestNewTuple;
var
  LTuple: TTuple;
begin
  // Act
  LTuple := TTuple.New([10, 'Hello', 3.14]);

  // Assert
  Assert.AreEqual(3, LTuple.Count);
  Assert.AreEqual(10, LTuple[0].AsInteger);
  Assert.AreEqual('Hello', LTuple[1].AsString);
  Assert.AreEqual(3.14, LTuple[2].AsExtended);
end;

procedure TestTuple.TestStrIntTuplaNew;
var
  LTuplaStrInt: TTuple<String>;
begin
  LTuplaStrInt := TTuple<String>.New(['A', 'B', 'C'], ['1', 2.3, 3]);

  Assert.IsTrue(LTuplaStrInt['B'].AsExtended > 0);
  Assert.AreEqual(3, LTuplaStrInt['C'].AsInteger);
end;

procedure TestTuple.TestTupleEquality;
var
  LTuple1: TTuple;
  LTuple2: TTuple;
begin
  // Arrange
  LTuple1 := TTuple.New([15, 'Hello Word']);
  LTuple2 := TTuple.New([15, 'Hello Word']);
  // Act & Assert
  Assert.IsTrue(LTuple1 = LTuple2);
  Assert.IsFalse(LTuple1 <> LTuple2);
end;

procedure TestTuple.TestTupleEquality_2;
var
  LTuple1: TTuple;
  LTuple2: TTuple;
begin
  LTuple1 := [10, 'Hello'];
  LTuple2 := [10, 'Hello'];
  // Act & Assert
  Assert.IsTrue(LTuple1 = LTuple2);
  Assert.IsFalse(LTuple1 <> LTuple2);
end;

procedure TestTuple.TestTupleGet;
var
  LTuple: TTuple;
  LValue: Integer;
begin
  // Arrange
  LTuple := TTuple.New([10, 'Hello', 3.14]);
  // Act
  LValue := LTuple.Get<Integer>(0);
  // Assert
  Assert.AreEqual(10, LValue);
end;

initialization
  TDUnitX.RegisterTestFixture(TestTuple);

end.





