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
  end;

implementation

{ TestTIfThen }

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
  LTuplaStrInt: TTuple<string>;
  LValueInt: Integer;
begin
  LTuplaStrInt := TTuple<string>.New(['A', 'B', 'C'], [1, 2, '3']);

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
  Assert.AreEqual(2.2, LTuplaIntDouble[2].AsExtended, 0.01);
end;

procedure TestTuple.TestMatchTupla;
var
  LTuple: Tuple;
  LResult: TResultPair<string, string>;
begin
  LTuple := ['Idade', 25];
  LResult := TMatch<Tuple>.Value(LTuple)
    .CaseEq(['_', 'Alice'], function(Value: Tuple): TValue begin Result := 'Personagem'; end)
    .CaseEq(['_', 25],      function(Value: Tuple): TValue begin Result := 'Jovem'; end)
    .CaseEq(['_', false],   function(Value: Tuple): TValue begin Result := 'Fria'; end)
    .Default(               function:               TValue begin Result := 'Default'; end)
    .Execute<string>;
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
  LResult: TResultPair<string, string>;
begin
  LTuple := ['Idade', 25];
  LResult := TMatch<Tuple>.Value(LTuple)
    .CaseEq(['Nome', '_*'],   function(Value: Tuple): TValue begin Result := 'Personagem'; end)
    .CaseEq(['Idade', '_*'],  function(Value: Tuple): TValue begin Result := 'Jovem'; end)
    .CaseEq(['Cidade', '_*'], function(Value: Tuple): TValue begin Result := 'Fria'; end)
    .Default(                function:                TValue begin Result := 'Default'; end)
    .Execute<string>;
  try
    Assert.AreEqual('Jovem', LResult.ValueSuccess);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTuple.TestStrIntTuplaNew;
var
  LTuplaStrInt: TTuple<string>;
begin
  LTuplaStrInt := TTuple<string>.New(['A', 'B', 'C'], ['1', 2.3, 3]);

  Assert.IsTrue(LTuplaStrInt['B'].AsExtended > 0);
  Assert.AreEqual(3, LTuplaStrInt['C'].AsInteger);
end;

initialization
  TDUnitX.RegisterTestFixture(TestTuple);

end.

