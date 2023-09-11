unit UTestEclbr.SysTuple;

interface

uses
  DUnitX.TestFramework, eclbr.tuple;

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
  LTuplaIntDouble.Keys([1, 2, 3]).Values([False, 2.2, 3.3]);

  Assert.IsTrue(LTuplaIntDouble.Get<Double>(2) > 0);

  LValueDouble := LTuplaIntDouble.Get<Double>(2);
  Assert.AreEqual(2.2, LValueDouble, 0.01);
end;

procedure TestTuple.TestStrIntTupla;
var
  LTuplaStrInt: TTuple<string>;
  LValueInt: Integer;
begin
  LTuplaStrInt.Keys(['A', 'B', 'C']).Values([1, 2, '3']);

  Assert.IsTrue(LTuplaStrInt.Get<Integer>('B') > 0);

  LValueInt := LTuplaStrInt.Get<Integer>('B');
  Assert.AreEqual(2, LValueInt);
end;

procedure TestTuple.TestIntDoubleTuplaNew;
var
  LTuplaIntDouble: TTuple<Integer>;
  LValueDouble: Double;
begin
  LTuplaIntDouble := TTuple<Integer>.New([1, 2, 3], [1.1, 2.2, True]);

  Assert.IsTrue(LTuplaIntDouble.Get<Double>(2) > 0);

  LValueDouble := LTuplaIntDouble.Get<Double>(2);
  Assert.AreEqual(2.2, LValueDouble, 0.01);
end;

procedure TestTuple.TestStrIntTuplaNew;
var
  LTuplaStrInt: TTuple<string>;
  LValueInt: Integer;
begin
  LTuplaStrInt := TTuple<string>.New(['A', 'B', 'C'], ['1', 2.3, 3]);

  Assert.IsTrue(LTuplaStrInt.Get<Double>('B') > 0);

  LValueInt := LTuplaStrInt.Get<Integer>('C');
  Assert.AreEqual(3, LValueInt);
end;

initialization
  TDUnitX.RegisterTestFixture(TestTuple);

end.

