unit UTestEclbr.SysIfTuple;

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
  LTuplaIntDouble: TTuple<Integer, Double>;
  LValueDouble: Double;
begin
  LTuplaIntDouble.Keys([1, 2, 3]).Values([1.1, 2.2, 3.3]);

  Assert.IsTrue(LTuplaIntDouble.Get(2) > 0);

  LValueDouble := LTuplaIntDouble.Get(2);
  Assert.AreEqual(2.2, LValueDouble, 0.01);
end;

procedure TestTuple.TestStrIntTupla;
var
  LTuplaStrInt: TTuple<string, Integer>;
  LValueInt: Integer;
begin
  LTuplaStrInt.Keys(['A', 'B', 'C']).Values([1, 2, 3]);

  Assert.IsTrue(LTuplaStrInt.Get('B') > 0);

  LValueInt := LTuplaStrInt.Get('B');
  Assert.AreEqual(2, LValueInt);
end;

initialization
  TDUnitX.RegisterTestFixture(TestTuple);

end.

