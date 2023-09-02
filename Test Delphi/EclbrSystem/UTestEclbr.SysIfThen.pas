unit UTestEclbr.SysIfThen;

interface

uses
  DUnitX.TestFramework, eclbr.sysifthen;

type
  [TestFixture]
  TestTIfThen = class
  public
    [Test]
    procedure TestIfThenWithTrueCondition;
    [Test]
    procedure TestIfThenWithFalseCondition;
    [Test]
    procedure TestIfThenWithFalseMultipleCondition;
    [Test]
    procedure TestWhenTrueThenString;
    [Test]
    procedure TestWhenTrueThenBoolean;
    [Test]
    procedure TestWithCondition;
    [Test]
    procedure TestWithConditionFunc;
    [Test]
    procedure TestIfThenWithMultipleElseOf;
    [Test]
    procedure TestMultipleIfThens;
  end;

implementation

procedure TestTIfThen.TestIfThenWithFalseMultipleCondition;
var
  LValue: Integer;
begin
// ECLBr lib

// ==========================Option 1
// LValue := TIfThen<Integer>.When(-1 > 0).ThenOf(42).ElseOf(0 > 1, 45).ElseOf(1 > 0, 42).Return;

// ==========================Option 1
 LValue := TIfThen<Integer>.When(-1 > 0)
                           .ThenOf(42).ElseOf(0 > 1, 45)
                           .ElseOf(1 > 0, 42)
                           .Return;

// NATIVE

// =========================Option 1
//if (-1 > 0) then LValue := 42 else if (0 > 1) then LValue := 45 else if (1 > 0) then LValue := 42;

// =========================Option 2
//if     (-1 > 0) then LValue := 42
//else if (0 > 1) then LValue := 45
//else if (1 > 0) then LValue := 42;

// =========================Option 3
//   if (-1 > 0) then
//     LValue := 42
//   else if 0 > 1 then
//     LValue := 45
//   else if 1 > 0 then
//     LValue := 42;

  Assert.AreEqual(42, LValue);

  if not False then if 1 > 0 then LValue := 42 else if 0 > 1 then LValue := 45;

end;

procedure TestTIfThen.TestIfThenWithMultipleElseOf;
var
  LResult: Integer;
  LValue: Integer;
begin
  LValue := 500;

  LResult := TIfThen<Integer>.When(False)
    .ThenOf(42)
    .ElseOf(LValue > 500,
      function: Integer
      begin
        Result := 99;
      end)
    .ElseOf(LValue < 1000,
      function: Integer
      begin
        Result := 1000;
      end)
    .Return;

  Assert.AreEqual(1000, LResult);
end;

procedure TestTIfThen.TestIfThenWithTrueCondition;
var
  LValue: Integer;
begin
  LValue := TIfThen<Integer>.When(True).ThenOf(42).ElseOf(45).Return;

  Assert.AreEqual(42, LValue);
end;

procedure TestTIfThen.TestMultipleIfThens;
var
  LResultValue1: Integer;
  LResultValue2: Integer;
begin
  LResultValue1 := TIfThen<Integer>
    .When(
      function: Boolean
      begin
        Result := True;
      end)
    .ThenOf(
      function: Integer
      begin
        Result := 42;
      end)
    .ElseOf(
      function: Integer
      begin
        Result := 99;
      end)
    .Return;

  Assert.AreEqual(42, LResultValue1);

  LResultValue2 := TIfThen<Integer>
    .When(
      function: Boolean
      begin
        Result := True;
      end)
    .ThenOf(
      function: Integer
      begin
        Result := 99;
      end)
    .ElseOf(
      function: Integer
      begin
        Result := 42;
      end)
    .Return;

  Assert.AreEqual(99, LResultValue2);
end;

procedure TestTIfThen.TestWhenTrueThenBoolean;
var
  LValue: Boolean;
begin
  LValue := TIfThen<Boolean>.When(True)
    .ThenOf(True)
    .ElseOf(False)
    .Return;

  Assert.IsTrue(LValue);
end;

procedure TestTIfThen.TestWhenTrueThenString;
var
  LValue: string;
begin
  LValue := TIfThen<string>.When(True)
    .ThenOf('True value')
    .ElseOf('False value')
    .Return;

  Assert.AreEqual('True value', LValue);
end;

procedure TestTIfThen.TestWithCondition;
var
  ResultValue: Integer;
begin
  ResultValue := TIfThen<Integer>
    .When(True)
    .ThenOf(
      function: Integer
      begin
        Result := 42;
      end)
    .ElseOf(
      function: Integer
      begin
        Result := 99;
      end)
    .Return;

  Assert.AreEqual(42, ResultValue);
end;

procedure TestTIfThen.TestWithConditionFunc;
var
  LResultValue: Integer;
begin
  LResultValue := TIfThen<Integer>
    .When(
      function: Boolean
      begin
        Result := True;
      end)
    .ThenOf(
      function: Integer
      begin
        Result := 42;
      end)
    .ElseOf(
      function: Integer
      begin
        Result := 99;
      end)
    .Return;

  Assert.AreEqual(42, LResultValue);
end;

procedure TestTIfThen.TestIfThenWithFalseCondition;
var
  LValue: Integer;
begin
  LValue := TIfThen<Integer>.When(False).ThenOf(42).ElseOf(0).Return;

  Assert.AreEqual(0, LValue);
end;

initialization
  TDUnitX.RegisterTestFixture(TestTIfThen);

end.
