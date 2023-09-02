unit UTestEclbr.SysMatch;

interface

uses
//  Rtti,
  SysUtils,
  Generics.Collections,
  eclbr.sysmatch,
  eclbr.result.pair,
  DUnitX.TestFramework;

type
  TAnimal = class end;
  TDog = class(TAnimal) end;
  TProduct = class
    Name: string;
    Price: Double;
  end;
  TDiscount = class
    function CalculateDiscount(OriginalPrice: Double): Double; virtual; abstract;
  end;
  TPercentageDiscount = class(TDiscount)
    Percentage: Double;
    function CalculateDiscount(OriginalPrice: Double): Double; override;
  end;
  TFixedAmountDiscount = class(TDiscount)
    Amount: Double;
    function CalculateDiscount(OriginalPrice: Double): Double; override;
  end;
  TEnumType = (One, Two, Three);

  TMatchTypeA = class
    // Campos e métodos específicos para o padrão A
  end;
  TMatchTypeA1 = class
    // Campos e métodos específicos para o padrão A1
  end;
  TMatchTypeA2 = class
    // Campos e métodos específicos para o padrão A2
  end;

  [TestFixture]
  TestTMatch = class
  private
    FValue1, FValue2: Integer;
    procedure Proc1;
    procedure Proc2;
    procedure ProcWithParam(Value: Integer);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    [Test]
    procedure TestMatchWithMatchingCase;
    [Test]
    procedure TestMatchWithNoMatchingCase;
    [Test]
    procedure TestMatchWithMultipleCases;
    [Test]
    procedure TestSingleCaseOf;
    [Test]
    Procedure TestCaseEqArrayInteger;
    [Test]
    Procedure TestCaseEqArrayString;
    [Test]
    Procedure TestCaseEqArrayChar;
    [Test]
    procedure TestCaseArray;
    [Test]
    procedure TestSingleCaseOfWithArgument;
    [Test]
    procedure TestCaseMultiple;
    [Test]
    procedure TestRange;
    [Test]
    procedure TestCaseIfAndGuard;
    [Test]
    procedure TestCaseIfOrGuard;
    [Test]
    procedure TestCaseIfNotGuard;
    [Test]
    procedure TestCaseDefault;
    [Test]
    procedure TestCaseTryExcept;
    [Test]
    procedure TestCaseChar;
    [Test]
    procedure TestCaseRegex;
    [Test]
    procedure TestCaseIsMatching;
    [Test]
    procedure TestCaseIsObjectMatching;
    [Test]
    procedure TestPatternCombinator;
    [Test]
    procedure TestMultipleCombines;
    [Test]
    procedure TestEnumPatternMatching;
    [Test]
    procedure TestPatternMatchingWithProduct;
    [Test]
    procedure TestPatternMatchingWithDiscounts;
    [Test]
    procedure TestCaseGtWithInteger;
    [Test]
    procedure TestCaseLtWithInteger;
    [Test]
    procedure TestNestedCases;
    [Test]
    procedure TestCombineWithDefault;
    [Test]
    procedure TestDefaultExecutionFailure;
end;

implementation

uses
  System.Classes;

procedure TestTMatch.Setup;
begin
  FValue1 := 1;
  FValue2 := 2;
end;

procedure TestTMatch.Teardown;
begin

end;

procedure TestTMatch.Proc1;
begin
  FValue1 := 42;
end;

procedure TestTMatch.Proc2;
begin
  FValue2 := 42;
end;

procedure TestTMatch.ProcWithParam(Value: Integer);
begin
  FValue1 := Value * 10;
end;

procedure TestTMatch.TestCaseIfAndGuard;
var
  LResult: TResultPair<boolean, string>;
  LResultString: string;
  LValue: integer;
begin
  LValue := 1;
  LResult := TMatch<Integer>.Value(LValue)
                            .CaseIf((LValue > 0) and (0 < LValue))
                            .CaseEq(1, procedure
                                       begin
                                         LResultString := 'Matched 1 with AndGuard';
                                       end)
                            .Execute;
  try
    Assert.AreEqual('Matched 1 with AndGuard', LResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseArray;
var
  LMatchResult: TResultPair<boolean, string>;
  LIsMatch: boolean;
begin
  LIsMatch := false;
  LMatchResult := TMatch<Integer>.Value(2)
                                 .CaseIn([1, 2, 3], procedure begin LIsMatch := true; end)
                                 .CaseIn([1, 4, 8], procedure begin LIsMatch := false; end)
                                 .CaseIn([1, 9, 6], procedure begin LIsMatch := false; end)
                                 .Execute;
  try
    Assert.IsTrue(LIsMatch, 'Expected match to be successful');
  finally
    LMatchResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseEqArrayChar;
var
  LMatchResult: TResultPair<boolean, string>;
  LIsMatch: boolean;
  LValue: TArray<Char>;
begin
  LIsMatch := false;
  LValue := ['E', 'V', 'N'];

  LMatchResult := TMatch<TArray<Char>>.Value(LValue)
                                  .CaseEq(['V', 'E', 'N'], procedure begin LIsMatch := false; end)
                                  .CaseEq(['E', 'V', 'N'], procedure begin LIsMatch := true; end)
                                  .CaseEq(['N', 'E', 'V'], procedure begin LIsMatch := false; end)
                                  .Default(procedure begin LIsMatch := false; end)
                                  .Execute;
  try
    Assert.IsTrue(LIsMatch, 'Expected match to be successful');
  finally
    LMatchResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseEqArrayInteger;
var
  LMatchResult: TResultPair<boolean, string>;
  LIsMatch: boolean;
  LValue: TArray<integer>;
begin
  LIsMatch := false;
  LValue := [1, 4, 8];

  LMatchResult := TMatch<TArray<integer>>.Value(LValue)
                                  .CaseIf(1 in [2, 8, 1])
                                  .CaseIf(Length(LValue) > 2)
                                  .CaseEq([1, 2, 3], procedure begin LIsMatch := false; end)
                                  .CaseEq([1, 4, 8], procedure begin LIsMatch := true; end)
                                  .CaseEq([1, 9, 6], procedure begin LIsMatch := false; end)
                                  .CaseRegex('email@gmail.com', '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z]{2,}$')
                                  .Default(procedure begin LIsMatch := false; end)
                                  .Execute;
  try
    Assert.IsTrue(LIsMatch, 'Expected match to be successful');
    Assert.IsTrue(LMatchResult.isSuccess, 'Expected match to be successful')
  finally
    LMatchResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseEqArrayString;
var
  LMatchResult: TResultPair<boolean, string>;
  LIsMatch: boolean;
  LValue: TArray<String>;
begin
  LIsMatch := false;
  LValue := ['Eu', 'Você', 'Nós'];

  LMatchResult := TMatch<TArray<String>>.Value(LValue)
                                  .CaseEq(['Você', 'Eu', 'Nós'], procedure begin LIsMatch := false; end)
                                  .CaseEq(['Eu', 'Você', 'Nós'], procedure begin LIsMatch := true; end)
                                  .CaseEq(['Nós', 'Eu', 'Você'], procedure begin LIsMatch := false; end)
                                  .Execute;
  try
    Assert.IsTrue(LIsMatch, 'Expected match to be successful');
  finally
    LMatchResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseGtWithInteger;
var
  LValue: Integer;
  LResult: TResultPair<boolean, string>;
  LIsMatch: boolean;
begin
  LValue := 10;
  LIsMatch := false;

  try
    LResult := TMatch<Integer>.Value(LValue)
      .CaseGt(5, procedure
        begin
          LIsMatch := true;
          Assert.Pass('Value is greater than 5.');
        end)
      .CaseGt(15, procedure
        begin
          LIsMatch := false;
          Assert.Fail('Value is not greater than 15.');
        end)
      .Execute;

    Assert.IsTrue(LIsMatch, 'Pattern matching failed.');
    Assert.AreEqual('Value is greater than 5.', LResult.ValueFailure, 'Pattern matching failed.');
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseChar;
var
  LMatchResult: TResultPair<boolean, string>;
  isMatch: boolean;
begin
  isMatch := false;
  try
    LMatchResult := TMatch<Char>.Value('A')
                                .CaseIn(['A', 'B', 'C'], procedure begin isMatch := true; end)
                                .CaseIn(['B', 'C'], procedure begin isMatch := false; end)
                                .CaseIn(['C'], procedure begin isMatch := false; end)
                                .Execute;

    Assert.IsTrue(isMatch, 'Expected match to be successful');
  finally
    LMatchResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseDefault;
var
  LMatchResult: TResultPair<boolean, string>;
  isMatch: boolean;
begin
  isMatch := false;
  try
    LMatchResult := TMatch<Integer>.Value(42)
                                   .CaseEq(1, procedure begin isMatch := false; end)
                                   .CaseEq(2, procedure begin isMatch := false; end)
                                   .Default(procedure begin isMatch := true; end)
                                   .Execute;

    Assert.IsTrue(isMatch, 'Expected match to be successful');
  finally
    LMatchResult.Dispose;
  end;
end;

procedure TestTMatch.TestMatchWithMatchingCase;
var
  LResult: TResultPair<boolean, string>;
begin
  LResult := TMatch<Integer>.Value(FValue1)
                           .CaseEq(1, Proc1)
                           .Execute;
  try
    Assert.AreEqual(42, FValue1);
    Assert.AreEqual(2, FValue2);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestMatchWithNoMatchingCase;
var
  LMatchResult: TResultPair<boolean, string>;
begin
  LMatchResult := TMatch<Integer>.Value(FValue2)
                     .CaseEq(2, Proc2)
                     .Execute;
  try
    Assert.IsFalse(LMatchResult.isFailure, 'Expected match to be successful');
  finally
    LMatchResult.Dispose;
  end;
end;

procedure TestTMatch.TestNestedCases;
var
  LResult: TResultPair<boolean, string>;
  LPattern: TMatch<TObject>;
  LTypeA: TMatchTypeA;
begin
  LTypeA := TMatchTypeA.Create;
  try
    LPattern := TMatch<TObject>.Value(LTypeA);

    LResult := LPattern.CaseIs<TMatchTypeA>(procedure(PatternTypeA: TMatchTypeA)
      begin
        LPattern.CaseIs<TMatchTypeA1>(procedure(PatternTypeA1: TMatchTypeA1)
          begin
            Assert.Pass('Pattern A1 successfully matched.');
          end);
        LPattern.CaseIs<TMatchTypeA2>(procedure(PatternTypeA2: TMatchTypeA2)
          begin
            Assert.Pass('Pattern A2 successfully matched.');
          end);
      end)
      .Execute;
  finally
    LTypeA.Free;
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestPatternMatchingWithDiscounts;
var
  LProduct: TProduct;
  LDiscount: TDiscount;
  LResult:  TResultPair<boolean, string>;
begin
  LProduct := TProduct.Create;
  LProduct.Name := 'Sample Product';
  LProduct.Price := 100.0;

  LDiscount := TPercentageDiscount.Create;
  (LDiscount as TPercentageDiscount).Percentage := 10;

  try
    LResult := TMatch<TProduct>.Value(LProduct)
      .CaseIs<TPercentageDiscount>(
        procedure(Discount: TPercentageDiscount)
        var
          DiscountedPrice: Double;
        begin
          DiscountedPrice := LProduct.Price - (LProduct.Price * Discount.Percentage / 100);
          Assert.AreEqual(90.0, DiscountedPrice, 0.01, 'Percentage discount calculation incorrect.');
        end)
      .CaseIs<TFixedAmountDiscount>(
        procedure(Discount: TFixedAmountDiscount)
        var
          DiscountedPrice: Double;
        begin
          DiscountedPrice := LProduct.Price - Discount.Amount;
          Assert.AreEqual(80.0, DiscountedPrice, 0.01, 'Fixed amount discount calculation incorrect.');
        end)
      .Default(
        procedure
        begin
          Assert.Fail('No matching case found.');
        end)
      .Execute;

    Assert.IsTrue(LResult.isSuccess, 'Pattern matching failed.');
  finally
    LProduct.Free;
    LDiscount.Free;
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestPatternMatchingWithProduct;
var
  LProduct: TProduct;
  LDiscount: TDiscount;
  LResult:  TResultPair<boolean, string>;
begin
  LProduct := TProduct.Create;
  LProduct.Name := 'Sample Product';
  LProduct.Price := 100.0;
  LDiscount := TPercentageDiscount.Create;
  try
    (LDiscount as TPercentageDiscount).Percentage := 10;

    LResult := TMatch<TProduct>.Value(LProduct)
      .CaseIs<TPercentageDiscount>(procedure(LDiscount: TPercentageDiscount)
        var
          LDiscountedPrice: Double;
        begin
          LDiscountedPrice := LProduct.Price - (LProduct.Price * LDiscount.Percentage / 100);
          Assert.AreEqual(90.0, LDiscountedPrice, 0.01, 'Percentage discount calculation incorrect.');
        end)
        .Default(procedure
        begin
          Assert.Fail('No matching case found.');
        end)
        .Execute;

    Assert.IsTrue(LResult.isSuccess, 'Pattern matching failed.');
  finally
    LProduct.Free;
    LDiscount.Free;
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseMultiple;
var
  LResult: TResultPair<boolean, string>;
  LResultString: string;
begin
  LResult := TMatch<Integer>.Value(2)
                           .CaseIn([1, 2], procedure begin LResultString := 'Matched 1 or 2'; end)
                           .Execute;
  try
    Assert.AreEqual('Matched 1 or 2', LResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseIfNotGuard;
var
  LResultString: string;
  LValue: integer;
  LResult: TResultPair<boolean, string>;
begin
  LValue := 1;
  try
    LResult := TMatch<Integer>.Value(LValue)
                             .CaseIf((LValue = 1) and not (LValue <> 1))
                             .CaseEq(1, procedure begin LResultString := 'Matched 1 with NotGuard'; end)
                             .Execute;
    Assert.AreEqual('Matched 1 with NotGuard', LResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseIfOrGuard;
var
  LResult: TResultPair<boolean, string>;
  LResultString: string;
  LValue: integer;
begin
  LValue := 1;
  LResult := TMatch<Integer>.Value(LValue)
                           .CaseIf((LValue = 1) or (LValue < 0))
                           .CaseEq(1, procedure begin LResultString := 'Matched 1 with OrGuard'; end)
                           .Execute;
  try
    Assert.AreEqual('Matched 1 with OrGuard', LResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestRange;
var
  LResult: TResultPair<boolean, string>;
  LResultString: string;
begin
  LResult := TMatch<Integer>.Value(5)
                   .CaseRange(1, 10, procedure begin LResultString := 'Matched range 1-10'; end)
                   .Execute;
  try
    Assert.AreEqual('Matched range 1-10', LResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseRegex;
var
  LMatchResult: TResultPair<boolean, string>;
  LIsMatch: boolean;
begin
  LIsMatch := false;
  try
    LMatchResult := TMatch<Integer>.Value(2)
                                    .CaseIn([1, 2, 3], procedure begin LIsMatch := true; end)
                                    .CaseIn([1, 4, 8], procedure begin LIsMatch := false; end)
                                    .CaseIn([1, 9, 6], procedure begin LIsMatch := false; end)
                                    .CaseRegex('email@gmail.com', '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z]{2,}$')
                                    .Execute;

    Assert.IsTrue(LIsMatch, 'Expected match to be successful');
  finally
    LMatchResult.Dispose;
  end;
end;

procedure TestTMatch.TestSingleCaseOf;
var
  LResult: TResultPair<boolean, string>;
  LResultString: string;
begin
  LResult := TMatch<Integer>.Value(1)
                           .CaseEq(1, procedure begin LResultString := 'Matched 1'; end)
                           .Execute;
  try
    Assert.AreEqual('Matched 1', LResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestSingleCaseOfWithArgument;
var
  LResult: TResultPair<boolean, string>;
  LResultString: string;
begin
  LResult := TMatch<Integer>.Value(1)
                           .CaseEq(1, procedure(Arg: Integer) begin LResultString := Format('Matched %d', [Arg]); end)
                           .Execute;
  try
    Assert.AreEqual('Matched 1', LResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseTryExcept;
var
  LMatchResult: TResultPair<boolean, string>;
  isMatch: boolean;
begin
  isMatch := false;
  try
    LMatchResult := TMatch<Integer>.Value(42)
                                     .CaseEq(1, procedure begin isMatch := false; end)
                                     .CaseEq(42, procedure begin raise Exception.Create('TryExcept'); end)
                                     .TryExcept(procedure begin isMatch := true; end)
                                     .Execute;

    Assert.AreEqual('TryExcept', LMatchResult.ValueFailure, 'Expected match to be successful');
    Assert.IsTrue(LMatchResult.isFailure, 'Expected match to be successful');
    Assert.IsTrue(isMatch, 'Expected match to be successful');
  finally
    LMatchResult.Dispose;
  end;
end;

procedure TestTMatch.TestEnumPatternMatching;
var
  LResult: TResultPair<boolean, string>;
  EnumValue: TEnumType;
  ResultString: string;
begin
  EnumValue := TEnumType.Two;

  LResult := TMatch<TEnumType>.Value(EnumValue)
    .CaseEq(TEnumType.One, procedure
      begin
        ResultString := 'EnumValue is One';
      end)
    .CaseEq(TEnumType.Two, procedure
      begin
        ResultString := 'EnumValue is Two';
      end)
    .CaseEq(TEnumType.Three, procedure
      begin
        ResultString := 'EnumValue is Three';
      end)
    .Default(procedure
      begin
        ResultString := 'EnumValue is not recognized';
      end)
    .Execute;

  try
    Assert.AreEqual('EnumValue is Two', ResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseIsMatching;
var
  LResult: TResultPair<boolean, string>;
  LValueList: TArray<Integer>;
  ResultString: string;
begin
  LValueList := [42, 36];

  LResult := TMatch<integer>.Value(LValueList[0])
                .CaseIs<Integer>(procedure(Value: Integer)
                  begin
                    ResultString := 'Value is an Integer: ' + Value.ToString;
                  end)
                .CaseIs<string>(procedure(Value: string)
                  begin
                    ResultString := 'Value is a String: ' + Value;
                  end)
                .CaseIs<TDateTime>(procedure(Value: TDateTime)
                  begin
                    ResultString := 'Value is a DateTime: ' + DateTimeToStr(Value);
                  end)
                .Default(procedure
                  begin
                    ResultString := 'Value is of an unknown type';
                  end)
                .Execute;

  try
    Assert.AreEqual('Value is an Integer: 42', ResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseIsObjectMatching;
var
  LResult: TResultPair<boolean, string>;
  LValueList: TArray<TAnimal>;
  ResultString: string;
begin
  LValueList := [TDog.Create];
  try
    LResult := TMatch<TAnimal>.Value(LValueList[0])
      .CaseIs<Integer>(procedure(Value: Integer)
        begin
          ResultString := 'Value is an Integer: ' + Value.ToString;
        end)
      .CaseIs<string>(procedure(Value: string)
        begin
          ResultString := 'Value is a String: ' + Value;
        end)
      .CaseIs<TDateTime>(procedure(Value: TDateTime)
        begin
          ResultString := 'Value is a DateTime: ' + DateTimeToStr(Value);
        end)
      .CaseIs<TAnimal>(procedure(Value: TAnimal)
        begin
          ResultString := 'Value is a Object: ' + Value.ClassName;
        end)
      .Default(procedure
        begin
          ResultString := 'Value is of an unknown type';
        end)
      .Execute;

    Assert.AreEqual('Value is a Object: TDog', ResultString);
  finally
    LValueList[0].Free;
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCaseLtWithInteger;
var
  LValue: Integer;
  LResult: TResultPair<boolean, string>;
  LIsMatch: boolean;
begin
  LValue := 10;
  LIsMatch := false;
  try
    LResult := TMatch<Integer>.Value(LValue)
      .CaseLt(15, procedure
        begin
          LIsMatch := true;
          Assert.Fail('Value is not greater than 15.');
        end)
      .CaseLt(5, procedure
        begin
          LIsMatch := false;
          Assert.Pass('Value is greater than 5.');
        end)
      .Execute;

    Assert.IsTrue(LIsMatch, 'Pattern matching failed.');
    Assert.AreEqual('Value is not greater than 15.', LResult.ValueFailure, 'Pattern matching failed.');
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestPatternCombinator;
var
  LResult: TResultPair<boolean, string>;
  LValueString: String;
  LResultString: string;
  LStrPattern: TMatch<String>;
begin
  LValueString := 'Hello';

  LStrPattern := TMatch<String>.Value(LValueString)
                                 .CaseIs<String>(procedure
                                   begin
                                     LResultString := 'Value is a String';
                                   end);

  LResult := TMatch<String>.Value(LValueString)
                .Combine(LStrPattern)
                .Default(procedure
                    begin
                      LResultString := 'Value is of an unknown type';
                    end)
                .Execute;
  try
    Assert.AreEqual('Value is a String', LResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestMultipleCombines;
var
  LValueString: String;
  LResultString: string;
  LStrPattern1, LStrPattern2: TMatch<String>;
  LResult: TResultPair<boolean, string>;
begin
  LValueString := 'Hello';

  // Padrão de combinação 1
  LStrPattern1 := TMatch<String>.Value(LValueString)
//    .CaseIf(false)
    .CaseIs<Integer>(procedure
      begin
        LResultString := 'Type is a String';
      end);

  // Padrão de combinação 2
  LStrPattern2 := TMatch<String>.Value(LValueString)
    .CaseEq('Hello', procedure
      begin
        LResultString := 'Value is "Hello"';
      end);

  // Padrão combinado
  try
    LResult := TMatch<String>.Value(LValueString)
      .Combine(LStrPattern1)
      .Combine(LStrPattern2)
      .Default(procedure
        begin
          LResultString := 'Value is of an unknown type';
        end)
      .Execute;

    Assert.AreEqual('Value is "Hello"', LResultString);
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestCombineWithDefault;
var
  LValueString: String;
  LResultString: string;
  LStrPattern1, LStrPattern2: TMatch<String>;
  LCombinedPattern: TResultPair<boolean, string>;
begin
  LValueString := 'Hello';

  // Padrão de combinação 1
  LStrPattern1 := TMatch<String>.Value(LValueString)
    .CaseIs<Integer>(procedure
      begin
        LResultString := 'Type is a String';
      end);

  // Padrão de combinação 2
  LStrPattern2 := TMatch<String>.Value(LValueString)
    .CaseEq('World', procedure
      begin
        LResultString := 'Value is "World"';
      end);

  // Padrão combinado
  try
    LCombinedPattern := TMatch<String>.Value(LValueString)
      .Combine(LStrPattern1)
      .Combine(LStrPattern2)
      .Default(procedure
        begin
          LResultString := 'Value is not matched by any pattern';
        end)
      .Execute;

    Assert.AreEqual('Value is not matched by any pattern', LResultString);
  finally
    LCombinedPattern.Dispose;
  end;
end;

procedure TestTMatch.TestDefaultExecutionFailure;
var
  LValue: Integer;
  LResult: TResultPair<boolean, string>;
begin
  LValue := 100;

  try
    LResult := TMatch<Integer>.Value(LValue)
      .CaseEq(42, procedure
        begin
          // Caso correspondente
        end)
      .CaseLt(50, procedure
        begin
          // Caso correspondente
        end)
      .Execute;

    Assert.IsFalse(LResult.isSuccess, 'Expected match to fail');
    Assert.IsTrue(LResult.isFailure, 'Expected match to fail');
  finally
    LResult.Dispose;
  end;
end;

procedure TestTMatch.TestMatchWithMultipleCases;
var
  LResult: TResultPair<boolean, string>;
begin
  LResult := TMatch<Integer>.Value(FValue1)
                           .CaseEq(1, ProcWithParam)
                           .CaseEq(2, Proc2)
                           .Execute;
  try
    Assert.AreEqual(10, FValue1);
    Assert.AreEqual(2, FValue2);
  finally
    LResult.Dispose;
  end;
end;

function TPercentageDiscount.CalculateDiscount(OriginalPrice: Double): Double;
begin
  Result := OriginalPrice * Percentage / 100;
end;

function TFixedAmountDiscount.CalculateDiscount(OriginalPrice: Double): Double;
begin
  Result := Amount;
end;

initialization
  TDUnitX.RegisterTestFixture(TestTMatch);

end.
