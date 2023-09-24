unit UTestEclbr.ObjectLib;

interface

uses
  DUnitX.TestFramework,
  eclbr.objectlib;

type
  TMyClass = class
  public
    class function New: TMyClass;
    function GetMessage: string;
  end;

  [TestFixture]
  TTestObectLib = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestOption;
  end;

implementation

procedure TTestObectLib.Setup;
begin

end;

procedure TTestObectLib.TearDown;
begin

end;

procedure TTestObectLib.TestOption;
var
  LOption: IAutoRef<TMyClass>;
begin
  LOption := TAutoRef<TMyClass>.New(TMyClass.New);

  Assert.IsNotNull(LOption.Get);
  Assert.AreEqual('Hello word', LOption.Get.GetMessage);
end;

{ TMyClass }

function TMyClass.GetMessage: string;
begin
  Result := 'Hello word';
end;

class function TMyClass.New: TMyClass;
begin
  Result := TMyClass.Create;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestObectLib);

end.
