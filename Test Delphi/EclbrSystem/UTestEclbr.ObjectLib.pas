unit UTestEclbr.ObjectLib;

interface

uses
  DUnitX.TestFramework,
  eclbr.objectlib;

type
  TMyClass = class
  public
    destructor Destroy; override;
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
    procedure TestAutoRef_New;
    [Test]
    procedure TestAutoRef_Create;
  end;

implementation

procedure TTestObectLib.Setup;
begin

end;

procedure TTestObectLib.TearDown;
begin

end;

procedure TTestObectLib.TestAutoRef_Create;
var
  LOption: IAutoRef<TMyClass>;
begin
  LOption := TAutoRef<TMyClass>.New;

  Assert.IsNotNull(LOption.Get);
  Assert.AreEqual('Hello word', LOption.Get.GetMessage);
end;

procedure TTestObectLib.TestAutoRef_New;
var
  LOption: IAutoRef<TMyClass>;
begin
  LOption := TAutoRef<TMyClass>.New(TMyClass.New);

  Assert.IsNotNull(LOption.Get);
  Assert.AreEqual('Hello word', LOption.Get.GetMessage);
end;

{ TMyClass }

destructor TMyClass.Destroy;
begin
  // Debugar aqui para verificar se está sendo liberado.
  inherited;
end;

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
