unit UTestEclbr.Objects;

interface

uses
  DUnitX.TestFramework,
  eclbr.objects;

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
    procedure TestAutoRef_Create;
    [Test]
    procedure TestAutoRef_New;
    [Test]
    procedure TestAutoRef_LazyLoad;
    [Test]
    procedure TestAutoRefRecord;
  end;

implementation

uses
  SysUtils;

procedure TTestObectLib.Setup;
begin

end;

procedure TTestObectLib.TearDown;
begin

end;

procedure TTestObectLib.TestAutoRef_LazyLoad;
var
  LOption: IAutoRef<TMyClass>;
begin
  LOption := TAutoRef<TMyClass>.LazyLoad;

  Assert.IsNotNull(LOption.Get);
  Assert.AreEqual('Hello word', LOption.Get.GetMessage);
end;

procedure TTestObectLib.TestAutoRef_Create;
var
  LOption: IAutoRef<TMyClass>;
begin
  LOption := TAutoRef<TMyClass>.New(TMyClass.Create());

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

procedure TTestObectLib.TestAutoRefRecord;
var
  LObject1: AutoRef<TMyClass>;
  LObject2: AutoRef<TStringBuilder>;
begin
  LObject1 := TMyClass.Create;
  LObject2 := TStringBuilder.Create;

  Assert.IsFalse(LObject1.IsNull);
  Assert.IsFalse(LObject2.IsNull);

  Assert.IsNotNull(LObject1.Value);
  Assert.IsNotNull(LObject2.Value);
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
