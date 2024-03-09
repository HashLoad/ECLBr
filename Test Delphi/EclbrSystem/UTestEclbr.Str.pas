unit UTestEclbr.Str;

interface

uses
  DUnitX.TestFramework,
  SysUtils,
  DateUtils,
  Classes,
  Generics.Collections;

type
  [TestFixture]
  TTesTStd = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

implementation

uses
  eclbr.str;

procedure TTesTStd.Setup;
begin

end;

procedure TTesTStd.TearDown;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TTesTStd);

end.

