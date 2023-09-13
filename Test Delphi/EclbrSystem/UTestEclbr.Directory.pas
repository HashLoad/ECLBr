unit UTestEclbr.Directory;

interface

uses
  DUnitX.TestFramework,
  Rtti,
  SysUtils,
  Generics.Collections;

type
  TDirectoryExTest = class
  private
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
  end;

implementation

{ TArrayDataTest }

procedure TDirectoryExTest.Setup;
begin

end;

procedure TDirectoryExTest.TearDown;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TDirectoryExTest);

end.

