program PCorrotina;

uses
  FastMM5,
  Vcl.Forms,
  UCorrotina in 'UCorrotina.pas' {Form2},
  eclbr.coroutine in '..\Source\eclbr.coroutine.pas',
  eclbr.threading in '..\Source\eclbr.threading.pas',
  eclbr.std in '..\Source\eclbr.std.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
