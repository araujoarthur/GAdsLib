program GAdsLibFrm;

uses
  Vcl.Forms,
  Forms.Main in 'Forms.Main.pas' {Form1},
  GAdsLib in 'GAdsLib.pas',
  GAdsLib.Utils.SharedResources in 'GAdsLib.Utils.SharedResources.pas',
  GAdsLib.Utils.Types in 'GAdsLib.Utils.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
