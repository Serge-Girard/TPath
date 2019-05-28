unit SelColorUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Colors;

type
  TFormSelectColor = class(TForm)
    ColorListBox1: TColorListBox;
    btnOk: TCornerButton;
    btnCancel: TCornerButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormSelectColor: TFormSelectColor;

implementation

{$R *.fmx}

procedure TFormSelectColor.btnCancelClick(Sender: TObject);
begin
ModalResult:=mrcancel;
end;

procedure TFormSelectColor.btnOkClick(Sender: TObject);
begin
ModalResult:=mrOk;
end;

procedure TFormSelectColor.FormCreate(Sender: TObject);
begin
ColorListBox1.Color:=TAlphacolors.White;
end;

end.
