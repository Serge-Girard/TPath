unit PathUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Data.Bind.EngExt, FMX.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  FMX.Bind.Editors, FMX.StdCtrls, Data.Bind.Components, FMX.Colors,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Objects, System.Math.Vectors,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.EditBox, FMX.ComboTrackBar,
  FMX.Layouts, System.ImageList, FMX.ImgList, Data.Bind.ObjectScope,
  FMX.ExtCtrls, FMX.TabControl;

type
  TPathGradient = record
    Fill: TBrush;
    FillAngle: Single;
    Stroke: TStrokeBrush;
    StrokeAngle: Single;
  end;

  TMainForm = class(TForm)
    Path1: TPath;
    GradientChkBox: TCheckBox;
    Memo1: TMemo;
    GradientEdit1: TGradientEdit;
    ColorPicker1: TColorPicker;
    ColorQuad1: TColorQuad;
    ColorBox1: TColorBox;
    btnRepaint: TButton;
    Remplissage: TComboBox;
    ColorDialogue: TGroupBox;
    Objet: TComboBox;
    AngleGradient: TArcDial;
    Epaisseur: TComboTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    btnSave: TButton;
    SaveDialog1: TSaveDialog;
    PathContainer: TLayout;
    Largeur: TTrackBar;
    Hauteur: TTrackBar;
    ImageList1: TImageList;
    btnLien: TSpeedButton;
    LabelTaille: TLabel;
    ColorAlpha: TComboTrackBar;
    Label5: TLabel;
    RotationDial: TArcDial;
    btnLoad: TButton;
    OpenDialog1: TOpenDialog;
    Text1: TText;
    Text2: TText;
    Label1: TLabel;
    PositionRotation: TComboBox;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Path2: TPath;
    btnAppliquer: TButton;
    Label4: TLabel;
    BtnLoadBitmap: TButton;
    FileOpen: TOpenDialog;
    Label6: TLabel;
    procedure GradientChkBoxChange(Sender: TObject);
    procedure ColorQuad1Change(Sender: TObject);
    procedure btnRepaintClick(Sender: TObject);
    procedure RemplissageChange(Sender: TObject);
    procedure ObjetChange(Sender: TObject);
    procedure AngleGradientChange(Sender: TObject);
    procedure EpaisseurChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLienClick(Sender: TObject);
    procedure HauteurChange(Sender: TObject);
    procedure LargeurChange(Sender: TObject);
    procedure ColorAlphaChange(Sender: TObject);
    procedure RotationDialChange(Sender: TObject);
    procedure GradientEdit1Change(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure Memo1ChangeTracking(Sender: TObject);
    procedure PositionRotationChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure btnAppliquerClick(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure BtnLoadBitmapClick(Sender: TObject);
  private
    { Déclarations privées }
    memPG: TPathGradient;
    function RecalcAngle(const Gradient: TGradient): Single;
    procedure CalcPosition(Angle: Single; Gradient: TGradient);
    procedure RedrawPath;
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses SelColorUnit, System.RegularExpressions;

procedure TMainForm.AngleGradientChange(Sender: TObject);
// Changement de l'angle du gradient linéaire
begin
  if Objet.ItemIndex = 0 then // Remplissage (Fill)
  begin
    CalcPosition(AngleGradient.Value, memPG.Fill.Gradient);
    memPG.FillAngle := AngleGradient.Value;
  end
  else
  begin // Contour (Stroke)
    CalcPosition(AngleGradient.Value, memPG.Stroke.Gradient);
    memPG.StrokeAngle := AngleGradient.Value;
  end;
  RedrawPath; // Redessiner
end;

procedure TMainForm.btnRepaintClick(Sender: TObject);
// Charge le nouveau dessin contenu dans le mémo
begin
  Path1.BeginUpdate;
  Path1.Data.Data := Memo1.Text;
  Path1.EndUpdate;
  btnRepaint.Visible := False;
end;

procedure TMainForm.btnSaveClick(Sender: TObject);
// Sauvegarde du dessin dans un fichier
var
  ABitmap: TBitmap; // un bitmap
  ACodec: TBitmapCodecSaveParams; // Qualité
  ACodecManager: TBitmapCodecManager; // filtres possibles

  procedure ChangeCouleur(aBmp: TBitmap;
    FromColor: TAlphaColor = TAlphaColors.Null;
    ToColor: TAlphaColor = TAlphaColors.Null);
  // Changer une couleur particulière
  var
    Data: TBitmapData;
    X, Y: Integer;
    AColor: TAlphaColor;
  begin
    aBmp.Map(TMapAccess.ReadWrite, Data); // récupère les pixels
    if FromColor = TAlphaColors.Null then
      FromColor := Data.GetPixel(0, 0); // premier point = transparence
    for X := 0 to aBmp.Height do
      for Y := 0 to aBmp.Width do
      begin
        AColor := Data.GetPixel(Y, X);
        if AColor = FromColor then
          Data.SetPixel(Y, X, ToColor); // modification
      end;
    aBmp.UnMap(Data); // Applique les modifications
  end;

begin
  ACodecManager := TBitmapCodecManager.Create;
  try
    // Fourni les filtres possibles
    SaveDialog1.Filter := ACodecManager.GetFilterString;

    if SaveDialog1.Execute then
    begin
      ACodec.Quality := 100;
      ABitmap := PathContainer.MakeScreenshot; // Récupère le dessin
      if Sametext(ExtractFileExt(SaveDialog1.FileName), '.bmp') then
      begin
        // si bmp demande quelle sera la couleur de fond
        if FormSelectColor.ShowModal = mrOK then
          ChangeCouleur(ABitmap, TAlphaColors.Null,
            FormSelectColor.ColorListBox1.Color);
      end;
      try
        ABitmap.SaveToFile(SaveDialog1.FileName, @ACodec); // sauvegarde
      finally
        FreeAndNil(ABitmap);
      end;
    end;
  finally
    FreeAndNil(ACodecManager);
  end;
end;

procedure TMainForm.CalcPosition(Angle: Single; Gradient: TGradient);
// calcul des positions de début et de fin en fonction d'un angle
var
  X, Y, Koef: Single;
  Radian: Single;
  CosRadian: Single;
  SinRadian: Single;
begin
  Radian := DegToRad(Angle);
  CosRadian := Cos(Radian);
  SinRadian := Sin(Radian);
  if (CosRadian <> 0) and (Abs(1 / CosRadian) >= 1) and
    (Abs(1 / CosRadian) <= 1.42) then
    X := Abs(1 / CosRadian)
  else
    X := 1;

  if (SinRadian <> 0) and (Abs(1 / SinRadian) >= 1) and
    (Abs(1 / SinRadian) <= 1.42) then
    Y := Abs(1 / SinRadian)
  else
    Y := 1;

  Koef := Max(X, Y);
  Koef := Koef * 0.5;
  Gradient.StartPosition.Point := PointF(0.5 - (CosRadian * Koef),
    0.5 + (SinRadian * Koef));
  Gradient.StopPosition.Point := PointF(0.5 + (CosRadian * Koef),
    0.5 - (SinRadian * Koef));
end;

procedure TMainForm.ColorAlphaChange(Sender: TObject);
// Changement de la transparence
begin
  ColorQuad1.Alpha := ColorAlpha.Value;
end;

procedure TMainForm.ColorQuad1Change(Sender: TObject);
// Changement de couleur
begin
  GradientEdit1.BeginUpdate;
  GradientEdit1.Gradient.Points[GradientEdit1.CurrentPoint].Color :=
    ColorPicker1.ColorQuad.ColorBox.Color;
  GradientEdit1.EndUpdate;
  if GradientChkBox.IsChecked then // en cas de gradient
  begin
    if Objet.ItemIndex = 0 then // remplissage
      memPG.Fill.Gradient := GradientEdit1.Gradient
    else // contour
      memPG.Stroke.Gradient := GradientEdit1.Gradient;
  end
  else
  begin // monochrome
    if Objet.ItemIndex = 0 then
      memPG.Fill.Color := ColorBox1.Color // remplissage
    else
      memPG.Stroke.Color := ColorBox1.Color; // contour
  end;
  RedrawPath;
end;

procedure TMainForm.ComboBox1Change(Sender: TObject);
// Chargement d'une image à partir d'une resssource
// utilisation de l'image pour le remplissage
var
  InStream: TResourceStream;
begin
  InStream := TResourceStream.Create(HInstance,
    ComboBox1.Items[ComboBox1.ItemIndex], RT_RCDATA);
  try
    Path2.Fill.Bitmap.Bitmap.LoadFromStream(InStream);
  finally
    FreeAndNil(InStream);
  end;
end;

procedure TMainForm.EpaisseurChange(Sender: TObject);
// Changement épaisseur du contour
begin
  Path1.Stroke.Thickness := Trunc(Epaisseur.Value);
end;

procedure TMainForm.RedrawPath;
// Redessine le composant
begin
  Path1.BeginUpdate;
  Path1.Data.Data := Memo1.Lines.Text;
  Path1.EndUpdate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
// Initialisation
begin
  Hauteur.Value := 200; // curseur de hauteur d'image
  Largeur.Value := 200; // curseur de largeur d'image
  Memo1.Lines.Text := Path1.Data.Data; // Love Delphi
  Path1.Fill.Color := TAlphaColors.Null; // pas de remplissage
  ColorBox1.Color := Path1.Fill.Color; // initialisation colorbox
  GradientEdit1.BeginUpdate; // initialisation GradientEdit
  GradientEdit1.Gradient.Color1 := TAlphaColors.White; // couleur de fin : blanc
  GradientEdit1.EndUpdate;
  // memPG mémorisation des gradients
  memPG.Fill := TBrush.Create(TBrushKind.Solid, Path1.Fill.Color);
  memPG.Stroke := TStrokeBrush.Create(TBrushKind.Solid, Path1.Stroke.Color);
  memPG.Fill := Path1.Fill;
  memPG.Stroke := Path1.Stroke;
end;

function TMainForm.RecalcAngle(const Gradient: TGradient): Single;
// recalcul de l'angle du gradient
// inutile puisque mémorisé dans memPG
begin
  With Gradient do
  begin
    Result := -RadToDeg(ArcTan((StopPosition.Y - StartPosition.Y) /
      (StopPosition.X - StartPosition.X)));
    if StopPosition.X - StartPosition.X <= 0 then
      Result := Result - 180;
  end;
end;

procedure TMainForm.RemplissageChange(Sender: TObject);
// Changement de type de remplissage du gradient
begin
  RotationDial.Visible := Remplissage.ItemIndex = 1;
  AngleGradient.Visible := Remplissage.ItemIndex = 0;
  case Remplissage.ItemIndex of
    0:
      begin
        if Objet.ItemIndex = 0 then
          memPG.Fill.Gradient.Style := TGradientStyle.Linear
        else
          memPG.Stroke.Gradient.Style := TGradientStyle.Linear;
      end;
    1:
      begin
        if Objet.ItemIndex = 0 then
          memPG.Fill.Gradient.Style := TGradientStyle.Radial
        else
          memPG.Stroke.Gradient.Style := TGradientStyle.Radial;
      end;
  end;
  RedrawPath;
end;

procedure TMainForm.RotationDialChange(Sender: TObject);
// rotation du gradient
begin
  if Remplissage.ItemIndex = 0 then // linéaire
  begin
    if Objet.ItemIndex = 0 then
    begin
      CalcPosition(AngleGradient.Value, memPG.Fill.Gradient);
    end
    else
    begin
      CalcPosition(AngleGradient.Value, memPG.Stroke.Gradient);
    end;
  end
  else // Radial
  begin
    // on joue sur Gradient.RadialTransform.RotationAngle
    if Objet.ItemIndex = 0 then
      memPG.Fill.Gradient.RadialTransform.RotationAngle := RotationDial.Value
    else
      memPG.Stroke.Gradient.RadialTransform.RotationAngle := RotationDial.Value;
  end;
  RedrawPath;
end;

procedure TMainForm.TabControl1Change(Sender: TObject);
// Tableau des modifications
begin
  if TabControl1.TabIndex = 1 then
  begin
    if not Path2.Fill.Bitmap.Bitmap.IsEmpty then
    begin
      memPG.Fill.Kind := TBrushKind.Bitmap;
      memPG.Fill.Bitmap.Bitmap := Path2.Fill.Bitmap.Bitmap;
    end;
  end
  else
  begin
    if GradientChkBox.IsChecked then
      memPG.Fill.Kind := TBrushKind.Gradient
    else
      memPG.Fill.Kind := TBrushKind.Solid;
  end;
  RedrawPath;
end;

procedure TMainForm.btnAppliquerClick(Sender: TObject);
// Appliquer un fond image
begin
  memPG.Fill.Kind := TBrushKind.Bitmap;
  memPG.Fill.Bitmap.Bitmap := Path2.Fill.Bitmap.Bitmap;
  RedrawPath;
end;

procedure TMainForm.btnLienClick(Sender: TObject);
// Associer/dissocier les réglages de hauteur et largeur de l'image
begin
  if btnLien.Imageindex = 0 then
    btnLien.Imageindex := 1
  else
    btnLien.Imageindex := 0;
end;

procedure TMainForm.BtnLoadBitmapClick(Sender: TObject);
Var
  ACodecManager: TBitmapCodecManager;
begin
  ACodecManager := TBitmapCodecManager.Create;
  try
    FileOpen.Filter := ACodecManager.GetFilterString;
    if FileOpen.Execute then
      Path2.Fill.Bitmap.Bitmap.LoadFromFile(FileOpen.FileName);
  finally
    FreeAndNil(ACodecManager);
  end;
end;

procedure TMainForm.btnLoadClick(Sender: TObject);
// Charger un fichier SVG
// Attention tous les SVG ne sont pas forcément possible à traiter
// chargement très basique à améliorer
var
  SL: TStringList;
  Match: TMatch;
begin
  if OpenDialog1.Execute then
  begin
    Path1.BeginUpdate;
    Path1.Data.Clear;
    Path1.Fill.Kind := TBrushKind.Solid;
    Path1.Fill.Color := Path1.Fill.DefaultColor;
    Path1.Stroke.Kind := TBrushKind.Solid;
    Path1.Stroke.Color := Path1.Stroke.DefaultColor;

    Memo1.Lines.Clear;
    SL := TStringList.Create;
    try
      SL.LoadFromFile(OpenDialog1.FileName);
      Match := TRegEx.Match(SL.Text, 'd="(.*)"');
      while Match.Success do
      begin
        Path1.Data.Data := Path1.Data.Data + Match.Groups.Item[1].Value;
        Match := Match.NextMatch;
      end;
    finally
      FreeAndNil(SL);
    end;
    Memo1.Lines.Add(Path1.Data.Data);
    Path1.EndUpdate;
  end;
end;

procedure TMainForm.GradientChkBoxChange(Sender: TObject);
// Utilisation ou non d'un gradient
var
  i: Integer;
begin
  if GradientChkBox.IsChecked then
  begin
    GradientEdit1.BeginUpdate;
    for i := 1 to GradientEdit1.Gradient.Points.Count - 2 do
      GradientEdit1.Gradient.Points.Delete(i);
    GradientEdit1.Gradient.Color := ColorBox1.Color;
    GradientEdit1.Gradient.Color1 := TAlphaColors.White;
    GradientEdit1.EndUpdate;
    GradientEdit1Change(Sender);
    if Objet.ItemIndex = 0 then
    begin
      memPG.Fill.Kind := TBrushKind.Gradient;
      memPG.Fill.Gradient := GradientEdit1.Gradient;
    end
    else
    begin
      memPG.Stroke.Kind := TBrushKind.Gradient;
      memPG.Stroke.Gradient := GradientEdit1.Gradient;
    end;
  end
  else
  begin
    if Objet.ItemIndex = 0 then
    begin
      memPG.Fill.Kind := TBrushKind.Solid;
      memPG.Fill.Color := ColorBox1.Color;
    end
    else
    begin
      memPG.Stroke.Kind := TBrushKind.Solid;
      memPG.Stroke.Color := ColorBox1.Color;
    end;
  end;
  RedrawPath;
end;

procedure TMainForm.GradientEdit1Change(Sender: TObject);
begin
  if Objet.ItemIndex = 0 then
  begin
    memPG.Fill.Gradient := GradientEdit1.Gradient;
    CalcPosition(AngleGradient.Value, memPG.Fill.Gradient);
  end
  else
  begin
    memPG.Stroke.Gradient := GradientEdit1.Gradient;
    CalcPosition(AngleGradient.Value, memPG.Stroke.Gradient);
  end;
  RedrawPath;
end;

procedure TMainForm.HauteurChange(Sender: TObject);
begin
  if btnLien.Imageindex = 0 then
  begin
    Largeur.Value := Hauteur.Value;
    PathContainer.Width := Largeur.Value;
  end;
  PathContainer.Height := Hauteur.Value;
  LabelTaille.Text := Format('Largeur %0.0f  Hauteur %0.0f',
    [Largeur.Value, Hauteur.Value]);
end;

procedure TMainForm.LargeurChange(Sender: TObject);
begin
  if btnLien.Imageindex = 0 then
  begin
    Hauteur.Value := Largeur.Value;
    PathContainer.Height := Hauteur.Value;
  end;
  PathContainer.Width := Largeur.Value;
  LabelTaille.Text := Format('Largeur %0.0f  Hauteur %0.0f',
    [Largeur.Value, Hauteur.Value]);
end;

procedure TMainForm.Memo1ChangeTracking(Sender: TObject);
begin
  btnRepaint.Visible := True;
end;

procedure TMainForm.ObjetChange(Sender: TObject);
begin
  Epaisseur.Visible := Objet.ItemIndex = 1;
  GradientEdit1.BeginUpdate;
  if Objet.ItemIndex = 0 then
  begin
    GradientChkBox.IsChecked := memPG.Fill.Kind = TBrushKind.Gradient;
    if GradientChkBox.IsChecked then
    begin
      GradientEdit1.Gradient := memPG.Fill.Gradient;
      ColorPicker1.Color := GradientEdit1.Gradient.Points[0].Color;
    end
    else
    begin
      ColorBox1.Color := memPG.Fill.Color;
      ColorPicker1.Color := ColorBox1.Color;
    end;
    RotationDial.Value := memPG.FillAngle;
  end
  else
  begin
    GradientChkBox.IsChecked := memPG.Stroke.Kind = TBrushKind.Gradient;
    if GradientChkBox.IsChecked then
    begin
      GradientEdit1.Gradient := memPG.Stroke.Gradient;
      ColorPicker1.Color := GradientEdit1.Gradient.Points[0].Color;
    end
    else
    begin
      ColorBox1.Color := memPG.Stroke.Color;
      ColorPicker1.Color := ColorBox1.Color;
    end;
    RotationDial.Value := memPG.StrokeAngle;
  end;
  GradientEdit1.EndUpdate;
end;

procedure TMainForm.PositionRotationChange(Sender: TObject);
// ne fonctionne plus depuis XE8 !
// http://riversoftavg.com/blogs/index.php/2015/04/28/xe8-and-radial-gradients-in-windows-fmx-applications/
begin
  case PositionRotation.ItemIndex of
    0:
      begin // coin haut gauche
        if Objet.ItemIndex = 0 then
        begin
          memPG.Fill.Gradient.RadialTransform.RotationCenter.X := 0;
          memPG.Fill.Gradient.RadialTransform.RotationCenter.Y := 0;
        end
        else
        begin
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.X := 0;
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.Y := 0;
        end;
      end;
    1:
      begin // haut centre
        if Objet.ItemIndex = 0 then
        begin
          memPG.Fill.Gradient.RadialTransform.RotationCenter.X := 0.5;
          memPG.Fill.Gradient.RadialTransform.RotationCenter.Y := 0;
        end
        else
        begin
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.X := 0.5;
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.Y := 0;
        end;
      end;
    2:
      begin // coin haut droit
        if Objet.ItemIndex = 0 then
        begin
          memPG.Fill.Gradient.RadialTransform.RotationCenter.X := 1;
          memPG.Fill.Gradient.RadialTransform.RotationCenter.Y := 0;
        end
        else
        begin
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.X := 1;
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.Y := 0;
        end;
      end;
    3:
      begin // Bord gauche centre
        if Objet.ItemIndex = 0 then
        begin
          memPG.Fill.Gradient.RadialTransform.RotationCenter.X := 0;
          memPG.Fill.Gradient.RadialTransform.RotationCenter.Y := 0.5;
        end
        else
        begin
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.X := 0;
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.Y := 0.5;
        end;
      end;
    4:
      begin // centre
        if Objet.ItemIndex = 0 then
        begin
          memPG.Fill.Gradient.RadialTransform.RotationCenter.X := 0.5;
          memPG.Fill.Gradient.RadialTransform.RotationCenter.Y := 0.5;
        end
        else
        begin
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.X := 0.5;
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.Y := 0.5;
        end;
      end;
    5:
      begin // bord droit centre
        if Objet.ItemIndex = 0 then
        begin
          memPG.Fill.Gradient.RadialTransform.RotationCenter.X := 1;
          memPG.Fill.Gradient.RadialTransform.RotationCenter.Y := 0.5;
        end
        else
        begin
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.X := 1;
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.Y := 0.5;
        end;
      end;
    6:
      begin // coin bas gauche
        if Objet.ItemIndex = 0 then
        begin
          memPG.Fill.Gradient.RadialTransform.RotationCenter.X := 0;
          memPG.Fill.Gradient.RadialTransform.RotationCenter.Y := 1;
        end
        else
        begin
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.X := 0;
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.Y := 1;
        end;
      end;
    7:
      begin // bas centre
        if Objet.ItemIndex = 0 then
        begin
          memPG.Fill.Gradient.RadialTransform.RotationCenter.X := 0.5;
          memPG.Fill.Gradient.RadialTransform.RotationCenter.Y := 1;
        end
        else
        begin
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.X := 0.5;
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.Y := 1;
        end;
      end;
    8:
      begin // coin bas droit
        if Objet.ItemIndex = 0 then
        begin
          memPG.Fill.Gradient.RadialTransform.RotationCenter.X := 1;
          memPG.Fill.Gradient.RadialTransform.RotationCenter.Y := 1;
        end
        else
        begin
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.X := 1;
          memPG.Stroke.Gradient.RadialTransform.RotationCenter.Y := 1;
        end;
      end;
  end;
  RotationDialChange(Sender);
end;

end.
