object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 7
  Margins.Top = 7
  Margins.Right = 7
  Margins.Bottom = 7
  Caption = 'Form1'
  ClientHeight = 992
  ClientWidth = 1404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -27
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 216
  TextHeight = 37
  object Memo1: TMemo
    Left = 18
    Top = 434
    Width = 1368
    Height = 558
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object edtRefreshToken: TLabeledEdit
    Left = 18
    Top = 54
    Width = 272
    Height = 45
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    EditLabel.Width = 162
    EditLabel.Height = 37
    EditLabel.Margins.Left = 7
    EditLabel.Margins.Top = 7
    EditLabel.Margins.Right = 7
    EditLabel.Margins.Bottom = 7
    EditLabel.Caption = 'Refresh Token'
    TabOrder = 1
    Text = ''
  end
  object edtAccessToken: TLabeledEdit
    Left = 18
    Top = 367
    Width = 272
    Height = 45
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    EditLabel.Width = 153
    EditLabel.Height = 37
    EditLabel.Margins.Left = 7
    EditLabel.Margins.Top = 7
    EditLabel.Margins.Right = 7
    EditLabel.Margins.Bottom = 7
    EditLabel.Caption = 'Access Token'
    TabOrder = 2
    Text = ''
  end
  object edtDevToken: TLabeledEdit
    Left = 18
    Top = 162
    Width = 272
    Height = 45
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    EditLabel.Width = 198
    EditLabel.Height = 37
    EditLabel.Margins.Left = 7
    EditLabel.Margins.Top = 7
    EditLabel.Margins.Right = 7
    EditLabel.Margins.Bottom = 7
    EditLabel.Caption = 'Developer Token'
    TabOrder = 3
    Text = ''
  end
  object edtClientID: TLabeledEdit
    Left = 414
    Top = 54
    Width = 272
    Height = 45
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    EditLabel.Width = 102
    EditLabel.Height = 37
    EditLabel.Margins.Left = 7
    EditLabel.Margins.Top = 7
    EditLabel.Margins.Right = 7
    EditLabel.Margins.Bottom = 7
    EditLabel.Caption = 'Client ID'
    TabOrder = 4
    Text = ''
  end
  object edtClientSecret: TLabeledEdit
    Left = 414
    Top = 162
    Width = 272
    Height = 45
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    EditLabel.Width = 148
    EditLabel.Height = 37
    EditLabel.Margins.Left = 7
    EditLabel.Margins.Top = 7
    EditLabel.Margins.Right = 7
    EditLabel.Margins.Bottom = 7
    EditLabel.Caption = 'Client Secret'
    TabOrder = 5
    Text = ''
  end
  object edtCustomerID: TLabeledEdit
    Left = 756
    Top = 162
    Width = 272
    Height = 45
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    EditLabel.Width = 147
    EditLabel.Height = 37
    EditLabel.Margins.Left = 7
    EditLabel.Margins.Top = 7
    EditLabel.Margins.Right = 7
    EditLabel.Margins.Bottom = 7
    EditLabel.Caption = 'Customer ID'
    TabOrder = 6
    Text = ''
  end
  object edtCustomerLoggedIn: TLabeledEdit
    Left = 756
    Top = 54
    Width = 272
    Height = 45
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    EditLabel.Width = 274
    EditLabel.Height = 37
    EditLabel.Margins.Left = 7
    EditLabel.Margins.Top = 7
    EditLabel.Margins.Right = 7
    EditLabel.Margins.Bottom = 7
    EditLabel.Caption = 'Customer Logged In ID'
    TabOrder = 7
    Text = ''
  end
  object btnCreateGAds: TButton
    Left = 1199
    Top = 365
    Width = 169
    Height = 56
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Criar GAds'
    TabOrder = 8
    OnClick = btnCreateGAdsClick
  end
  object btnSerialize: TButton
    Left = 972
    Top = 364
    Width = 169
    Height = 56
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Serialize'
    TabOrder = 9
    OnClick = btnSerializeClick
  end
  object edtQuery: TLabeledEdit
    Left = 972
    Top = 305
    Width = 396
    Height = 45
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    EditLabel.Width = 71
    EditLabel.Height = 37
    EditLabel.Margins.Left = 7
    EditLabel.Margins.Top = 7
    EditLabel.Margins.Right = 7
    EditLabel.Margins.Bottom = 7
    EditLabel.Caption = 'Query'
    TabOrder = 10
    Text = ''
  end
  object Button1: TButton
    Left = 738
    Top = 364
    Width = 169
    Height = 56
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'Run Query'
    TabOrder = 11
    OnClick = Button1Click
  end
end
