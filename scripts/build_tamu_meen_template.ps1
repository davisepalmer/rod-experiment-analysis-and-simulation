param(
  [string]$OutputPath = (Join-Path (Get-Location) "TAMU_MEEN_Research_Template.pptx")
)

$ErrorActionPreference = "Stop"

try {
  Add-Type -AssemblyName System.Drawing
}
catch {
}

$msoTrue = -1
$msoFalse = 0
$ppLayoutBlank = 12
$ppAlignLeft = 1
$ppAlignCenter = 2
$ppAlignRight = 3

$script:Typography = @{
  HeroTitle = 40
  Title = 36
  SectionTitle = 34
  Body = 24
  CardBody = 20
  Subtitle = 18
  Caption = 14
  Small = 16
  Tag = 12
  SlideNumber = 11
}

$script:Layout = @{
  SlideWidth = 960
  SlideHeight = 540
  OuterMargin = 64
  TopBandHeight = 44
  FooterRuleY = 518
  Gutter = 24
  TitleTop = 58
  SubtitleTop = 100
  TitleAccentWidth = 8
  ContentTop = 126
}

$script:Layout.ContentWidth = $script:Layout.SlideWidth - (2 * $script:Layout.OuterMargin)
$script:Layout.TwoUpWidth = [Math]::Round(($script:Layout.ContentWidth - $script:Layout.Gutter) / 2, 2)
$script:Layout.ThirdWidth = [Math]::Round(($script:Layout.ContentWidth - (2 * $script:Layout.Gutter)) / 3, 2)
$script:Layout.TwoThirdWidth = [Math]::Round((2 * $script:Layout.ThirdWidth) + $script:Layout.Gutter, 2)

function Get-ColumnLeft {
  param([int]$ColumnIndex)

  [Math]::Round(
    $script:Layout.OuterMargin + ($ColumnIndex * ($script:Layout.ThirdWidth + $script:Layout.Gutter)),
    2
  )
}

function Get-ColumnWidth {
  param([int]$Span = 1)

  [Math]::Round(
    ($script:Layout.ThirdWidth * $Span) + ($script:Layout.Gutter * ($Span - 1)),
    2
  )
}

function Convert-HexToOleColor {
  param([string]$Hex)

  $clean = $Hex.TrimStart("#")
  $r = [Convert]::ToInt32($clean.Substring(0, 2), 16)
  $g = [Convert]::ToInt32($clean.Substring(2, 2), 16)
  $b = [Convert]::ToInt32($clean.Substring(4, 2), 16)
  $color = [System.Drawing.Color]::FromArgb($r, $g, $b)
  [System.Drawing.ColorTranslator]::ToOle($color)
}

function Add-Rectangle {
  param(
    [object]$SlideOrMaster,
    [double]$Left,
    [double]$Top,
    [double]$Width,
    [double]$Height,
    [int]$FillColor,
    [int]$LineColor,
    [double]$LineWeight = 1,
    [int]$ShapeType = 1
  )

  $shape = $SlideOrMaster.Shapes.AddShape($ShapeType, $Left, $Top, $Width, $Height)
  $shape.Fill.Visible = $msoTrue
  $shape.Fill.Solid()
  $shape.Fill.ForeColor.RGB = $FillColor
  $shape.Line.Visible = $msoTrue
  $shape.Line.ForeColor.RGB = $LineColor
  $shape.Line.Weight = $LineWeight
  $shape
}

function Add-LineShape {
  param(
    [object]$SlideOrMaster,
    [double]$X1,
    [double]$Y1,
    [double]$X2,
    [double]$Y2,
    [int]$Color,
    [double]$Weight = 1.25
  )

  $line = $SlideOrMaster.Shapes.AddLine($X1, $Y1, $X2, $Y2)
  $line.Line.ForeColor.RGB = $Color
  $line.Line.Weight = $Weight
  $line
}

function Set-ShapeText {
  param(
    [object]$Shape,
    [string]$Text,
    [double]$FontSize = 18,
    [int]$FontColor,
    [bool]$Bold = $false,
    [bool]$Italic = $false,
    [string]$FontName = "Times New Roman",
    [int]$Alignment = 1,
    [double]$MarginLeft = 10,
    [double]$MarginRight = 10,
    [double]$MarginTop = 6,
    [double]$MarginBottom = 6
  )

  $Shape.TextFrame.TextRange.Text = $Text
  $Shape.TextFrame.MarginLeft = $MarginLeft
  $Shape.TextFrame.MarginRight = $MarginRight
  $Shape.TextFrame.MarginTop = $MarginTop
  $Shape.TextFrame.MarginBottom = $MarginBottom
  $Shape.TextFrame.WordWrap = $msoTrue
  $Shape.TextFrame.TextRange.Font.Name = $FontName
  $Shape.TextFrame.TextRange.Font.Size = $FontSize
  $Shape.TextFrame.TextRange.Font.Bold = $(if ($Bold) { $msoTrue } else { $msoFalse })
  $Shape.TextFrame.TextRange.Font.Italic = $(if ($Italic) { $msoTrue } else { $msoFalse })
  $Shape.TextFrame.TextRange.Font.Color.RGB = $FontColor
  $Shape.TextFrame.TextRange.ParagraphFormat.Alignment = $Alignment
}

function Add-TextBox {
  param(
    [object]$SlideOrMaster,
    [double]$Left,
    [double]$Top,
    [double]$Width,
    [double]$Height,
    [string]$Text,
    [double]$FontSize,
    [int]$FontColor,
    [bool]$Bold = $false,
    [bool]$Italic = $false,
    [string]$FontName = "Times New Roman",
    [int]$Alignment = 1
  )

  $shape = $SlideOrMaster.Shapes.AddTextbox(1, $Left, $Top, $Width, $Height)
  $shape.Fill.Visible = $msoFalse
  $shape.Line.Visible = $msoFalse
  Set-ShapeText -Shape $shape -Text $Text -FontSize $FontSize -FontColor $FontColor -Bold $Bold -Italic $Italic -FontName $FontName -Alignment $Alignment
  $shape
}

function Add-PictureContain {
  param(
    [object]$SlideOrMaster,
    [string]$ImagePath,
    [double]$Left,
    [double]$Top,
    [double]$MaxWidth,
    [double]$MaxHeight
  )

  if (-not (Test-Path $ImagePath)) {
    return $null
  }

  $img = [System.Drawing.Image]::FromFile($ImagePath)
  try {
    $ratio = [Math]::Min($MaxWidth / $img.Width, $MaxHeight / $img.Height)
    $width = [Math]::Round($img.Width * $ratio, 2)
    $height = [Math]::Round($img.Height * $ratio, 2)
  }
  finally {
    $img.Dispose()
  }

  $actualLeft = $Left + (($MaxWidth - $width) / 2)
  $actualTop = $Top + (($MaxHeight - $height) / 2)
  $SlideOrMaster.Shapes.AddPicture($ImagePath, $msoFalse, $msoTrue, $actualLeft, $actualTop, $width, $height)
}

function Add-PlotPlaceholder {
  param(
    [object]$Slide,
    [double]$Left,
    [double]$Top,
    [double]$Width,
    [double]$Height,
    [string]$Caption
  )

  $card = Add-Rectangle -SlideOrMaster $Slide -Left $Left -Top $Top -Width $Width -Height $Height -FillColor $script:Colors.White -LineColor $script:Colors.Gray300 -LineWeight 1.2 -ShapeType 5
  $captionBar = Add-Rectangle -SlideOrMaster $Slide -Left ($Left + 1) -Top ($Top + 1) -Width ($Width - 2) -Height 28 -FillColor $script:Colors.Sand -LineColor $script:Colors.Sand -LineWeight 0
  Set-ShapeText -Shape $captionBar -Text $Caption -FontSize $script:Typography.Caption -FontColor $script:Colors.Gray900 -Bold $true -MarginLeft 12 -MarginTop 5 -MarginBottom 3

  $axisLeft = $Left + 42
  $axisBottom = $Top + $Height - 44
  Add-LineShape -SlideOrMaster $Slide -X1 $axisLeft -Y1 ($Top + 46) -X2 $axisLeft -Y2 $axisBottom -Color $script:Colors.Gray500 -Weight 1.2 | Out-Null
  Add-LineShape -SlideOrMaster $Slide -X1 $axisLeft -Y1 $axisBottom -X2 ($Left + $Width - 28) -Y2 $axisBottom -Color $script:Colors.Gray500 -Weight 1.2 | Out-Null

  foreach ($offset in @(0.2, 0.4, 0.6, 0.8)) {
    $y = $Top + 46 + (($Height - 90) * $offset)
    Add-LineShape -SlideOrMaster $Slide -X1 ($axisLeft + 1) -Y1 $y -X2 ($Left + $Width - 28) -Y2 $y -Color $script:Colors.Gray100 -Weight 0.8 | Out-Null
  }

  $labelFontSize = if ($Height -lt 165 -or $Width -lt 320) { 17 } else { 20 }
  $label = Add-TextBox -SlideOrMaster $Slide -Left ($Left + 64) -Top ($Top + ($Height / 2) - 22) -Width ($Width - 128) -Height 44 -Text "[Insert plot, figure or image]" -FontSize $labelFontSize -FontColor $script:Colors.Gray500 -Italic $true -Alignment $ppAlignCenter
  $label.TextFrame.MarginTop = 0
  $label.TextFrame.MarginBottom = 0
  $null
}

function Add-InsightCard {
  param(
    [object]$Slide,
    [double]$Left,
    [double]$Top,
    [double]$Width,
    [double]$Height,
    [string]$Title,
    [string]$Body
  )

  $card = Add-Rectangle -SlideOrMaster $Slide -Left $Left -Top $Top -Width $Width -Height $Height -FillColor $script:Colors.Gray100 -LineColor $script:Colors.Gray300 -LineWeight 1 -ShapeType 5
  $tagWidth = [Math]::Min(($Width - 28), [Math]::Max(80, (($Title.Length * 10) + 16)))
  $tag = Add-Rectangle -SlideOrMaster $Slide -Left ($Left + 16) -Top ($Top + 14) -Width $tagWidth -Height 24 -FillColor $script:Colors.Maroon -LineColor $script:Colors.Maroon -LineWeight 0
  Set-ShapeText -Shape $tag -Text $Title -FontSize $script:Typography.Tag -FontColor $script:Colors.White -Bold $true -Alignment $ppAlignCenter -MarginLeft 4 -MarginRight 4 -MarginTop 5 -MarginBottom 2
  $bodyFontSize = if ($Height -le 96) { 17 } elseif ($Height -le 110) { 18 } else { $script:Typography.CardBody }
  Add-TextBox -SlideOrMaster $Slide -Left ($Left + 18) -Top ($Top + 52) -Width ($Width - 36) -Height ($Height - 66) -Text $Body -FontSize $bodyFontSize -FontColor $script:Colors.Gray900 | Out-Null
  $null
}

function Add-SlideTitle {
  param(
    [object]$Slide,
    [string]$Title,
    [string]$Subtitle = ""
  )

  Add-Rectangle -SlideOrMaster $Slide -Left $script:Layout.OuterMargin -Top ($script:Layout.TitleTop + 4) -Width $script:Layout.TitleAccentWidth -Height 36 -FillColor $script:Colors.Maroon -LineColor $script:Colors.Maroon -LineWeight 0 | Out-Null
  Add-TextBox -SlideOrMaster $Slide -Left ($script:Layout.OuterMargin + 18) -Top $script:Layout.TitleTop -Width 720 -Height 42 -Text $Title -FontSize $script:Typography.Title -FontColor $script:Colors.Maroon -Bold $true | Out-Null
  if ($Subtitle) {
    Add-TextBox -SlideOrMaster $Slide -Left ($script:Layout.OuterMargin + 18) -Top $script:Layout.SubtitleTop -Width 760 -Height 24 -Text $Subtitle -FontSize $script:Typography.Subtitle -FontColor $script:Colors.Gray500 | Out-Null
  }
}

function Configure-SlideMaster {
  param(
    [object]$Presentation,
    [string]$WhiteLogoPath
  )

  $master = $Presentation.SlideMaster

  $background = Add-Rectangle -SlideOrMaster $master -Left 0 -Top 0 -Width $script:Layout.SlideWidth -Height $script:Layout.SlideHeight -FillColor $script:Colors.White -LineColor $script:Colors.White -LineWeight 0
  $background.ZOrder(1) | Out-Null

  Add-Rectangle -SlideOrMaster $master -Left 0 -Top 0 -Width $script:Layout.SlideWidth -Height $script:Layout.TopBandHeight -FillColor $script:Colors.Maroon -LineColor $script:Colors.Maroon -LineWeight 0 | Out-Null
  Add-Rectangle -SlideOrMaster $master -Left 0 -Top $script:Layout.TopBandHeight -Width $script:Layout.SlideWidth -Height 4 -FillColor $script:Colors.Sand -LineColor $script:Colors.Sand -LineWeight 0 | Out-Null
  Add-LineShape -SlideOrMaster $master -X1 $script:Layout.OuterMargin -Y1 $script:Layout.FooterRuleY -X2 ($script:Layout.SlideWidth - $script:Layout.OuterMargin) -Y2 $script:Layout.FooterRuleY -Color $script:Colors.Gray300 -Weight 1 | Out-Null

  if (Test-Path $WhiteLogoPath) {
    Add-PictureContain -SlideOrMaster $master -ImagePath $WhiteLogoPath -Left 674 -Top 4 -MaxWidth 250 -MaxHeight 30 | Out-Null
  }
  else {
    Add-TextBox -SlideOrMaster $master -Left 676 -Top 5 -Width 250 -Height 28 -Text "Mechanical Engineering" -FontSize 18 -FontColor $script:Colors.White -Bold $true -Alignment $ppAlignRight | Out-Null
  }

  $Presentation.SlideMaster.HeadersFooters.DateAndTime.Visible = $msoFalse
  $Presentation.SlideMaster.HeadersFooters.Footer.Visible = $msoFalse
  $Presentation.SlideMaster.HeadersFooters.SlideNumber.Visible = $msoTrue

  for ($i = 1; $i -le $master.Shapes.Count; $i++) {
    $shape = $master.Shapes.Item($i)
    if ($shape.Type -ne 14) {
      continue
    }

    $shape.TextFrame.TextRange.Font.Name = "Times New Roman"

    switch ($shape.PlaceholderFormat.Type) {
      1 {
        $shape.TextFrame.TextRange.Font.Size = $script:Typography.Title
        $shape.TextFrame.TextRange.Font.Bold = $msoTrue
        $shape.TextFrame.TextRange.Font.Color.RGB = $script:Colors.Maroon
      }
      2 {
        $shape.TextFrame.TextRange.Font.Size = $script:Typography.Body
        $shape.TextFrame.TextRange.Font.Bold = $msoFalse
        $shape.TextFrame.TextRange.Font.Color.RGB = $script:Colors.Gray900
      }
      3 {
        $shape.TextFrame.TextRange.Font.Size = $script:Typography.Title
        $shape.TextFrame.TextRange.Font.Bold = $msoTrue
        $shape.TextFrame.TextRange.Font.Color.RGB = $script:Colors.Maroon
      }
      4 {
        $shape.TextFrame.TextRange.Font.Size = $script:Typography.Subtitle
        $shape.TextFrame.TextRange.Font.Bold = $msoFalse
        $shape.TextFrame.TextRange.Font.Color.RGB = $script:Colors.Gray500
      }
      13 {
        $shape.Left = ($script:Layout.SlideWidth - $script:Layout.OuterMargin - 4)
        $shape.Top = ($script:Layout.FooterRuleY + 1)
        $shape.Width = 28
        $shape.Height = 16
        $shape.TextFrame.MarginLeft = 0
        $shape.TextFrame.MarginRight = 0
        $shape.TextFrame.MarginTop = 0
        $shape.TextFrame.MarginBottom = 0
        $shape.TextFrame.TextRange.Font.Size = $script:Typography.SlideNumber
        $shape.TextFrame.TextRange.Font.Color.RGB = $script:Colors.Maroon
        $shape.TextFrame.TextRange.Font.Bold = $msoTrue
        $shape.TextFrame.TextRange.ParagraphFormat.Alignment = $ppAlignRight
      }
    }
  }
}

function Add-TitleSlide {
  param(
    [object]$Presentation,
    [string]$MaroonLogoPath,
    [string]$WhiteLogoPath
  )

  $slide = $Presentation.Slides.Add(1, $ppLayoutBlank)
  Add-Rectangle -SlideOrMaster $slide -Left 0 -Top 44 -Width 292 -Height 496 -FillColor $script:Colors.Maroon -LineColor $script:Colors.Maroon -LineWeight 0 | Out-Null
  Add-Rectangle -SlideOrMaster $slide -Left 292 -Top 44 -Width 8 -Height 496 -FillColor $script:Colors.Sand -LineColor $script:Colors.Sand -LineWeight 0 | Out-Null

  if (Test-Path $WhiteLogoPath) {
    Add-PictureContain -SlideOrMaster $slide -ImagePath $WhiteLogoPath -Left 34 -Top 86 -MaxWidth 226 -MaxHeight 72 | Out-Null
  }

  Add-TextBox -SlideOrMaster $slide -Left 42 -Top 188 -Width 220 -Height 126 -Text "Mechanical`nEngineering`nResearch Group" -FontSize $script:Typography.SectionTitle -FontColor $script:Colors.White -Bold $true | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left 42 -Top 432 -Width 220 -Height 74 -Text "Texas A&M University`nResearch presentation template" -FontSize $script:Typography.Subtitle -FontColor $script:Colors.White | Out-Null

  Add-TextBox -SlideOrMaster $slide -Left 338 -Top 110 -Width 532 -Height 88 -Text "[Presentation Title]" -FontSize $script:Typography.HeroTitle -FontColor $script:Colors.Maroon -Bold $true | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left 340 -Top 208 -Width 500 -Height 34 -Text "[Presenter Name(s)]" -FontSize $script:Typography.Body -FontColor $script:Colors.Gray900 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left 340 -Top 246 -Width 500 -Height 30 -Text "[Research Group / Lab Name]" -FontSize $script:Typography.Body -FontColor $script:Colors.Gray800 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left 340 -Top 286 -Width 520 -Height 24 -Text "J. Mike Walker '66 Department of Mechanical Engineering" -FontSize $script:Typography.Subtitle -FontColor $script:Colors.Gray500 | Out-Null

  $hero = Add-Rectangle -SlideOrMaster $slide -Left 328 -Top 334 -Width 548 -Height 118 -FillColor $script:Colors.Gray100 -LineColor $script:Colors.Gray300 -LineWeight 1 -ShapeType 5
  Set-ShapeText -Shape $hero -Text "Use this area for an experimental photo, apparatus render, top-line figure, or one-sentence research claim." -FontSize 22 -FontColor $script:Colors.Gray500 -Italic $true -Alignment $ppAlignCenter -MarginLeft 32 -MarginRight 32 -MarginTop 26 -MarginBottom 16

  if (Test-Path $MaroonLogoPath) {
    Add-PictureContain -SlideOrMaster $slide -ImagePath $MaroonLogoPath -Left 600 -Top 460 -MaxWidth 276 -MaxHeight 36 | Out-Null
  }
}

function Add-SectionSlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(2, $ppLayoutBlank)
  $left = Get-ColumnLeft 0
  $right = Get-ColumnLeft 1
  Add-Rectangle -SlideOrMaster $slide -Left $left -Top 132 -Width (Get-ColumnWidth 1) -Height 228 -FillColor $script:Colors.Maroon -LineColor $script:Colors.Maroon -LineWeight 0 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left ($left + 22) -Top 164 -Width 170 -Height 28 -Text "SECTION" -FontSize $script:Typography.Subtitle -FontColor $script:Colors.Sand -Bold $true | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left ($left + 22) -Top 206 -Width 214 -Height 112 -Text "[Section Title]" -FontSize $script:Typography.SectionTitle -FontColor $script:Colors.White -Bold $true | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left $right -Top 170 -Width (Get-ColumnWidth 2) -Height 94 -Text "Use divider slides to reset the room between methods, results, validation, and appendix material." -FontSize $script:Typography.Body -FontColor $script:Colors.Gray900 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left $right -Top 286 -Width (Get-ColumnWidth 2) -Height 54 -Text "Keep one message here. The point is pacing, not explanation." -FontSize $script:Typography.Subtitle -FontColor $script:Colors.Gray500 | Out-Null
}

function Add-AgendaSlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(3, $ppLayoutBlank)
  Add-SlideTitle -Slide $slide -Title "Research Overview / Agenda" -Subtitle "A clean opener for study design, motivation, or the talk roadmap."
  $left = Get-ColumnLeft 0
  $right = Get-ColumnLeft 1
  Add-Rectangle -SlideOrMaster $slide -Left $left -Top 132 -Width 8 -Height 252 -FillColor $script:Colors.Maroon -LineColor $script:Colors.Maroon -LineWeight 0 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left ($left + 22) -Top 130 -Width 290 -Height 256 -Text "1. Motivation and research question`n2. Experimental or numerical approach`n3. Core result set`n4. Interpretation and limits`n5. Next steps" -FontSize $script:Typography.Body -FontColor $script:Colors.Gray900 | Out-Null
  Add-InsightCard -Slide $slide -Left $right -Top 132 -Width (Get-ColumnWidth 2) -Height 214 -Title "USE THIS" -Body "Frame the problem, test matrix, assumptions, or one anchor visual for the audience."
  Add-Rectangle -SlideOrMaster $slide -Left $right -Top 370 -Width (Get-ColumnWidth 2) -Height 76 -FillColor $script:Colors.White -LineColor $script:Colors.Gray300 -LineWeight 1 -ShapeType 5 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left ($right + 18) -Top 390 -Width ((Get-ColumnWidth 2) - 36) -Height 30 -Text "Reserve this strip for the one takeaway you want remembered from the opening section." -FontSize $script:Typography.Subtitle -FontColor $script:Colors.Gray800 | Out-Null
}

function Add-FullPlotSlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(4, $ppLayoutBlank)
  Add-SlideTitle -Slide $slide -Title "Primary Result" -Subtitle "Designed for one dominant figure with enough breathing room for axes, legends and captions."
  Add-PlotPlaceholder -Slide $slide -Left $script:Layout.OuterMargin -Top 132 -Width $script:Layout.ContentWidth -Height 310 -Caption "Main Result / Figure 1"
  Add-Rectangle -SlideOrMaster $slide -Left $script:Layout.OuterMargin -Top 454 -Width $script:Layout.ContentWidth -Height 42 -FillColor $script:Colors.Gray100 -LineColor $script:Colors.Gray300 -LineWeight 1 -ShapeType 5 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left ($script:Layout.OuterMargin + 18) -Top 464 -Width ($script:Layout.ContentWidth - 36) -Height 20 -Text "Figure note: state what changed, by how much, and why the audience should care." -FontSize $script:Typography.Small -FontColor $script:Colors.Gray800 | Out-Null
}

function Add-PlotBulletsSlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(5, $ppLayoutBlank)
  Add-SlideTitle -Slide $slide -Title "Result With Interpretation" -Subtitle "For one plot plus the three or four statements you need while presenting it."
  $plotLeft = Get-ColumnLeft 0
  $cardsLeft = Get-ColumnLeft 2
  Add-PlotPlaceholder -Slide $slide -Left $plotLeft -Top 132 -Width (Get-ColumnWidth 2) -Height 320 -Caption "Figure / Experimental Curve / CFD Field"
  Add-InsightCard -Slide $slide -Left $cardsLeft -Top 132 -Width (Get-ColumnWidth 1) -Height 96 -Title "KEY RESULT" -Body "State the main numerical change."
  Add-InsightCard -Slide $slide -Left $cardsLeft -Top 244 -Width (Get-ColumnWidth 1) -Height 96 -Title "INTERPRETATION" -Body "Link the result to the mechanism."
  Add-InsightCard -Slide $slide -Left $cardsLeft -Top 356 -Width (Get-ColumnWidth 1) -Height 96 -Title "IMPACT" -Body "Explain why the result matters."
}

function Add-ComparisonSlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(6, $ppLayoutBlank)
  Add-SlideTitle -Slide $slide -Title "Comparison Layout" -Subtitle "Built for A/B cases, before/after states, baseline vs. model, or experiment vs. simulation."
  $left = Get-ColumnLeft 0
  $middle = Get-ColumnLeft 1
  $right = Get-ColumnLeft 2
  Add-PlotPlaceholder -Slide $slide -Left $left -Top 132 -Width (Get-ColumnWidth 1) -Height 150 -Caption "Case A"
  Add-PlotPlaceholder -Slide $slide -Left $middle -Top 132 -Width (Get-ColumnWidth 1) -Height 150 -Caption "Case B"
  Add-InsightCard -Slide $slide -Left $right -Top 132 -Width (Get-ColumnWidth 1) -Height 314 -Title "SUMMARY" -Body "Report the delta, uncertainty band, or validation error here."
  Add-PlotPlaceholder -Slide $slide -Left $left -Top 300 -Width (Get-ColumnWidth 2) -Height 146 -Caption "Difference / Error / Zoomed View"
}

function Add-EquationSlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(7, $ppLayoutBlank)
  Add-SlideTitle -Slide $slide -Title "Equation / Model Layout" -Subtitle "Use for governing equations, constitutive models, estimators or reduced-order fits."
  $main = Get-ColumnLeft 0
  $side = Get-ColumnLeft 2
  $eqCard = Add-Rectangle -SlideOrMaster $slide -Left $main -Top 132 -Width (Get-ColumnWidth 2) -Height 286 -FillColor $script:Colors.White -LineColor $script:Colors.Maroon -LineWeight 1.5 -ShapeType 5
  $eqTag = Add-Rectangle -SlideOrMaster $slide -Left $main -Top 132 -Width (Get-ColumnWidth 2) -Height 28 -FillColor $script:Colors.Maroon -LineColor $script:Colors.Maroon -LineWeight 0
  Set-ShapeText -Shape $eqTag -Text "Model / Governing Equation" -FontSize $script:Typography.Caption -FontColor $script:Colors.White -Bold $true -MarginLeft 12 -MarginTop 6 -MarginBottom 2
  Add-TextBox -SlideOrMaster $slide -Left ($main + 34) -Top 198 -Width ((Get-ColumnWidth 2) - 68) -Height 82 -Text "[Insert equation here]" -FontSize 30 -FontColor $script:Colors.Gray500 -Italic $true -Alignment $ppAlignCenter | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left ($main + 34) -Top 298 -Width ((Get-ColumnWidth 2) - 68) -Height 58 -Text "Keep symbols defined nearby. Reveal derivation detail in stages if the audience needs it." -FontSize $script:Typography.Subtitle -FontColor $script:Colors.Gray800 -Alignment $ppAlignCenter | Out-Null
  Add-InsightCard -Slide $slide -Left $side -Top 132 -Width (Get-ColumnWidth 1) -Height 144 -Title "INPUTS" -Body "List variables, parameters, units, and valid ranges."
  Add-InsightCard -Slide $slide -Left $side -Top 290 -Width (Get-ColumnWidth 1) -Height 144 -Title "ASSUMPTIONS" -Body "State the closure, boundary conditions, and limits."
  Add-Rectangle -SlideOrMaster $slide -Left $script:Layout.OuterMargin -Top 446 -Width $script:Layout.ContentWidth -Height 42 -FillColor $script:Colors.Gray100 -LineColor $script:Colors.Gray300 -LineWeight 1 -ShapeType 5 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left ($script:Layout.OuterMargin + 18) -Top 456 -Width ($script:Layout.ContentWidth - 36) -Height 18 -Text "Tip: equations land better when the slide answers one question: prediction, identification, control, or scaling." -FontSize $script:Typography.Small -FontColor $script:Colors.Gray800 | Out-Null
}

function Add-TableChartSlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(8, $ppLayoutBlank)
  Add-SlideTitle -Slide $slide -Title "Table + Chart Layout" -Subtitle "Use when the audience needs both exact values and a fast visual read."

  $tableLeft = $script:Layout.OuterMargin
  $chartLeft = $script:Layout.OuterMargin + $script:Layout.TwoUpWidth + $script:Layout.Gutter
  $tableShape = $slide.Shapes.AddTable(5, 4, $tableLeft, 132, $script:Layout.TwoUpWidth, 248)
  $table = $tableShape.Table
  $headers = @("Metric", "Case A", "Case B", "Case C")
  $rows = @(
    @("Peak response", "0.00", "0.00", "0.00"),
    @("Mean error", "0.00", "0.00", "0.00"),
    @("Std. dev.", "0.00", "0.00", "0.00"),
    @("Sample size", "n", "n", "n")
  )

  for ($c = 1; $c -le 4; $c++) {
    $cell = $table.Cell(1, $c).Shape
    $cell.Fill.ForeColor.RGB = $script:Colors.Maroon
    $cell.TextFrame.TextRange.Text = $headers[$c - 1]
    $cell.TextFrame.TextRange.Font.Name = "Times New Roman"
    $cell.TextFrame.TextRange.Font.Size = 18
    $cell.TextFrame.TextRange.Font.Bold = $msoTrue
    $cell.TextFrame.TextRange.Font.Color.RGB = $script:Colors.White
    $cell.TextFrame.TextRange.ParagraphFormat.Alignment = $ppAlignCenter
  }

  for ($r = 2; $r -le 5; $r++) {
    for ($c = 1; $c -le 4; $c++) {
      $cell = $table.Cell($r, $c).Shape
      $cell.Fill.ForeColor.RGB = $(if ($r % 2 -eq 0) { $script:Colors.White } else { $script:Colors.Gray100 })
      $cell.TextFrame.TextRange.Text = $rows[$r - 2][$c - 1]
      $cell.TextFrame.TextRange.Font.Name = "Times New Roman"
      $cell.TextFrame.TextRange.Font.Size = 18
      $cell.TextFrame.TextRange.Font.Color.RGB = $script:Colors.Gray900
      $cell.TextFrame.TextRange.ParagraphFormat.Alignment = $(if ($c -eq 1) { $ppAlignLeft } else { $ppAlignCenter })
    }
  }

  Add-PlotPlaceholder -Slide $slide -Left $chartLeft -Top 132 -Width $script:Layout.TwoUpWidth -Height 248 -Caption "Supporting Chart / Ranking / Error Bars"
  Add-Rectangle -SlideOrMaster $slide -Left $script:Layout.OuterMargin -Top 404 -Width $script:Layout.ContentWidth -Height 84 -FillColor $script:Colors.White -LineColor $script:Colors.Gray300 -LineWeight 1 -ShapeType 5 | Out-Null
  $readoutTag = Add-Rectangle -SlideOrMaster $slide -Left ($script:Layout.OuterMargin + 18) -Top 420 -Width 124 -Height 24 -FillColor $script:Colors.Maroon -LineColor $script:Colors.Maroon -LineWeight 0
  Set-ShapeText -Shape $readoutTag -Text "READOUT" -FontSize $script:Typography.Tag -FontColor $script:Colors.White -Bold $true -Alignment $ppAlignCenter -MarginLeft 4 -MarginRight 4 -MarginTop 5 -MarginBottom 2
  Add-TextBox -SlideOrMaster $slide -Left ($script:Layout.OuterMargin + 18) -Top 454 -Width ($script:Layout.ContentWidth - 36) -Height 24 -Text "Use this strip to state what the table confirms that the figure only suggests." -FontSize $script:Typography.Subtitle -FontColor $script:Colors.Gray800 | Out-Null
}

function Add-DashboardSlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(9, $ppLayoutBlank)
  Add-SlideTitle -Slide $slide -Title "Results Dashboard" -Subtitle "A dense but readable option for parameter sweeps, sensitivity studies, ablations or four-condition comparisons."
  $left = $script:Layout.OuterMargin
  $right = $script:Layout.OuterMargin + $script:Layout.TwoUpWidth + $script:Layout.Gutter
  Add-PlotPlaceholder -Slide $slide -Left $left -Top 132 -Width $script:Layout.TwoUpWidth -Height 150 -Caption "Panel A"
  Add-PlotPlaceholder -Slide $slide -Left $right -Top 132 -Width $script:Layout.TwoUpWidth -Height 150 -Caption "Panel B"
  Add-PlotPlaceholder -Slide $slide -Left $left -Top 304 -Width $script:Layout.TwoUpWidth -Height 150 -Caption "Panel C"
  Add-PlotPlaceholder -Slide $slide -Left $right -Top 304 -Width $script:Layout.TwoUpWidth -Height 150 -Caption "Panel D"
}

function Add-SummarySlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(10, $ppLayoutBlank)
  Add-SlideTitle -Slide $slide -Title "Summary / Takeaways" -Subtitle "End with conclusions that are quantitative, not generic."
  $main = Get-ColumnLeft 0
  $side = Get-ColumnLeft 2
  Add-Rectangle -SlideOrMaster $slide -Left $main -Top 132 -Width (Get-ColumnWidth 2) -Height 306 -FillColor $script:Colors.White -LineColor $script:Colors.Gray300 -LineWeight 1.2 -ShapeType 5 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left ($main + 20) -Top 154 -Width ((Get-ColumnWidth 2) - 40) -Height 252 -Text "1. State the strongest result first.`n`n2. Tie it to the physical or modeling interpretation.`n`n3. Call out the design implication, limit, or uncertainty.`n`n4. End on the next experiment or model revision." -FontSize $script:Typography.Body -FontColor $script:Colors.Gray900 | Out-Null
  Add-InsightCard -Slide $slide -Left $side -Top 132 -Width (Get-ColumnWidth 1) -Height 90 -Title "BEST NUMBER" -Body "Peak gain, error, or confidence bound."
  Add-InsightCard -Slide $slide -Left $side -Top 240 -Width (Get-ColumnWidth 1) -Height 90 -Title "LIMIT" -Body "State where the claim stops being reliable."
  Add-InsightCard -Slide $slide -Left $side -Top 348 -Width (Get-ColumnWidth 1) -Height 90 -Title "NEXT" -Body "Define the next action for the group."
}

function Add-AppendixSlide {
  param([object]$Presentation)

  $slide = $Presentation.Slides.Add(11, $ppLayoutBlank)
  Add-SlideTitle -Slide $slide -Title "Appendix / Backup" -Subtitle "Use for extra plots, parameter tables, derivations, instrumentation details or reviewer questions."
  Add-PlotPlaceholder -Slide $slide -Left (Get-ColumnLeft 0) -Top 132 -Width (Get-ColumnWidth 1) -Height 146 -Caption "Backup Figure A"
  Add-PlotPlaceholder -Slide $slide -Left (Get-ColumnLeft 1) -Top 132 -Width (Get-ColumnWidth 1) -Height 146 -Caption "Backup Figure B"
  Add-PlotPlaceholder -Slide $slide -Left (Get-ColumnLeft 2) -Top 132 -Width (Get-ColumnWidth 1) -Height 146 -Caption "Backup Figure C"
  Add-Rectangle -SlideOrMaster $slide -Left $script:Layout.OuterMargin -Top 304 -Width $script:Layout.ContentWidth -Height 152 -FillColor $script:Colors.Gray100 -LineColor $script:Colors.Gray300 -LineWeight 1 -ShapeType 5 | Out-Null
  Add-TextBox -SlideOrMaster $slide -Left ($script:Layout.OuterMargin + 22) -Top 328 -Width ($script:Layout.ContentWidth - 44) -Height 100 -Text "Reserve this area for dense support material. It should answer obvious follow-up questions without feeling like a second presentation." -FontSize $script:Typography.Body -FontColor $script:Colors.Gray800 | Out-Null
}

$script:Colors = @{
  Maroon = Convert-HexToOleColor "#500000"
  MaroonDark = Convert-HexToOleColor "#3C0000"
  MaroonLight = Convert-HexToOleColor "#732F2F"
  White = Convert-HexToOleColor "#FFFFFF"
  Gray900 = Convert-HexToOleColor "#1F1B19"
  Gray800 = Convert-HexToOleColor "#413A36"
  Gray500 = Convert-HexToOleColor "#6B6662"
  Gray300 = Convert-HexToOleColor "#D8D2C8"
  Gray100 = Convert-HexToOleColor "#FAF8F5"
  Sand = Convert-HexToOleColor "#DDD6C6"
}

$assetDir = Join-Path (Get-Location) "ppt_assets"
$whiteLogoPath = Join-Path $assetDir "MEEN_horiz_white.png"
$maroonLogoPath = Join-Path $assetDir "MEEN_horiz_maroon.png"

$ppt = $null
$presentation = $null

try {
  $resolvedOutputPath = [System.IO.Path]::GetFullPath($OutputPath)
  $outputDir = Split-Path -Parent $resolvedOutputPath

  if (-not (Test-Path $outputDir)) {
    New-Item -ItemType Directory -Force -Path $outputDir | Out-Null
  }

  if (Test-Path $resolvedOutputPath) {
    Remove-Item $resolvedOutputPath -Force
  }

  $ppt = New-Object -ComObject PowerPoint.Application
  $presentation = $ppt.Presentations.Add()
  $presentation.PageSetup.SlideWidth = 960
  $presentation.PageSetup.SlideHeight = 540

  Configure-SlideMaster -Presentation $presentation -WhiteLogoPath $whiteLogoPath
  Add-TitleSlide -Presentation $presentation -MaroonLogoPath $maroonLogoPath -WhiteLogoPath $whiteLogoPath
  Add-SectionSlide -Presentation $presentation
  Add-AgendaSlide -Presentation $presentation
  Add-FullPlotSlide -Presentation $presentation
  Add-PlotBulletsSlide -Presentation $presentation
  Add-ComparisonSlide -Presentation $presentation
  Add-EquationSlide -Presentation $presentation
  Add-TableChartSlide -Presentation $presentation
  Add-DashboardSlide -Presentation $presentation
  Add-SummarySlide -Presentation $presentation
  Add-AppendixSlide -Presentation $presentation

  $presentation.SaveAs($resolvedOutputPath, 24)
  Start-Sleep -Milliseconds 1200

  if (-not (Test-Path $resolvedOutputPath)) {
    $presentation.SaveCopyAs($resolvedOutputPath)
    Start-Sleep -Milliseconds 1200
  }

  if (-not (Test-Path $resolvedOutputPath)) {
    throw "PowerPoint did not write the output file: $resolvedOutputPath"
  }
}
finally {
  if ($presentation) {
    try {
      $presentation.Close()
    }
    catch {
    }
    try {
      [void][System.Runtime.InteropServices.Marshal]::ReleaseComObject($presentation)
    }
    catch {
    }
  }
  if ($ppt) {
    try {
      $ppt.Quit()
    }
    catch {
    }
    try {
      [void][System.Runtime.InteropServices.Marshal]::ReleaseComObject($ppt)
    }
    catch {
    }
  }
  [GC]::Collect()
  [GC]::WaitForPendingFinalizers()
}

Write-Output ("Created: " + $resolvedOutputPath)
