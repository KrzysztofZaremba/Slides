"""
Generate a visual cheat sheet PDF for lm() and summary() in R.
Business example: predicting monthly sales from advertising spend.
"""

from reportlab.lib.pagesizes import letter
from reportlab.lib.units import inch, cm
from reportlab.lib.colors import (
    HexColor, white, black
)
from reportlab.pdfgen import canvas
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.platypus import Paragraph
from reportlab.lib.enums import TA_LEFT, TA_CENTER

# ── Colors ──────────────────────────────────────────────────────────
DARK_BLUE   = HexColor("#1B2A4A")
MED_BLUE    = HexColor("#2C5F8A")
LIGHT_BLUE  = HexColor("#D6E8F7")
ACCENT_BLUE = HexColor("#3A7FBF")
GREEN       = HexColor("#2E7D32")
LIGHT_GREEN = HexColor("#E8F5E9")
ORANGE      = HexColor("#E65100")
LIGHT_ORANGE= HexColor("#FFF3E0")
RED         = HexColor("#C62828")
LIGHT_RED   = HexColor("#FFEBEE")
PURPLE      = HexColor("#6A1B9A")
LIGHT_PURPLE= HexColor("#F3E5F5")
GRAY        = HexColor("#F5F5F5")
DARK_GRAY   = HexColor("#616161")
CODE_BG     = HexColor("#F8F9FA")
BORDER_GRAY = HexColor("#E0E0E0")

W, H = letter  # 612 x 792

def draw_rounded_rect(c, x, y, w, h, r=8, fill=None, stroke=None, stroke_width=1):
    """Draw a rounded rectangle."""
    c.saveState()
    if fill:
        c.setFillColor(fill)
    if stroke:
        c.setStrokeColor(stroke)
        c.setLineWidth(stroke_width)
    p = c.beginPath()
    p.roundRect(x, y, w, h, r)
    if fill and stroke:
        c.drawPath(p, fill=1, stroke=1)
    elif fill:
        c.drawPath(p, fill=1, stroke=0)
    else:
        c.drawPath(p, fill=0, stroke=1)
    c.restoreState()

def draw_code_block(c, x, y, w, lines, font_size=9):
    """Draw a code block with background. Returns the bottom y."""
    line_h = font_size + 4
    block_h = len(lines) * line_h + 16
    draw_rounded_rect(c, x, y - block_h, w, block_h, r=6, fill=CODE_BG, stroke=BORDER_GRAY)
    c.setFont("Courier", font_size)
    c.setFillColor(DARK_BLUE)
    ty = y - 12
    for line in lines:
        c.drawString(x + 10, ty, line)
        ty -= line_h
    return y - block_h

def draw_section_header(c, x, y, w, text, color=MED_BLUE, icon=None):
    """Draw a colored section header bar."""
    h = 26
    draw_rounded_rect(c, x, y - h, w, h, r=6, fill=color)
    c.setFont("Helvetica-Bold", 13)
    c.setFillColor(white)
    label = text
    if icon:
        label = icon + "  " + text
    c.drawString(x + 12, y - h + 8, label)
    return y - h - 6

def draw_arrow_annotation(c, x1, y1, x2, y2, text, color=ORANGE, font_size=8.5):
    """Draw a curved arrow from (x1,y1) to (x2,y2) with annotation text."""
    c.saveState()
    c.setStrokeColor(color)
    c.setLineWidth(1.5)
    # Simple line with arrowhead
    c.line(x1, y1, x2, y2)
    # Arrowhead
    import math
    angle = math.atan2(y2 - y1, x2 - x1)
    arrow_len = 8
    c.line(x2, y2, x2 - arrow_len * math.cos(angle - 0.4), y2 - arrow_len * math.sin(angle - 0.4))
    c.line(x2, y2, x2 - arrow_len * math.cos(angle + 0.4), y2 - arrow_len * math.sin(angle + 0.4))
    # Text
    c.setFont("Helvetica-Bold", font_size)
    c.setFillColor(color)
    mx = (x1 + x2) / 2
    my = (y1 + y2) / 2
    c.drawString(mx + 4, my + 2, text)
    c.restoreState()


# ═══════════════════════════════════════════════════════════════════
#  PAGE 1: THE MODEL + lm()
# ═══════════════════════════════════════════════════════════════════

def draw_page1(c):
    margin = 36
    content_w = W - 2 * margin
    y = H - 30

    # ── Title banner ──
    draw_rounded_rect(c, 0, H - 70, W, 70, r=0, fill=DARK_BLUE)
    c.setFont("Helvetica-Bold", 22)
    c.setFillColor(white)
    c.drawCentredString(W / 2, H - 32, "Simple Linear Regression in R")
    c.setFont("Helvetica", 12)
    c.drawCentredString(W / 2, H - 52, "A Visual Guide to lm() and summary()")
    y = H - 85

    # ── Business scenario box ──
    box_h = 72
    draw_rounded_rect(c, margin, y - box_h, content_w, box_h, r=8, fill=LIGHT_BLUE, stroke=ACCENT_BLUE, stroke_width=1.5)
    c.setFont("Helvetica-Bold", 12)
    c.setFillColor(DARK_BLUE)
    c.drawString(margin + 14, y - 18, "Business Scenario")
    c.setFont("Helvetica", 10)
    c.setFillColor(black)
    c.drawString(margin + 14, y - 35, "A marketing manager wants to know: does advertising spend predict monthly sales?")
    c.drawString(margin + 14, y - 49, "She has data on 30 stores: how much each spent on ads (in $1,000s) and their sales (in $1,000s).")
    c.setFont("Helvetica-Bold", 10)
    c.setFillColor(MED_BLUE)
    c.drawString(margin + 14, y - 64, "Y = Sales     |     X = Advertising Spend")
    y -= box_h + 12

    # ── The Model ──
    y = draw_section_header(c, margin, y, content_w, "The Linear Regression Model")

    # Model equation box
    eq_h = 60
    draw_rounded_rect(c, margin + 20, y - eq_h, content_w - 40, eq_h, r=8, fill=white, stroke=ACCENT_BLUE, stroke_width=1.5)
    c.setFont("Helvetica-Bold", 16)
    c.setFillColor(DARK_BLUE)
    c.drawCentredString(W / 2, y - 25, "Sales  =  b0  +  b1 x AdvertisingSpend  +  error")

    # Labels below equation
    c.setFont("Helvetica", 8.5)
    # b0
    c.setFillColor(GREEN)
    c.drawCentredString(W / 2 - 80, y - 42, "intercept:")
    c.drawCentredString(W / 2 - 80, y - 52, "predicted sales")
    c.drawCentredString(W / 2 - 80, y - 62, "when ads = $0")
    # b1
    c.setFillColor(ORANGE)
    c.drawCentredString(W / 2 + 20, y - 42, "slope:")
    c.drawCentredString(W / 2 + 20, y - 52, "change in sales for")
    c.drawCentredString(W / 2 + 20, y - 62, "each extra $1,000 in ads")
    # error
    c.setFillColor(RED)
    c.drawCentredString(W / 2 + 145, y - 42, "error:")
    c.drawCentredString(W / 2 + 145, y - 52, "what we")
    c.drawCentredString(W / 2 + 145, y - 62, "can't explain")

    y -= eq_h + 18

    # ── Step 1: lm() ──
    y = draw_section_header(c, margin, y, content_w, "Step 1: Fit the Model with lm()")
    c.setFont("Helvetica", 10)
    c.setFillColor(black)
    c.drawString(margin + 8, y - 2, "The lm() function fits a straight line through your data:")
    y -= 16

    y = draw_code_block(c, margin + 8, y, content_w - 16, [
        "model <- lm(Sales ~ AdvertisingSpend, data = stores)",
    ], font_size=11)
    y -= 6

    # Annotated breakdown
    ann_h = 78
    draw_rounded_rect(c, margin + 8, y - ann_h, content_w - 16, ann_h, r=6, fill=LIGHT_GREEN, stroke=GREEN)
    c.setFont("Helvetica-Bold", 9)
    c.setFillColor(GREEN)
    c.drawString(margin + 18, y - 14, "Reading this line:")
    c.setFont("Courier-Bold", 10)
    c.setFillColor(DARK_BLUE)
    c.drawString(margin + 18, y - 32, "model")
    c.drawString(margin + 115, y - 32, "lm(")
    c.drawString(margin + 155, y - 32, "Sales ~ AdvertisingSpend")
    c.drawString(margin + 395, y - 32, "data = stores")
    c.drawString(margin + 500, y - 32, ")")

    c.setFont("Helvetica", 8.5)
    c.setFillColor(DARK_GRAY)
    c.drawString(margin + 18, y - 48, "save result")
    c.drawString(margin + 18, y - 58, "for later")
    c.drawString(margin + 115, y - 48, "fit a")
    c.drawString(margin + 115, y - 58, "linear model")
    c.drawString(margin + 175, y - 48, "Y ~ X means")
    c.drawString(margin + 175, y - 58, "\"Y predicted by X\"")
    c.drawString(margin + 175, y - 68, "(~ reads as \"depends on\")")
    c.drawString(margin + 395, y - 48, "which data")
    c.drawString(margin + 395, y - 58, "frame to use")

    y -= ann_h + 12

    # ── What lm() gives you ──
    y = draw_section_header(c, margin, y, content_w, "What lm() Gives You")

    y -= 2
    y = draw_code_block(c, margin + 8, y, content_w - 16, [
        "> model",
        "",
        "Call:",
        "lm(formula = Sales ~ AdvertisingSpend, data = stores)",
        "",
        "Coefficients:",
        "      (Intercept)  AdvertisingSpend",
        "           12.351             2.876",
    ], font_size=9.5)
    y -= 6

    # Interpretation boxes side by side
    box_w = (content_w - 24) / 2
    box_h = 62

    # Intercept box
    draw_rounded_rect(c, margin + 8, y - box_h, box_w, box_h, r=6, fill=LIGHT_GREEN, stroke=GREEN)
    c.setFont("Helvetica-Bold", 10)
    c.setFillColor(GREEN)
    c.drawString(margin + 18, y - 16, "b0 = 12.351 (Intercept)")
    c.setFont("Helvetica", 9)
    c.setFillColor(black)
    c.drawString(margin + 18, y - 32, "A store that spends $0 on ads is")
    c.drawString(margin + 18, y - 44, "predicted to have $12,351 in sales.")
    c.setFont("Helvetica-Oblique", 8)
    c.setFillColor(DARK_GRAY)
    c.drawString(margin + 18, y - 57, "(May not be meaningful if no store actually spends $0)")

    # Slope box
    draw_rounded_rect(c, margin + 8 + box_w + 8, y - box_h, box_w, box_h, r=6, fill=LIGHT_ORANGE, stroke=ORANGE)
    c.setFont("Helvetica-Bold", 10)
    c.setFillColor(ORANGE)
    c.drawString(margin + 26 + box_w, y - 16, "b1 = 2.876 (Slope)")
    c.setFont("Helvetica", 9)
    c.setFillColor(black)
    c.drawString(margin + 26 + box_w, y - 32, "Each additional $1,000 in ads is")
    c.drawString(margin + 26 + box_w, y - 44, "associated with $2,876 more in sales.")
    c.setFont("Helvetica-Oblique", 8)
    c.setFillColor(DARK_GRAY)
    c.drawString(margin + 26 + box_w, y - 57, "(This is the key business insight!)")

    y -= box_h + 12

    # ── Quick tip ──
    tip_h = 36
    draw_rounded_rect(c, margin, y - tip_h, content_w, tip_h, r=6, fill=LIGHT_PURPLE, stroke=PURPLE)
    c.setFont("Helvetica-Bold", 9)
    c.setFillColor(PURPLE)
    c.drawString(margin + 14, y - 14, "Tip:  You can also get just the coefficients with:")
    c.setFont("Courier", 9)
    c.setFillColor(DARK_BLUE)
    c.drawString(margin + 14, y - 28, "coef(model)          # returns b0 and b1 as a named vector")

    # Footer
    c.setFont("Helvetica", 8)
    c.setFillColor(DARK_GRAY)
    c.drawCentredString(W / 2, 20, "Business Forecasting  |  ITAM  |  Page 1 of 3")


# ═══════════════════════════════════════════════════════════════════
#  PAGE 2: summary() — FULL ANNOTATED OUTPUT
# ═══════════════════════════════════════════════════════════════════

def draw_page2(c):
    margin = 36
    content_w = W - 2 * margin
    y = H - 20

    # ── Header ──
    draw_rounded_rect(c, 0, H - 50, W, 50, r=0, fill=DARK_BLUE)
    c.setFont("Helvetica-Bold", 18)
    c.setFillColor(white)
    c.drawCentredString(W / 2, H - 35, "Step 2: Understand the Output with summary()")
    y = H - 65

    y = draw_code_block(c, margin + 8, y, content_w - 16, [
        "summary(model)"
    ], font_size=11)
    y -= 8

    # ── FULL summary() output ──
    output_lines = [
        "Call:",
        "lm(formula = Sales ~ AdvertisingSpend, data = stores)",
        "",
        "Residuals:",
        "    Min      1Q  Median      3Q     Max",
        " -8.324  -2.147   0.381   2.503   7.196",
        "",
        "Coefficients:",
        "                 Estimate Std. Error t value Pr(>|t|)",
        "(Intercept)       12.351      3.214   3.842  0.00064 ***",
        "AdvertisingSpend   2.876      0.412   6.981  1.7e-07 ***",
        "---",
        "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1",
        "",
        "Residual standard error: 3.891 on 28 degrees of freedom",
        "Multiple R-squared: 0.6351",
    ]

    line_h = 13
    block_h = len(output_lines) * line_h + 20
    draw_rounded_rect(c, margin + 8, y - block_h, content_w - 16, block_h, r=6, fill=CODE_BG, stroke=BORDER_GRAY)

    c.setFont("Courier", 8.5)
    c.setFillColor(DARK_BLUE)
    ty = y - 14
    line_positions = {}
    for i, line in enumerate(output_lines):
        c.drawString(margin + 18, ty, line)
        line_positions[i] = ty
        ty -= line_h

    y -= block_h

    # ── Now annotate each section below ──
    y -= 10

    # Section A: Residuals
    y = draw_section_header(c, margin, y, content_w, "A  Residuals", color=MED_BLUE)
    box_h = 70
    draw_rounded_rect(c, margin + 8, y - box_h, content_w - 16, box_h, r=6, fill=LIGHT_BLUE, stroke=ACCENT_BLUE)
    c.setFont("Helvetica", 9.5)
    c.setFillColor(black)
    c.drawString(margin + 18, y - 16, "Residuals = Actual Sales - Predicted Sales  (the errors of your model)")
    c.setFont("Helvetica", 9)
    c.drawString(margin + 18, y - 32, "The five-number summary (Min, Q1, Median, Q3, Max) tells you how big the errors are.")
    c.setFont("Helvetica-Bold", 9)
    c.setFillColor(GREEN)
    c.drawString(margin + 18, y - 48, "What to look for:")
    c.setFont("Helvetica", 9)
    c.setFillColor(black)
    c.drawString(margin + 18, y - 62, "Median close to 0 is good (errors are balanced).  Here: 0.381, nicely centered around zero.")
    y -= box_h + 10

    # Section B: Coefficients table
    y = draw_section_header(c, margin, y, content_w, "B  Coefficients Table (the core of the output)", color=MED_BLUE)

    # Four columns explained
    cols = [
        ("Estimate", LIGHT_GREEN, GREEN,
         "The coefficients\nthemselves.\n\nb0 = 12.351\nb1 = 2.876"),
        ("Std. Error", LIGHT_ORANGE, ORANGE,
         "How uncertain\nis each estimate.\n\nSmaller = more\nprecise estimate."),
        ("t value", LIGHT_PURPLE, PURPLE,
         "= Estimate / Std.Error\n\nHow many standard\nerrors away from 0.\n\nBigger |t| = stronger."),
        ("Pr(>|t|)", LIGHT_RED, RED,
         "The p-value.\n\nProbability of seeing\nthis result if the\ntrue effect were 0."),
    ]

    col_w = (content_w - 40) / 4
    col_h = 95
    for i, (title, bg, fg, desc) in enumerate(cols):
        cx = margin + 8 + i * (col_w + 8)
        draw_rounded_rect(c, cx, y - col_h, col_w, col_h, r=6, fill=bg, stroke=fg)
        c.setFont("Helvetica-Bold", 9.5)
        c.setFillColor(fg)
        c.drawCentredString(cx + col_w / 2, y - 14, title)
        c.setFont("Helvetica", 8)
        c.setFillColor(black)
        lines = desc.split("\n")
        for j, line in enumerate(lines):
            c.drawCentredString(cx + col_w / 2, y - 28 - j * 10, line)
    y -= col_h + 10

    # Stars explanation
    star_h = 42
    draw_rounded_rect(c, margin + 8, y - star_h, content_w - 16, star_h, r=6, fill=LIGHT_ORANGE, stroke=ORANGE)
    c.setFont("Helvetica-Bold", 9)
    c.setFillColor(ORANGE)
    c.drawString(margin + 18, y - 14, "What do the stars (***) mean?")
    c.setFont("Courier", 8.5)
    c.setFillColor(black)
    c.drawString(margin + 18, y - 28, "***  p < 0.001      **  p < 0.01      *  p < 0.05      .  p < 0.1      (blank)  not significant")
    c.setFont("Helvetica-Oblique", 8)
    c.setFillColor(DARK_GRAY)
    c.drawString(margin + 18, y - 39, "More stars = stronger evidence that the variable matters.  Our slope has *** so advertising clearly matters!")

    y -= star_h + 8

    # Section C: R-squared and RSE
    y = draw_section_header(c, margin, y, content_w, "C  Model Fit Statistics", color=MED_BLUE)

    box_w2 = (content_w - 24) / 2
    box_h2 = 72

    # R-squared
    draw_rounded_rect(c, margin + 8, y - box_h2, box_w2, box_h2, r=6, fill=LIGHT_GREEN, stroke=GREEN)
    c.setFont("Helvetica-Bold", 11)
    c.setFillColor(GREEN)
    c.drawString(margin + 18, y - 18, "R-squared = 0.6351")
    c.setFont("Helvetica", 9)
    c.setFillColor(black)
    c.drawString(margin + 18, y - 34, "63.5% of the variation in sales is")
    c.drawString(margin + 18, y - 46, "explained by advertising spend.")
    c.setFont("Helvetica-Oblique", 8)
    c.setFillColor(DARK_GRAY)
    c.drawString(margin + 18, y - 62, "Range: 0 (model explains nothing)")
    c.drawString(margin + 18, y - 72, "to 1 (model explains everything)")

    # RSE
    draw_rounded_rect(c, margin + 8 + box_w2 + 8, y - box_h2, box_w2, box_h2, r=6, fill=LIGHT_PURPLE, stroke=PURPLE)
    c.setFont("Helvetica-Bold", 11)
    c.setFillColor(PURPLE)
    c.drawString(margin + 26 + box_w2, y - 18, "Residual Std. Error = 3.891")
    c.setFont("Helvetica", 9)
    c.setFillColor(black)
    c.drawString(margin + 26 + box_w2, y - 34, "On average, predictions are off by")
    c.drawString(margin + 26 + box_w2, y - 46, "about $3,891 from actual sales.")
    c.setFont("Helvetica-Oblique", 8)
    c.setFillColor(DARK_GRAY)
    c.drawString(margin + 26 + box_w2, y - 62, "Same units as Y (here: $1,000s).")
    c.drawString(margin + 26 + box_w2, y - 72, "Smaller = better predictions.")

    # Footer
    c.setFont("Helvetica", 8)
    c.setFillColor(DARK_GRAY)
    c.drawCentredString(W / 2, 20, "Business Forecasting  |  ITAM  |  Page 2 of 3")


# ═══════════════════════════════════════════════════════════════════
#  PAGE 3: PUTTING IT ALL TOGETHER + QUICK REFERENCE
# ═══════════════════════════════════════════════════════════════════

def draw_page3(c):
    margin = 36
    content_w = W - 2 * margin
    y = H - 20

    # ── Header ──
    draw_rounded_rect(c, 0, H - 50, W, 50, r=0, fill=DARK_BLUE)
    c.setFont("Helvetica-Bold", 18)
    c.setFillColor(white)
    c.drawCentredString(W / 2, H - 35, "Putting It All Together")
    y = H - 65

    # ── Complete workflow ──
    y = draw_section_header(c, margin, y, content_w, "Complete Workflow: From Data to Interpretation")

    y = draw_code_block(c, margin + 8, y, content_w - 16, [
        "# 1. Load data",
        "stores <- read.csv(\"stores.csv\")",
        "",
        "# 2. Quick scatterplot to visualize the relationship",
        "plot(stores$AdvertisingSpend, stores$Sales,",
        "     xlab = \"Advertising Spend ($1,000s)\",",
        "     ylab = \"Sales ($1,000s)\",",
        "     main = \"Sales vs Advertising\",",
        "     pch = 19, col = \"steelblue\")",
        "",
        "# 3. Fit the model",
        "model <- lm(Sales ~ AdvertisingSpend, data = stores)",
        "",
        "# 4. Add the regression line to the plot",
        "abline(model, col = \"red\", lwd = 2)",
        "",
        "# 5. See the full results",
        "summary(model)",
        "",
        "# 6. Make a prediction: what sales do we expect",
        "#    if we spend $15,000 on ads?",
        "predict(model, newdata = data.frame(AdvertisingSpend = 15))",
        "# Answer: 12.351 + 2.876 * 15 = $55,491",
    ], font_size=8.5)
    y -= 12

    # ── Visual: The scatterplot ──
    y = draw_section_header(c, margin, y, content_w, "What the Scatterplot Looks Like")

    plot_w = content_w - 16
    plot_h = 160
    plot_x = margin + 8
    plot_y = y - plot_h

    # Plot background
    draw_rounded_rect(c, plot_x, plot_y, plot_w, plot_h, r=6, fill=white, stroke=BORDER_GRAY)

    # Axes
    ax_left = plot_x + 50
    ax_bottom = plot_y + 30
    ax_right = plot_x + plot_w - 20
    ax_top = plot_y + plot_h - 20

    c.saveState()
    c.setStrokeColor(DARK_GRAY)
    c.setLineWidth(1)
    c.line(ax_left, ax_bottom, ax_right, ax_bottom)  # x-axis
    c.line(ax_left, ax_bottom, ax_left, ax_top)  # y-axis

    # Axis labels
    c.setFont("Helvetica", 8)
    c.setFillColor(DARK_GRAY)
    c.drawCentredString((ax_left + ax_right) / 2, plot_y + 8, "Advertising Spend ($1,000s)")
    c.saveState()
    c.translate(plot_x + 14, (ax_bottom + ax_top) / 2)
    c.rotate(90)
    c.drawCentredString(0, 0, "Sales ($1,000s)")
    c.restoreState()

    # Tick marks
    for i in range(6):
        tx = ax_left + i * (ax_right - ax_left) / 5
        c.setFont("Helvetica", 7)
        c.setFillColor(DARK_GRAY)
        c.drawCentredString(tx, ax_bottom - 10, str(i * 5))
        c.setStrokeColor(BORDER_GRAY)
        c.setLineWidth(0.3)
        c.line(tx, ax_bottom, tx, ax_top)

    for i in range(5):
        ty2 = ax_bottom + i * (ax_top - ax_bottom) / 4
        c.setFont("Helvetica", 7)
        c.setFillColor(DARK_GRAY)
        c.drawRightString(ax_left - 4, ty2 - 3, str(int(i * 20)))
        c.setStrokeColor(BORDER_GRAY)
        c.setLineWidth(0.3)
        c.line(ax_left, ty2, ax_right, ty2)

    # Scatter points (simulated data roughly matching b0=12.35, b1=2.876)
    import random
    random.seed(42)
    points = []
    for _ in range(30):
        xv = random.uniform(1, 25)
        yv = 12.351 + 2.876 * xv + random.gauss(0, 3.891)
        points.append((xv, yv))

    x_range = 25
    y_range = 80
    for (xv, yv) in points:
        px = ax_left + (xv / x_range) * (ax_right - ax_left)
        py = ax_bottom + (yv / y_range) * (ax_top - ax_bottom)
        c.setFillColor(ACCENT_BLUE)
        c.circle(px, py, 3, fill=1, stroke=0)

    # Regression line
    c.setStrokeColor(RED)
    c.setLineWidth(2.5)
    lx1 = 0
    ly1 = 12.351 + 2.876 * lx1
    lx2 = 25
    ly2 = 12.351 + 2.876 * lx2
    px1 = ax_left + (lx1 / x_range) * (ax_right - ax_left)
    py1 = ax_bottom + (ly1 / y_range) * (ax_top - ax_bottom)
    px2 = ax_left + (lx2 / x_range) * (ax_right - ax_left)
    py2 = ax_bottom + (ly2 / y_range) * (ax_top - ax_bottom)
    c.line(px1, py1, px2, py2)

    c.restoreState()

    # Annotation for the line
    c.setFont("Helvetica-Bold", 9)
    c.setFillColor(RED)
    c.drawString(ax_right - 160, py2 + 8, "Sales = 12.35 + 2.88 x Ads")

    # Annotation for a residual
    # Pick a point and show residual
    xv, yv = points[5]
    yhat = 12.351 + 2.876 * xv
    px = ax_left + (xv / x_range) * (ax_right - ax_left)
    py_actual = ax_bottom + (yv / y_range) * (ax_top - ax_bottom)
    py_pred = ax_bottom + (yhat / y_range) * (ax_top - ax_bottom)
    c.saveState()
    c.setStrokeColor(PURPLE)
    c.setLineWidth(1)
    c.setDash(3, 3)
    c.line(px, py_actual, px, py_pred)
    c.restoreState()
    c.setFont("Helvetica-Bold", 7.5)
    c.setFillColor(PURPLE)
    c.drawString(px + 4, (py_actual + py_pred) / 2, "residual")

    y = plot_y - 12

    # ── Quick Reference ──
    y = draw_section_header(c, margin, y, content_w, "Quick Reference: Useful Functions")

    funcs = [
        ("lm(Y ~ X, data)", "Fit the regression model"),
        ("summary(model)", "Full output: coefficients, p-values, R-squared"),
        ("coef(model)", "Extract just the coefficients (b0, b1)"),
        ("fitted(model)", "Get predicted values (Y-hat) for all observations"),
        ("residuals(model)", "Get residuals (Y - Y-hat) for all observations"),
        ("predict(model, newdata=...)", "Predict Y for new X values"),
        ("confint(model, level=0.95)", "95% confidence intervals for b0 and b1"),
        ("plot(X, Y); abline(model)", "Scatterplot with regression line"),
    ]

    row_h = 16
    table_h = len(funcs) * row_h + 24
    draw_rounded_rect(c, margin + 8, y - table_h, content_w - 16, table_h, r=6, fill=GRAY, stroke=BORDER_GRAY)

    # Header row
    c.setFont("Helvetica-Bold", 9)
    c.setFillColor(DARK_BLUE)
    c.drawString(margin + 18, y - 14, "Function")
    c.drawString(margin + 250, y - 14, "What It Does")
    ty = y - 28
    for func, desc in funcs:
        c.setFont("Courier", 8.5)
        c.setFillColor(MED_BLUE)
        c.drawString(margin + 18, ty, func)
        c.setFont("Helvetica", 8.5)
        c.setFillColor(black)
        c.drawString(margin + 250, ty, desc)
        ty -= row_h

    # Footer
    c.setFont("Helvetica", 8)
    c.setFillColor(DARK_GRAY)
    c.drawCentredString(W / 2, 20, "Business Forecasting  |  ITAM  |  Page 3 of 3")


# ═══════════════════════════════════════════════════════════════════
#  BUILD PDF
# ═══════════════════════════════════════════════════════════════════

output_path = r"C:\Users\kzysi\Dropbox\Itam_teaching\Markdowns\C4\C4_a\lm_summary_cheatsheet.pdf"

c = canvas.Canvas(output_path, pagesize=letter)

draw_page1(c)
c.showPage()
draw_page2(c)
c.showPage()
draw_page3(c)
c.showPage()

c.save()
print(f"PDF created: {output_path}")
