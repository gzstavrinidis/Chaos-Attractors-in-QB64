' Interactive 3D Attractor Viewer
' Features: Lorenz, Roessler, Sprott, and Langford (Aizawa) Attractors with dynamic 3D axes

' --- 1. Setup Screen & Input ---
Screen _NewImage(800, 600, 32)
_Title "Attractor Viewer: Lorenz, Roessler, Sprott, Aizawa"

' --- 2. Shared Variables ---
Dim Shared angle As Single
angle = 0.0

' Global coordinates and time step
Dim Shared x As Single, y As Single, z As Single
Dim Shared dt As Single

' Global Attractor State
Dim Shared currentSystem As Integer
Dim Shared zOffset As Single
Dim Shared sysScale As Single

' Axis lengths that dynamically adjust per system
Dim Shared axXY As Single
Dim Shared axZMin As Single, axZMax As Single

' Lorenz Parameters
Dim Shared sigma As Single, rho As Single, l_beta As Single
sigma = 10.0: rho = 28.0: l_beta = 8.0 / 3.0

' Roessler Parameters
Dim Shared r_a As Single, r_b As Single, r_c As Single
r_a = 0.2: r_b = 0.2: r_c = 5.7

' Sprott Parameters
Dim Shared s_a As Single, s_b As Single
s_a = 2.07: s_b = 1.79

' Aizawa (Langford) Parameters
Dim Shared a_a As Single, a_b As Single, a_c As Single, a_d As Single, a_e As Single, a_f As Single
a_a = 0.950: a_b = 0.700: a_c = 0.600: a_d = 3.500: a_e = 0.250: a_f = 0.100

' --- 3. Tail History Array Setup ---
Const TAIL_LEN = 2100
Dim Shared hX(TAIL_LEN) As Single
Dim Shared hY(TAIL_LEN) As Single
Dim Shared hZ(TAIL_LEN) As Single

' Initialize the system with Lorenz by default
InitializeSystem 1

' --- 4. Main Animation Loop ---
Do
    _Limit 60 ' Smooth 60 FPS

    ' Check for exit via ESC key
    If InKey$ = Chr$(27) Then Exit Do

    ' --- INPUT PHASE: Check Mouse Clicks ---
    Dim mx As Integer, my As Integer, mb1 As Integer
    Do While _MouseInput
        mx = _MouseX
        my = _MouseY
        mb1 = _MouseButton(1)

        If mb1 = -1 Then
            ' Button 1 (Lorenz)
            If mx >= 620 And mx <= 780 And my >= 20 And my <= 50 Then InitializeSystem 1
            ' Button 2 (Roessler)
            If mx >= 620 And mx <= 780 And my >= 60 And my <= 90 Then InitializeSystem 2
            ' Button 3 (Sprott)
            If mx >= 620 And mx <= 780 And my >= 100 And my <= 130 Then InitializeSystem 3
            ' Button 4 (Aizawa)
            If mx >= 620 And mx <= 780 And my >= 140 And my <= 170 Then InitializeSystem 4
        End If
    Loop

    ' --- MATH PHASE: Calculate next step based on active system ---
    Dim dx As Single, dy As Single, dz As Single

    If currentSystem = 1 Then
        ' Lorenz Equations
        dx = (sigma * (y - x)) * dt
        dy = (x * (rho - z) - y) * dt
        dz = (x * y - (l_beta * z)) * dt

    ElseIf currentSystem = 2 Then
        ' Roessler Equations
        dx = (-y - z) * dt
        dy = (x + r_a * y) * dt
        dz = (r_b + z * (x - r_c)) * dt

    ElseIf currentSystem = 3 Then
        ' Sprott Attractor Equations
        dx = (y + s_a * x * y + x * z) * dt
        dy = (1 - s_b * (x * x) + y * z) * dt
        dz = (x - (x * x) - (y * y)) * dt

    ElseIf currentSystem = 4 Then
        ' Aizawa (Langford) Equations
        dx = ((z - a_b) * x - a_d * y) * dt
        dy = (a_d * x + (z - a_b) * y) * dt
        dz = (a_c + a_a * z - (z * z * z) / 3.0 - ((x * x) + (y * y)) * (1.0 + a_e * z) + a_f * z * (x * x * x)) * dt
    End If

    x = x + dx: y = y + dy: z = z + dz

    ' Shift history array backwards
    Dim i As Integer
    For i = 1 To TAIL_LEN - 1
        hX(i) = hX(i + 1)
        hY(i) = hY(i + 1)
        hZ(i) = hZ(i + 1)
    Next
    hX(TAIL_LEN) = x: hY(TAIL_LEN) = y: hZ(TAIL_LEN) = z

    ' Rotate the camera slightly every frame
    angle = angle + 0.005
    If angle > 6.28318 Then angle = angle - 6.28318

    ' --- DRAWING PHASE ---
    Cls

    ' A. Draw 3D Axes System
    DrawLine3D -axXY, 0, 0, axXY, 0, 0, _RGB32(100, 0, 0) ' X Axis
    DrawLine3D 0, -axXY, 0, 0, axXY, 0, _RGB32(0, 100, 0) ' Y Axis
    DrawLine3D 0, 0, axZMin, 0, 0, axZMax, _RGB32(0, 50, 100) ' Z Axis

    ' B. Draw Fading Tail
    For i = 1 To TAIL_LEN - 1
        Dim fade As Single: fade = i / TAIL_LEN
        Dim r As Integer, g As Integer, b As Integer

        ' Assign distinct dynamic colors based on system loaded
        If currentSystem = 1 Then ' Lorenz (Cyan/Blue)
            r = Int((hZ(i) / 50.0) * 255 * fade)
            g = Int(180 * fade)
            b = Int((255 - r) * fade)
        ElseIf currentSystem = 2 Then ' Roessler (Red/Orange)
            b = Int((hZ(i) / 30.0) * 255 * fade)
            r = Int(180 * fade)
            g = Int((255 - b) * fade)
        ElseIf currentSystem = 3 Then ' Sprott (Green/Yellow)
            r = Int(Abs(hX(i)) * 120 * fade)
            g = Int(200 * fade)
            b = Int(Abs(hY(i)) * 100 * fade)
        Else ' Aizawa (Purple/Magenta)
            r = Int(160 * fade)
            g = Int(Abs(hZ(i)) * 80 * fade)
            b = Int(220 * fade)
        End If

        If r < 0 Then r = 0: If r > 255 Then r = 255
        If g < 0 Then g = 0: If g > 255 Then g = 255
        If b < 0 Then b = 0: If b > 255 Then b = 255

        DrawLine3D hX(i), hY(i), hZ(i), hX(i + 1), hY(i + 1), hZ(i + 1), _RGB32(r, g, b)
    Next

    ' C. Print Equations on Screen
    Color _RGB32(220, 220, 220), _RGB32(0, 0, 0)
    If currentSystem = 1 Then
        _PrintString (20, 20), "Lorenz Attractor"
        Color _RGB32(150, 220, 255)
        _PrintString (20, 45), "dx/dt = sigma * (y - x)"
        _PrintString (20, 65), "dy/dt = x * (rho - z) - y"
        _PrintString (20, 85), "dz/dt = x * y - beta * z"
    ElseIf currentSystem = 2 Then
        _PrintString (20, 20), "Roessler Attractor"
        Color _RGB32(255, 180, 150)
        _PrintString (20, 45), "dx/dt = -y - z"
        _PrintString (20, 65), "dy/dt = x + a * y"
        _PrintString (20, 85), "dz/dt = b + z * (x - c)"
    ElseIf currentSystem = 3 Then
        _PrintString (20, 20), "Sprott Attractor"
        Color _RGB32(150, 255, 150)
        _PrintString (20, 45), "dx/dt = y + axy + xz"
        _PrintString (20, 65), "dy/dt = 1 - bx^2 + yz"
        _PrintString (20, 85), "dz/dt = x - x^2 - y^2"
    ElseIf currentSystem = 4 Then
        _PrintString (20, 20), "Langford (Aizawa) Attractor"
        Color _RGB32(220, 150, 255)
        _PrintString (20, 45), "dx/dt = (z - b)x - dy"
        _PrintString (20, 65), "dy/dt = dx + (z - b)y"
        _PrintString (20, 85), "dz/dt = c + az - z^3/3 - (x^2+y^2)(1+ez) + fzx^3"
    End If

    ' D. Draw Interactive Buttons
    DrawButton 620, 20, 780, 50, "Load Lorenz", currentSystem = 1
    DrawButton 620, 60, 780, 90, "Load Roessler", currentSystem = 2
    DrawButton 620, 100, 780, 130, "Load Sprott", currentSystem = 3
    DrawButton 620, 140, 780, 170, "Load Aizawa", currentSystem = 4

    _Display
Loop

End

' --- Subroutines ---

Sub DrawButton (x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer, txt As String, isActive As Integer)
    Line (x1, y1)-(x2, y2), _RGB32(40, 40, 50), BF
    Line (x1, y1)-(x2, y2), _RGB32(200, 200, 200), B
    Color _RGB32(255, 255, 255), _RGB32(40, 40, 50)
    _PrintString (x1 + 25, y1 + 10), txt
    If isActive Then Line (x1 - 2, y1 - 2)-(x2 + 2, y2 + 2), _RGB32(0, 255, 0), B
End Sub

Sub InitializeSystem (sysType As Integer)
    currentSystem = sysType
    Dim i As Integer

    If sysType = 1 Then ' Lorenz
        x = 0.1: y = 0.0: z = 0.0
        dt = 0.005: zOffset = 25.0: sysScale = 650
        axXY = 35.0: axZMin = -10.0: axZMax = 60.0
    ElseIf sysType = 2 Then ' Roessler
        x = 5.0: y = 0.0: z = 0.0
        dt = 0.015: zOffset = 5.0: sysScale = 650
        axXY = 35.0: axZMin = -10.0: axZMax = 40.0
    ElseIf sysType = 3 Then ' Sprott
        x = 0.63: y = 0.47: z = -0.54
        dt = 0.01: zOffset = 0.0: sysScale = 9000
        axXY = 2.5: axZMin = -2.5: axZMax = 2.5
    ElseIf sysType = 4 Then ' Aizawa
        x = 0.1: y = 0.0: z = 0.0
        dt = 0.01: zOffset = 0.5: sysScale = 8500
        axXY = 2.5: axZMin = -1.0: axZMax = 3.0
    End If

    For i = 1 To TAIL_LEN
        hX(i) = x: hY(i) = y: hZ(i) = z
    Next
End Sub

Sub DrawLine3D (x1 As Single, y1 As Single, z1 As Single, x2 As Single, y2 As Single, z2 As Single, c As _Unsigned Long)
    Dim camDist As Single: camDist = 110
    Dim tiltAngle As Single: tiltAngle = 0.5

    Dim az1 As Single: az1 = z1 - zOffset
    Dim az2 As Single: az2 = z2 - zOffset

    Dim yawX1 As Single, yawY1 As Single, yawZ1 As Single
    yawX1 = x1 * Cos(angle) - az1 * Sin(angle)
    yawY1 = y1
    yawZ1 = x1 * Sin(angle) + az1 * Cos(angle)

    Dim pX1 As Single, pY1 As Single, pZ1 As Single
    pX1 = yawX1
    pY1 = yawY1 * Cos(tiltAngle) - yawZ1 * Sin(tiltAngle)
    pZ1 = yawY1 * Sin(tiltAngle) + yawZ1 * Cos(tiltAngle)

    Dim yawX2 As Single, yawY2 As Single, yawZ2 As Single
    yawX2 = x2 * Cos(angle) - az2 * Sin(angle)
    yawY2 = y2
    yawZ2 = x2 * Sin(angle) + az2 * Cos(angle)

    Dim pX2 As Single, pY2 As Single, pZ2 As Single
    pX2 = yawX2
    pY2 = yawY2 * Cos(tiltAngle) - yawZ2 * Sin(tiltAngle)
    pZ2 = yawY2 * Sin(tiltAngle) + yawZ2 * Cos(tiltAngle)

    Dim d1 As Single: d1 = camDist - pZ1
    Dim sx1 As Single, sy1 As Single
    sx1 = (pX1 / d1) * sysScale + 400
    sy1 = -(pY1 / d1) * sysScale + 300

    Dim d2 As Single: d2 = camDist - pZ2
    Dim sx2 As Single, sy2 As Single
    sx2 = (pX2 / d2) * sysScale + 400
    sy2 = -(pY2 / d2) * sysScale + 300

    Line (sx1, sy1)-(sx2, sy2), c
END SUB

