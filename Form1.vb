' simple 8080 assembler.
'(c)2023 by www.vabolis.lt
Imports System.IO
Imports System.Reflection
Imports System.Text
Imports System.Text.RegularExpressions


Public Class ASM8080
    Dim keywords As New List(Of String) From {"START", "END", "ORG", ".ORG", ".TITLE"}

    Dim ASM As New List(Of String) From
        {
        "XCHG", "NOP", "LDA", "STA",
        "JMP", "CALL", "EQU", "=", "RRC", "RAR", "CMA", "CMC", "RET", "PCHL",
        "SPHL", "RZ", "RC", "RPE", "RM", "CNZ", "CNC", "CPO", "CP", "OUT",
        "XTHL", "DI", "JNZ", "JNC", "JPO", "JP",
        "RNZ", "RNC", "RPO", "RP", "SHLD", "HLT",
        "RLC", "RAL", "DAA", "STC", "LDHL",
        "ADD", "ADC", "SUB", "SBB", "ANA", "XRA", "ORA", "CMP",
        "MOV", "EI",
        "CPI", "XRI", "SBI", "ACI", "IN", "ADI", "SUI", "ANI", "ORI",
        "LXI", ".EQU", "MVI", "LHLD",
        "INX", "INR", "DCR", "DAD", "DCX", "LDAX", "PUSH", "POP", "STAX", "RST",
        "JZ", "CZ", "JC", "CC", "JPE", "CPE", "JM", "CM",
        "DB", ".DB", "DW", ".DW", "DS", ".DS"}
    Dim ASM_A As New List(Of Byte) From
        {
        &HEB, 0, &H3A, &H32,
        &HC3, &HCD, 0, 0, &HF, &H1F, &H2F, &H3F, &HC9, &HE9,
        &HF9, &HC8, &HD8, &HE8, &HF8, &HC4, &HD4, &HE4, &HF4, &HD3,
        &HE3, &HF3, &HC2, &HD2, &HE2, &HF2,
        &HC0, &HD0, &HE0, &HF0, &H22, &H76,
        &H7, &H17, &H27, &H37, &H2A,
        &H80, &H88, &H90, &H98, &HA0, &HA8, &HB0, &HB8,
        &H40, &HFB,
        &HFE, &HEE, &HDE, &HCE, &HDB, &HC6, &HD6, &HE6, &HF6,
        &H1, 0, &H6, &H2A,
        &H3, &H4, &H5, &H9, &HB, &HA, &HC5, &HC1, &H2, &HC7,
        &HCA, &HCC, &HDA, &HDC, &HEA, &HEC, &HFA, &HFC,
        0, 0, 0, 0, 0, 0
    }

    Dim ASM_L As New List(Of Byte) From
        {
        1, 1, 3, 3,
         3, 3, 0, 0, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 3, 3, 3, 3, 2,
        1, 1, 3, 3, 3, 3,
        1, 1, 1, 1, 3, 1,
        1, 1, 1, 1, 3,
        1, 1, 1, 1, 1, 1, 1, 1,
        1, 1,
        2, 2, 2, 2, 2, 2, 2, 2, 2,
        3, 0, 2, 3,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        3, 3, 3, 3, 3, 3, 3, 3,
        0, 0, 0, 0, 0, 0}


    Dim labels As New List(Of String)
    Dim labels_adr As New List(Of UInt32)
    Dim labels_lineno As New List(Of UInt32)

    Dim errors As New List(Of String)
    Dim listing As New StringBuilder

    Dim separatoriai() As Char = {" ", vbTab}
    Dim trimchars() As Char = {" ", ":", vbTab, vbCr, vbCrLf}

    Dim current_adr As UInt32

    Dim ram_adr As UInt32
    Dim RAM(&H10000) As Byte

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim tmp As String
        Dim i, j As Int16

        listing = New StringBuilder

        current_adr = 0

        Dim lastLine = RichTextBox1.Lines.Length - 1
        For j = 0 To 1
            RichTextBox2.AppendText("%%%%%% PASS #" + j.ToString + vbCrLf)
            For i = 0 To lastLine
                tmp = RichTextBox1.Lines(i)
                Analyze_line(tmp, i, j)
            Next
        Next j

        RichTextBox2.AppendText(listing.ToString + vbCrLf)
        If labels.Count > 0 Then
            RichTextBox2.AppendText(vbCrLf + "%%%%%% LABEL DATABASE:" + vbCrLf)
            For i = 0 To labels.Count - 1
                RichTextBox2.AppendText(labels(i) + ":" + vbTab + "$" + HEX4(labels_adr(i)) + vbTab + "; defined at line:" + labels_lineno(i).ToString + vbCrLf)
            Next
        End If

        If errors.Count > 0 Then
            RichTextBox2.AppendText(vbCrLf + vbCrLf + "%%%%% ERROR REPORT:" + vbCrLf)
            RichTextBox2.AppendText(String.Join("", errors.ToArray))
        End If

        If current_adr <> ram_adr Then
            RichTextBox2.AppendText("%%%%%% ADR from assembler:" + Hex(current_adr) + " vs ADR from memcounter:" + Hex(ram_adr) + vbCrLf)
        End If

    End Sub
    ''' <summary>
    ''' Convert integer to fixed lenght (4) hex string
    ''' With padding zeroes. Value trimet to word
    ''' </summary>
    ''' <param name="a">input integer</param>
    ''' <returns></returns>
    Public Shared Function HEX4(a As UInt32) As String
        Dim s As String = ""
        a = a And &HFFFF
        If a < &H10 Then s += "0"
        If a < &H100 Then s += "0"
        If a < &H1000 Then s += "0"
        s += Hex(a)
        Return s
    End Function
    ''' <summary>
    ''' Convert integert to fixed lenght byte (2) string
    ''' With padding zeroes, value trimmed to byte
    ''' </summary>
    ''' <param name="a"></param>
    ''' <returns></returns>
    Public Shared Function HEX2(a As UInt32) As String
        a = a And 255
        Dim s As String = ""
        If a < 16 Then s = "0"
        Return s + Hex(a)
    End Function
    ''' <summary>
    ''' Convert HEX string to integer is safe way
    ''' </summary>
    ''' <param name="a">Input string</param>
    ''' <returns>Hex converted to integer</returns>
    Public Shared Function PHEX(a As String) As UInt32
        a = a.ToUpper.Trim
        a = Regex.Replace(a, "[^0-9A-F]", "")
        Return CUInt("&H0" + a)
    End Function

    Dim kabutese As String = "" 'work'a'round
    ''' <summary>
    ''' Parse source code line
    ''' </summary>
    ''' <param name="line">Listing line as string</param>
    ''' <param name="lineno">Line number as integer (for error lisntings)</param>
    ''' <param name="pass">Pass as byte. 0 - initial analyze and label collection,
    ''' 2 - full compilation. Must run both passes.</param>
    Private Sub Analyze_line(line As String, lineno As UInt32, pass As Byte)

        Dim tmp_string, tmp_string2, tmp_string3 As String
        Dim tmps As String
        Dim registras1, registras2 As Byte
        kabutese = ""
        Dim opcode As Byte

        line = Regex.Replace(line, "[^ -~]/g", "") '// ALL printable?
        line = CleanSpacesProperly(line) '//Remove first 3 space fills, tab fils. Remove tabs.

        If line.Contains("""") Then
            Dim ka = line.IndexOf("""")
            Dim kb = line.LastIndexOf("""")
            If ka < kb Then
                kabutese = line.Substring(ka + 1)
                kabutese = kabutese.Remove(kb - ka - 1)
            End If
        ElseIf line.Contains("'") Then
        Dim ka = line.IndexOf("'")
            Dim kb = line.LastIndexOf("'")
            If ka < kb Then
                kabutese = line.Substring(ka + 1)
                kabutese = kabutese.Remove(kb - ka - 1)
            End If
        End If

        '// ";" - komentaras.
        If line.Contains(";") And kabutese <> ";" Then
            Dim b = line.IndexOf(";")
            line = line.Substring(0, b)
        End If

        Dim a = line.Split(separatoriai)
        '//Komanda,label,parametrai

        tmp_string = a(0).Trim(trimchars)
        tmp_string2 = ""
        tmp_string3 = ""
        If a.Length > 1 Then
            If a(1) IsNot Nothing Then
                tmp_string2 = a(1).Trim(trimchars)
            End If
        End If
        If a.Length > 2 Then
            If a(2) IsNot Nothing Then
                tmp_string3 = a(2).Trim(trimchars)
            End If
        End If
        If a.Length > 3 Then
            Dim i
            For i = 3 To a.Length - 1
                tmp_string3 = tmp_string3 + " " + a(i)
            Next
        End If


        tmp_string = tmp_string.ToUpper
        If keywords.Contains(tmp_string) Or keywords.Contains(tmp_string2) Then '//KOMANDOS
            If tmp_string = "ORG" Or tmp_string = ".ORG" Then
                current_adr = CalculateValue(tmp_string2)
                'listing.Append("New ORG: " + HEX4(current_adr) + vbCrLf)
                Exit Sub
            ElseIf tmp_string.Contains(".TITLE") Then
                listing.Append("TITLE" + vbCrLf)
                tmp_string = ""
                tmp_string2 = ""
                tmp_string3 = ""

            ElseIf tmp_string2 = "ORG" Or tmp_string2 = ".ORG" Then
                current_adr = CalculateValue(tmp_string3)
                'listing.Append("New ORG: " + HEX4(current_adr) + vbCrLf)
                Exit Sub
            ElseIf tmp_string = "END" Or tmp_string2 = "END" Then
                listing.Append("ADR:" + HEX4(current_adr) + ", END direktyva at line:" + lineno.ToString + vbCrLf)
                Exit Sub
            ElseIf tmp_string = "START" Or tmp_string2 = "START" Then
                listing.Append("START direktyva at line:" + lineno.ToString + vbCrLf)
                'nothing
            End If
        ElseIf pass = 0 Then
            tmp_string2 = tmp_string2.ToUpper
            '//labels
            If tmp_string <> "" Then
                If Not labels.Contains(tmp_string) Then
                    labels.Add(tmp_string)

                    If tmp_string2 = "EQU" Or tmp_string2 = ".EQU" Or tmp_string2 = "=" Then
                        labels_adr.Add(CalculateValue(tmp_string3))
                        labels_lineno.Add(lineno + 1)
                    Else
                        labels_adr.Add(current_adr)
                        labels_lineno.Add(lineno + 1)
                    End If

                    If tmp_string2 = "DS" Or tmp_string2 = ".DS" Or tmp_string2 = "DB" Or tmp_string2 = ".DB" Or tmp_string2 = "DW" Or tmp_string2 = ".DW" Then
                        current_adr = current_adr + Calcualte_DxLen(tmp_string2, tmp_string3)
                    End If
                    'listing.Append(HEX4(current_adr) + " Nauja label:" + tmp_string + vbCrLf)
                Else
                    ReportError("Dublicate label: " + tmp_string + " at line:" + lineno.ToString + ". Previous labels defined at line:" + labels_lineno(labels.IndexOf(tmp_string)).ToString)
                End If
            ElseIf tmp_string2 = "DS" Or tmp_string2 = ".DS" Or tmp_string2 = "DB" Or tmp_string2 = ".DB" Or tmp_string2 = "DW" Or tmp_string2 = ".DW" Then
                current_adr = current_adr + Calcualte_DxLen(tmp_string2, tmp_string3)
            End If
        ElseIf pass = 1 Then

        End If

        If tmp_string2 <> "" Then
            tmp_string2 = tmp_string2.ToUpper
            If ASM.Contains(tmp_string2) Then 'ASM operatoriai
                If pass = 0 Then
                    Dim i = ASM.IndexOf(tmp_string2)
                    current_adr = (current_adr + ASM_L(i))
                    If current_adr > &HFFFF Then
                        ReportError("RAM adr overflow counting source. Program too long?")
                        current_adr = current_adr And &HFFFF
                    End If

                ElseIf pass = 1 Then


                    Dim i = ASM.IndexOf(tmp_string2)

                    If ASM(i) = "ADD" Or ASM(i) = "SUB" Or ASM(i) = "ADC" Or ASM(i) = "SBB" Or ASM(i) = "ANA" Or ASM(i) = "XRA" Or ASM(i) = "ORA" Or ASM(i) = "CMP" Then
                        registras1 = AnalizuotiRegistra(tmp_string3.ToUpper)
                        CA(current_adr)
                        opcode = ASM_A(i) + registras1
                        listing.Append(HEX2(opcode))
                        CD(opcode)
                    ElseIf ASM(i) = "INX" Or ASM(i) = "INR" Or ASM(i) = "DCR" Or ASM(i) = "DAD" Or ASM(i) = "DCX" Or ASM(i) = "LDAX" Or ASM(i) = "POP" Or ASM(i) = "PUSH" Or ASM(i) = "STAX" Or ASM(i) = "RST" Then
                        registras1 = AnalizuotiRegistra(tmp_string3.ToUpper)
                        CA(current_adr)
                        opcode = ASM_A(i) + (registras1 << 3)
                        listing.Append(HEX2(opcode))
                        CD(opcode)
                    ElseIf ASM(i) = "DB" Or ASM(i) = ".DB" Then
                        CA(current_adr)
                        CalculateDB(tmp_string3, listing, current_adr, True)
                        tmp_string3 = ""
                    ElseIf ASM(i) = "DW" Or ASM(i) = ".DW" Then
                        CA(current_adr)
                        CalculateDW(tmp_string3, listing, current_adr, True)
                        tmp_string3 = ""
                    ElseIf ASM(i) = "DS" Or ASM(i) = ".DS" Then 'iterpti tiek vienetu, kiek nurodyta.
                        Dim tmp As Int32 = CalculateValue(tmp_string3) - 1
                        If tmp >= 0 Then
                            CA(current_adr)
                            listing.Append(";Reserved bytes at [" + HEX4(current_adr) + "-")
                            current_adr = current_adr + tmp
                            CA(current_adr)
                            listing.Append("]")
                            current_adr += 1
                        Else
                            errors.Add("DS value at line " + lineno.ToString + " is 0. Ignored.")
                        End If

                        tmp_string3 = ""
                        'DD 32bitai

                    ElseIf ASM(i) = "LXI" Or ASM(i) = "MVI" Then
                        Dim rr() As String = tmp_string3.Split(",")
                        If rr.Length = 2 Then
                            registras1 = AnalizuotiRegistra(rr(0))
                            CA(current_adr)
                            opcode = ASM_A(i) + (registras1 << 3)
                            listing.Append(HEX2(opcode))
                            CD(opcode)
                            tmp_string3 = rr(1)
                        Else
                            ReportError(ASM(i) + " problem (no register?) at line:" + lineno.ToString + ": " + line)
                        End If


                    ElseIf ASM(i) = "MOV" Then
                        Dim rr() As String = tmp_string3.Split(",")
                        If rr.Length = 2 Then
                            registras1 = AnalizuotiRegistra(rr(0))
                            registras2 = AnalizuotiRegistra(rr(1))
                            If registras1 > 7 Or registras2 > 7 Then
                                ReportError(HEX4(current_adr) + "Unknown register in args:" + tmp_string3 + " at line: " + lineno.ToString)
                            Else
                                CA(current_adr)
                                opcode = ASM_A(i) + registras2 + (registras1 << 3)
                                listing.Append(HEX2(opcode))
                                CD(opcode)
                            End If

                        Else
                            'error!
                            ReportError(HEX4(current_adr) + "Too many or few args:" + tmp_string3 + " at line: " + lineno.ToString)
                        End If
                        tmp_string3 = ""
                    Else
                        CA(current_adr)
                        listing.Append(HEX2(ASM_A(i)))
                        CD(ASM_A(i))
                    End If

                    current_adr = current_adr + ASM_L(i)
                    '//search for label
                    If labels.Contains(tmp_string3) Then

                        Dim l As UInt32 = CalculateValue(tmp_string3)

                        If ASM_L(i) = 2 Then
                            listing.Append(" " + HEX2(l And 255) + vbTab)
                            CD(l)
                            tmps = " " + HEX2(l And 255)
                        ElseIf ASM_L(i) = 3 Then
                            'listing.Append(" " + HEX2(l And 255) + " " + HEX2(l >> 8)) '//LSB-MSB
                            listing.Append(HexPair(l))
                            CD(l And 255) : CD(l >> 8)
                            tmps = " " + HEX4(l)
                        Else
                            tmps = ""
                        End If

                        listing.Append(vbTab + ": ")
                        listing.Append(ASM(i))
                        listing.Append(tmps)

                    Else
                        '//no label? direct value?
                        Dim k = CalculateValue(tmp_string3)
                        If ASM_L(i) = 1 Then listing.Append(vbTab)
                        If ASM_L(i) = 2 Then listing.Append(" " + HEX2(k) + vbTab) : CD(k) '//tiesioginis baito krovimas
                        '        If ASM_L(i) = 3 Then listing.Append(" " + HEX2(k And 255) + " " + HEX2(k >> 8)) : CD(k And 255) : CD(k >> 8) '//LSB-MSB
                        If ASM_L(i) = 3 Then listing.Append(HexPair(k)) : CD(k And 255) : CD(k >> 8) '//LSB-MSB



                        listing.Append(vbTab)
                        listing.Append(": " + ASM(i))

                        If ASM_L(i) = 2 Then listing.Append(" #" + HEX2(k))
                        If ASM_L(i) = 3 Then listing.Append(" #" + HEX4(k))


                    End If



                    '//error
                    listing.Append(vbTab + vbTab + "; " + line)
                    listing.Append(vbCrLf)
                End If
            Else
                ReportError("Unknown ASM OPCODE at line:" + lineno.ToString + ", [" + tmp_string2 + "][" + tmp_string3 + "]")

            End If
        End If

    End Sub
    ''' <summary>
    ''' Calculate register as binary from symbolic name.
    ''' Works both in single, dual, number and specific format.
    ''' Used to calculate different opcodes.
    ''' </summary>
    ''' <param name="a">Register name</param>
    ''' <returns>Register number as byte</returns>
    Private Function AnalizuotiRegistra(a As String) As Byte
        Dim b As Byte
        a = a.ToUpper
        a = a.Trim(trimchars)
        If a = "" Then
            ReportError("Empty or wrong (wrong chars) register name.")
            Return 0 '//briedas
        End If
        Select Case a

            Case "B", "BC", "0" : b = 0
            Case "C", "1" : b = 1
            Case "D", "2", "DE" : b = 2
            Case "E", "3" : b = 3
            Case "H", "4", "HL" : b = 4
            Case "L", "5" : b = 5
            Case "M", "6", "SP", "PSW" : b = 6
            Case "A", "7" : b = 7

            Case Else : b = 0 : ReportError("Register selection error: [" + a + "].") '//using "default value", only report
        End Select
        Return b

    End Function
    ''' <summary>
    ''' Calculates value from string. Can convert:
    ''' decimal, hexadecimal, octal, label, list (comma separated),
    ''' chars (encapsulated in ' or "), simple math: a+1, 'A"-1, label-'a',...
    ''' Warning: this function is recursive.
    ''' </summary>
    ''' <param name="a">String of value (values)</param>
    ''' <returns>Value as int</returns>
    Private Function CalculateValue(a As String) As UInt32
        Dim l As UInt32
        Dim tmp_a = a
        a = a.Trim(trimchars)

        If a = "" Then Return 0
        If a.Length < 1 Then Return 0 '? kodel nesuveike?

        If a = """""" And kabutese <> "" Then                                       ' ""
            a = kabutese
            l = Asc(a)
        ElseIf a = """""" And kabutese = "" Then
            ReportError("Empty value. Sustituted with 0. 2")
            l = 0
        ElseIf a = "''" And kabutese <> "" Then                                     ' ''
            a = kabutese
            l = Asc(a)
        ElseIf a.StartsWith("""") And a.EndsWith("""") And a.Length > 3 Then
            '//STRING! cia neturi patekti!
            'a = a.Trim("""")
            'Dim i
            'For i = 0 To a.Length - 1
            '    '//????
            'Next
            a = a

        ElseIf labels.Contains(a) Then
            Dim k = labels.IndexOf(a)
            l = labels_adr(k)
        ElseIf a.Length = 3 And (a.Chars(0) = "'" Or a.Chars(0) = """") Then
            If a.Chars(0) = "'" And a.Chars(2) = "'" And a.Length = 3 Then          ' 'x'
                l = Asc(a.Chars(1))
            ElseIf a.Chars(0) = """" And a.Chars(2) = """" And a.Length = 3 Then    ' "x"
                l = Asc(a.Chars(1))
            End If
        ElseIf a.Contains("+") Then
            Dim b() As String = a.Split("+")
            If b.Length = 2 Then
                l = CalculateValue(b(0)) + CalculateValue(b(1))
            End If
        ElseIf a.Contains("-") Then
            Dim b() As String = a.Split("-")
            If b.Length = 2 Then
                Dim c As Int32 = CInt(CalculateValue(b(0))) - CInt(CalculateValue(b(1)))
                l = c And &HFFFF
            End If


        Else
            ' paprastas skacius
            If a.StartsWith("$") Or a.EndsWith("H") Or a.EndsWith("h") Then
                l = PHEX(a.ToUpper)
            ElseIf a.Contains("O") Or a.EndsWith("Q") Or a.EndsWith("q") Then
                l = Val("&O0" + Regex.Replace(a, "[^0-8]", "")) And &HFFFF
            Else
                Regex.Replace(a, "[^\d]", "")
                If a = "" Then ReportError("No value calculated: [" + tmp_a + "]") : Return 0
                l = Val(a)
            End If

        End If
        Return l
    End Function
    ''' <summary>
    ''' Push data to listing and RAM buffer. Increments RAM adr.
    ''' </summary>
    ''' <param name="d">data to store</param>
    Private Sub CD(d As UInt32)
        'If d > 255 Then ReportError("Internal byte overflow: [" + Hex(d) + "].")

        d = d And 255
        RAM(ram_adr) = d

        ram_adr += 1
        If ram_adr > &HFFFF Then
            ReportError("RAM adr overflow writring byte. Program too long?")
            ram_adr = ram_adr And &HFFFF
        End If
    End Sub
    ''' <summary>
    ''' Convert integer data to string as pair of hex bytes.
    ''' </summary>
    ''' <param name="l">integer</param>
    ''' <returns>Formated hex string</returns>
    Private Function HexPair(l As UInt32) As String
        Return HEX2(l And 255) + " " + HEX2(l >> 8)
    End Function
    ''' <summary>
    ''' Construct Address
    ''' </summary>
    ''' <param name="a">ADRESS</param>
    Private Sub CA(a As UInt32)
        listing.Append(HEX4(a) + ": ")
        If a > &HFFFF Then
            ReportError("RAM adr overflow: [" + Hex(a) + "]")
        End If
        ram_adr = a And &HFFFF
    End Sub
    ''' <summary>
    ''' Push error message to errors list and listing
    ''' </summary>
    ''' <param name="a">Error text</param>
    Private Sub ReportError(a As String)
        errors.Add(a + vbCrLf)
        listing.Append(" ERROR: " + a + vbCrLf)
    End Sub
    ''' <summary>
    ''' Calculate how many bytes are in data string (DB,DW,DS)
    ''' </summary>
    ''' <param name="type">Type of ASM command</param>
    ''' <param name="value">String of data</param>
    ''' <returns>Number of bytes</returns>
    Private Function Calcualte_DxLen(type As String, value As String) As UInt32
        Dim l As UInt32 = 0
        Dim n As New StringBuilder
        Select Case type
            Case "DB", ".DB"
                CalculateDB(value, n, l, False)
            Case "DW", ".DW"
                CalculateDW(value, n, l, False)
            Case "DS", ".DS"
                CalculateDS(value, n, l)
        End Select
        Return l
    End Function
    Private Sub UnfoldTheString(value As String, ByRef listing As StringBuilder, ByRef current_adr As UInt32, write As Boolean)
        value = value.Trim("""")
        Dim i As UInt32
        Dim a As Byte

        For i = 0 To value.Length - 1
            a = Asc(value.Chars(i))
            If write Then CD(a)
            listing.Append(HEX2(a) + " ")
            current_adr += 1
        Next
    End Sub
    ''' <summary>
    ''' Calculate DB string to data.
    ''' </summary>
    ''' <param name="value">String of data</param>
    ''' <param name="listing">Pointer to listing</param>
    ''' <param name="current_adr">Current adr of DB</param>
    ''' <param name="write">Boolean: True=write data, False=just count</param>
    Private Sub CalculateDB(value As String, ByRef listing As StringBuilder, ByRef current_adr As UInt32, write As Boolean)
        Dim a As UInt32


        If value = """,""" Or value = "','" Then '2E
            listing.Append("2E ")
            CD(&H2E)
        ElseIf value.Contains(",") Then
            Dim charss As String() = value.Split(",")
            For Each chars In charss
                If chars.StartsWith("""") And chars.EndsWith("""") And chars.Length > 3 Then
                    UnfoldTheString(chars, listing, current_adr, write)
                Else
                    a = CalculateValue(chars)
                    listing.Append(HEX2(a) + " ")
                    If write Then CD(a)
                    current_adr += 1
                End If

            Next

            'ElseIf value.Contains("""") Then
            '    a = a

            'ElseIf value.Contains("'") Then
            '    a = a

        Else
            If value.StartsWith("""") And value.EndsWith("""") And value.Length > 3 Then
                UnfoldTheString(value, listing, current_adr, write)
            Else
                a = CalculateValue(value)
                listing.Append(HEX2(a))
                If write Then CD(a)
                current_adr += 1
            End If
        End If
        'listing.Append(vbTab)
    End Sub
    ''' <summary>
    ''' Calculate DW string to data.
    ''' </summary>
    ''' <param name="value">String of data</param>
    ''' <param name="listing">Pointer to listing</param>
    ''' <param name="current_adr">Current adr of DB</param>
    ''' <param name="write">Boolean: True=write data, False=just count</param>
    Private Sub CalculateDW(value As String, ByRef listing As StringBuilder, ByRef current_adr As UInt32, write As Boolean)
        Dim a As UInt32
        If value.Contains(",") Then
            Dim charss As String() = value.Split(",")
            For Each chars In charss
                a = CalculateValue(chars)
                'listing.Append(HEX2(a >> 8) + " " + HEX2(a And 255) + " ")
                listing.Append(HexPair(a))
                If write Then CD(a >> 8) : CD(a And 255)
                current_adr += 2
            Next
        Else
            a = CalculateValue(value)
            'listing.Append(HEX2(a >> 8) + " " + HEX2(a And 255))
            listing.Append(HexPair(a))
            If write Then CD(a >> 8) : CD(a And 255)
            current_adr += 2
        End If

    End Sub
    ''' <summary>
    ''' Calculate DS value to data.
    ''' </summary>
    ''' <param name="value">Value</param>
    ''' <param name="listing">Pointer to listing</param>
    ''' <param name="current_adr">Current adr of DB</param>
    Private Sub CalculateDS(value As String, ByRef listing As StringBuilder, ByRef current_adr As UInt32)
        If value <> "" Then
            current_adr = current_adr + CalculateValue(value)
        End If
    End Sub
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        labels.Clear()
        labels_adr.Clear()
        RichTextBox2.Clear()
        errors.Clear()
        RAM.Initialize()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        File.WriteAllBytes("C:\Users\User\Desktop\Altair32\files\asm\TESTAS.BIN", RAM)
    End Sub

    Private Function CleanSpacesProperly(value As String) As String
        Dim stopas As Boolean = False
        Dim i As UInt32 = 0
        Dim radau As Boolean = False
        Dim tabai As Byte = 0
        value = value.Replace(vbTab, " ")
        value = value.TrimEnd(trimchars)
        If value = "" Then Return ""

        While Not stopas
            If value.Chars(i) <> " " Then
                i = i + 1
                radau = False
            ElseIf value.Chars(i) = " " And Not radau Then
                radau = True
                i = i + 1
                tabai = tabai + 1
            Else
                value = value.Remove(i, 1)
            End If

            If i >= value.Length - 1 Or tabai = 4 Then stopas = True
        End While
        Return value
    End Function


    End Sub
End Class
