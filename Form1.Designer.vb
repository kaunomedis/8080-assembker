<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class ASM8080
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(ASM8080))
        Me.RichTextBox1 = New System.Windows.Forms.RichTextBox()
        Me.RichTextBox2 = New System.Windows.Forms.RichTextBox()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.FileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.LoadAssemblerSourceToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.SaveIntelHexToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveAllRAMToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveListingToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveLabelDatabaseToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.ExitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.NewSourceToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SavelSourceToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'RichTextBox1
        '
        Me.RichTextBox1.AcceptsTab = True
        Me.RichTextBox1.Location = New System.Drawing.Point(12, 23)
        Me.RichTextBox1.Name = "RichTextBox1"
        Me.RichTextBox1.Size = New System.Drawing.Size(264, 415)
        Me.RichTextBox1.TabIndex = 0
        Me.RichTextBox1.Text = resources.GetString("RichTextBox1.Text")
        '
        'RichTextBox2
        '
        Me.RichTextBox2.Font = New System.Drawing.Font("Consolas", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.RichTextBox2.Location = New System.Drawing.Point(282, 23)
        Me.RichTextBox2.Name = "RichTextBox2"
        Me.RichTextBox2.Size = New System.Drawing.Size(626, 415)
        Me.RichTextBox2.TabIndex = 1
        Me.RichTextBox2.Text = ""
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(914, 23)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(80, 38)
        Me.Button1.TabIndex = 2
        Me.Button1.Text = "ASSEMB"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Me.Button2.Location = New System.Drawing.Point(914, 67)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(80, 38)
        Me.Button2.TabIndex = 3
        Me.Button2.Text = "Clear"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileToolStripMenuItem})
        Me.MenuStrip1.Location = New System.Drawing.Point(0, 0)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.Size = New System.Drawing.Size(1006, 24)
        Me.MenuStrip1.TabIndex = 5
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'FileToolStripMenuItem
        '
        Me.FileToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NewSourceToolStripMenuItem, Me.LoadAssemblerSourceToolStripMenuItem, Me.ToolStripSeparator1, Me.SavelSourceToolStripMenuItem, Me.SaveIntelHexToolStripMenuItem, Me.SaveAllRAMToolStripMenuItem, Me.SaveListingToolStripMenuItem, Me.SaveLabelDatabaseToolStripMenuItem, Me.ToolStripSeparator2, Me.ExitToolStripMenuItem})
        Me.FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        Me.FileToolStripMenuItem.Size = New System.Drawing.Size(37, 20)
        Me.FileToolStripMenuItem.Text = "File"
        '
        'LoadAssemblerSourceToolStripMenuItem
        '
        Me.LoadAssemblerSourceToolStripMenuItem.Name = "LoadAssemblerSourceToolStripMenuItem"
        Me.LoadAssemblerSourceToolStripMenuItem.Size = New System.Drawing.Size(194, 22)
        Me.LoadAssemblerSourceToolStripMenuItem.Text = "Load assembler source"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(191, 6)
        '
        'SaveIntelHexToolStripMenuItem
        '
        Me.SaveIntelHexToolStripMenuItem.Name = "SaveIntelHexToolStripMenuItem"
        Me.SaveIntelHexToolStripMenuItem.Size = New System.Drawing.Size(194, 22)
        Me.SaveIntelHexToolStripMenuItem.Text = "Save IntelHex"
        '
        'SaveAllRAMToolStripMenuItem
        '
        Me.SaveAllRAMToolStripMenuItem.Name = "SaveAllRAMToolStripMenuItem"
        Me.SaveAllRAMToolStripMenuItem.Size = New System.Drawing.Size(194, 22)
        Me.SaveAllRAMToolStripMenuItem.Text = "Save All RAM"
        '
        'SaveListingToolStripMenuItem
        '
        Me.SaveListingToolStripMenuItem.Name = "SaveListingToolStripMenuItem"
        Me.SaveListingToolStripMenuItem.Size = New System.Drawing.Size(194, 22)
        Me.SaveListingToolStripMenuItem.Text = "Save Listing"
        '
        'SaveLabelDatabaseToolStripMenuItem
        '
        Me.SaveLabelDatabaseToolStripMenuItem.Name = "SaveLabelDatabaseToolStripMenuItem"
        Me.SaveLabelDatabaseToolStripMenuItem.Size = New System.Drawing.Size(194, 22)
        Me.SaveLabelDatabaseToolStripMenuItem.Text = "Save Label database"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(191, 6)
        '
        'ExitToolStripMenuItem
        '
        Me.ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        Me.ExitToolStripMenuItem.Size = New System.Drawing.Size(194, 22)
        Me.ExitToolStripMenuItem.Text = "Exit"
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        '
        'NewSourceToolStripMenuItem
        '
        Me.NewSourceToolStripMenuItem.Name = "NewSourceToolStripMenuItem"
        Me.NewSourceToolStripMenuItem.Size = New System.Drawing.Size(194, 22)
        Me.NewSourceToolStripMenuItem.Text = "New source"
        '
        'SavelSourceToolStripMenuItem
        '
        Me.SavelSourceToolStripMenuItem.Name = "SavelSourceToolStripMenuItem"
        Me.SavelSourceToolStripMenuItem.Size = New System.Drawing.Size(194, 22)
        Me.SavelSourceToolStripMenuItem.Text = "Savel source"
        '
        'ASM8080
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(1006, 450)
        Me.Controls.Add(Me.Button2)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.RichTextBox2)
        Me.Controls.Add(Me.RichTextBox1)
        Me.Controls.Add(Me.MenuStrip1)
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "ASM8080"
        Me.Text = "8080 asembleris"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents RichTextBox1 As RichTextBox
    Friend WithEvents RichTextBox2 As RichTextBox
    Friend WithEvents Button1 As Button
    Friend WithEvents Button2 As Button
    Friend WithEvents SaveFileDialog1 As SaveFileDialog
    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents FileToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents SaveIntelHexToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents SaveAllRAMToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents SaveListingToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents SaveLabelDatabaseToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ExitToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents LoadAssemblerSourceToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator
    Friend WithEvents ToolStripSeparator2 As ToolStripSeparator
    Friend WithEvents OpenFileDialog1 As OpenFileDialog
    Friend WithEvents NewSourceToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents SavelSourceToolStripMenuItem As ToolStripMenuItem
End Class
