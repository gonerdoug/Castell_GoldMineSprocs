declare @gmnv varchar(20),
		@recid varchar(15)

set nocount OFF

EXEC Castell_Create @gmnv OUTPUT
--Only supply a RECID pair if you're UPDATING an existing record
--EXEC Castell_SetValue @gmnv, 'recid', 'H5CBL64Z7CM@T&@'

--Supply the accounto to indicate which contact to link this file to
EXEC Castell_SetValue @gmnv, 'accountno', 'A5031478653%=E!DHDou'
EXEC Castell_SetValue @gmnv, 'filename', 'C:\Program Files\Castell Computers\Advanced Stored Procedures for GoldMine 32 Bit\castell.ico'
EXEC Castell_SetValue @gmnv, 'user', 'DOUG'
EXEC Castell_SetValue @gmnv, 'notes', 'This is a fantastic icon'
EXEC Castell_SetValue @gmnv, 'ref', 'great icon'

EXEC Castell_WriteLinkedDoc @gmnv
EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT
print @recid

EXEC dbo.Castell_Delete @gmnv
