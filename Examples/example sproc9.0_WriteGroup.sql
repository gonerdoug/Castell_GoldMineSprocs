declare @gmnv varchar(20),
		@recid varchar(15)

set nocount OFF

EXEC Castell_Create @gmnv OUTPUT
EXEC Castell_SetValue @gmnv, 'name', 'Show leads 35'
EXEC Castell_SetValue @gmnv, 'recid', 'H6HERDB1G3F7T&@'
EXEC Castell_SetValue @gmnv, 'code', 'LDS'
EXEC Castell_SetValue @gmnv, 'sync', 1
EXEC Castell_SetValue @gmnv, 'user', 'DOUG'

EXEC Castell_WriteGroup @gmnv
EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT

print @recid

EXEC Castell_Delete @gmnv


