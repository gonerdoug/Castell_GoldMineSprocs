declare @gmnv varchar(20),
		@recid varchar(15)

set nocount OFF

EXEC Castell_Create @gmnv OUTPUT
--Only supply a RECID pair if you're UPDATING an existing record
--EXEC Castell_SetValue @gmnv, 'recid', 'H5CBH6J5%<H%T&@'
EXEC Castell_SetValue @gmnv, 'accountno', 'B2021049161#8B0{ FF6'
EXEC Castell_SetValue @gmnv, 'rectype', 'a'
EXEC Castell_SetValue @gmnv, 'user', 'DOUG'
EXEC Castell_SetValue @gmnv, 'notes', 'John called to report how much he enjoys owning his widgets'
EXEC Castell_SetValue @gmnv, 'ref', 'Happy customer likes widgets'
EXEC Castell_SetValue @gmnv, 'actvcode', 'FBK'
EXEC Castell_SetValue @gmnv, 'resultcode', 'COM'
EXEC Castell_SetValue @gmnv, 'ondate', '01/30/2015'
EXEC Castell_SetValue @gmnv, 'ontime', '09:00'
EXEC Castell_SetValue @gmnv, 'duration', '90'
EXEC Castell_SetValue @gmnv, 'contact', 'John Smith'
--Pass the recid of an existing cal record you wish to comlpete to a history record
--EXEC Castell_SetValue @gmnv, 'calrecid', 'G2P6TL1#V, /T&@'

EXEC Castell_WriteHistory @gmnv
EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT
print @recid

EXEC dbo.Castell_Delete @gmnv