declare @gmnv varchar(20),
		@recid varchar(15)

set nocount OFF

EXEC Castell_Create @gmnv OUTPUT
--Only supply a RECID pair if you're UPDATING an existing record
--EXEC Castell_SetValue @gmnv, 'recid', 'H5CBH6J5%<H%T&@'
EXEC Castell_SetValue @gmnv, 'accountno', 'B5012647614&:;:!HKat'
EXEC Castell_SetValue @gmnv, 'user', 'DOUG'
EXEC Castell_SetValue @gmnv, 'detail', 'DetailName'
EXEC Castell_SetValue @gmnv, 'ref', 'A Reference line'
--The fields on the INFO tab of a detail record are referred to as ufield1 through ufield12.  
--They are size/type limited by the contsupp table structure.
EXEC Castell_SetValue @gmnv, 'ufield1', 'somevalue'
EXEC Castell_SetValue @gmnv, 'ufield4', 'other'
EXEC Castell_SetValue @gmnv, 'ufield6', 'fld6'

EXEC Castell_WriteDetail @gmnv
EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT
print @recid

EXEC Castell_Delete @gmnv