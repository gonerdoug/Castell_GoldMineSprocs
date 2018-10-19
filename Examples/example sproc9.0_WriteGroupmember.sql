declare @gmnv varchar(20),
		@recid varchar(15)

set nocount OFF

EXEC Castell_Create @gmnv OUTPUT
EXEC Castell_SetValue @gmnv, 'grouprecid', 'H6HCAQY:FV+:T&@' --the recid of the group we're adding someone to
--Only supply a RECID pair if you're UPDATING an existing record
--EXEC Castell_SetValue @gmnv, 'recid', 'H6HJTAFM%F6%T&@'
EXEC Castell_SetValue @gmnv, 'accountno', 'B2021049163 0JWF 706'
EXEC Castell_SetValue @gmnv, 'ref', 'AAA'
EXEC Castell_SetValue @gmnv, 'code', 'MKT'
EXEC Castell_SetValue @gmnv, 'user', 'DOUG'

EXEC Castell_WriteGroupMember @gmnv
EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT

print @recid

EXEC Castell_Delete @gmnv
