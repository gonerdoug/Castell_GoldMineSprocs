declare @gmnv varchar(20),
		@recid varchar(15)

set nocount OFF

EXEC Castell_Create @gmnv OUTPUT
--Only supply a RECID pair if you're UPDATING an existing record
--EXEC Castell_SetValue @gmnv, 'recid', 'G2P4Q5LF.=5<T&@'
EXEC Castell_SetValue @gmnv, 'accountno', 'B2021049161#8B0{ FF6'
EXEC Castell_SetValue @gmnv, 'rectype', 'a'
EXEC Castell_SetValue @gmnv, 'user', 'DOUG'
EXEC Castell_SetValue @gmnv, 'notes', 'New customer interested in information about our widgets.'
EXEC Castell_SetValue @gmnv, 'contact', 'John Smith'
EXEC Castell_SetValue @gmnv, 'ref', 'Seeking info'
EXEC Castell_SetValue @gmnv, 'actvcode', 'TST'
EXEC Castell_SetValue @gmnv, 'ondate', '01/15/2015'
EXEC Castell_SetValue @gmnv, 'ontime', '02:00'
EXEC Castell_SetValue @gmnv, 'alarm', '1'
EXEC Castell_SetValue @gmnv, 'alarmdate', '01/15/2015'
EXEC Castell_SetValue @gmnv, 'alarmtime', '08:50'
EXEC Castell_SetValue @gmnv, 'duration', '90'

EXEC Castell_WriteSchedule @gmnv
EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT
print @recid

EXEC dbo.Castell_Delete @gmnv
