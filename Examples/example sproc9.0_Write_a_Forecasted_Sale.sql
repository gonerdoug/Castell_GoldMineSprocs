declare @gmnv varchar(20),
		@recid varchar(15)

set nocount OFF

EXEC dbo.Castell_Create @gmnv OUTPUT
--Only supply a RECID pair if you're UPDATING an existing record
--EXEC dbo.Castell_SetValue @gmnv, 'Recid', ''
EXEC dbo.Castell_SetValue @gmnv, 'User', 'DOUG'
EXEC dbo.Castell_SetValue @gmnv, 'accountno', 'A1011765128%)\+WHSte'
EXEC dbo.Castell_SetValue @gmnv, 'rectype', 'S'
EXEC Castell_SetValue @gmnv, 'contact', 'John Smith'
--If this sale is to be linked to an opportunity, supply the opprotunity's RECID here
--EXEC dbo.Castell_SetValue @gmnv, 'loprecid', 'GXZWEBB:=PP7T&@'
EXEC dbo.Castell_SetValue @gmnv, 'actvcode', 'CON' 
EXEC dbo.Castell_SetValue @gmnv, 'ondate', '07/30/2018'
EXEC dbo.Castell_SetValue @gmnv, 'unitssale', 20 
EXEC dbo.Castell_SetValue @gmnv, 'potnsale', 1000 
EXEC dbo.Castell_SetValue @gmnv, 'probsale', 50 
EXEC dbo.Castell_SetValue @gmnv, 'notes', 'We agreed to give john a huge discount on widgets if he buys them soon!'
EXEC dbo.Castell_SetValue @gmnv, 'ref', 'Standard Widgets'

EXEC Castell_WriteSchedule @gmnv

Exec dbo.Castell_GetValue @gmnv,'RECID', @recid OUTPUT
print @recid

EXEC dbo.Castell_Delete @gmnv