declare @gmnv varchar(20),
		@recid varchar(15)

set nocount OFF
 
EXEC dbo.Castell_Create @gmnv OUTPUT
--This snippet adds notes to an existing opportunity
--Only supply a RECID pair if you're UPDATING an existing record
--EXEC dbo.Castell_SetValue @gmnv, 'Recid', 'GXZDVN2#SIYUT&@'
EXEC dbo.Castell_SetValue @gmnv, 'Userid', 'MASTER'
EXEC dbo.Castell_SetValue @gmnv, 'Name', 'A big opportunity'
EXEC dbo.Castell_SetValue @gmnv, 'accountno', 'B5040744202%^@TFHLen'
EXEC dbo.Castell_SetValue @gmnv, 'startdate', '05/22/2013'
EXEC dbo.Castell_SetValue @gmnv, 'closeddate', '07/22/2013'
EXEC dbo.Castell_SetValue @gmnv, 'closeby', '07/22/2013'
EXEC dbo.Castell_SetValue @gmnv, 'units', '10' -- units for forecasted sale
EXEC dbo.Castell_SetValue @gmnv, 'forprob', '50' -- probability for forecasted sale
EXEC dbo.Castell_SetValue @gmnv, 'foramt', '1000' -- forecast amount for forecasted sale
EXEC dbo.Castell_SetValue @gmnv, 'notes', 'Notes are here'

EXEC Castell_WriteOpportunity @gmnv

Exec dbo.Castell_GetValue @gmnv,'RECID', @recid OUTPUT
print @recid

EXEC dbo.Castell_Delete @gmnv


