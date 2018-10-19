declare @gmnv varchar(20),
		@recid varchar(15)

set nocount OFF

EXEC Castell_Create @gmnv OUTPUT
--Only supply a RECID pair if you're UPDATING an existing record
--EXEC Castell_SetValue @gmnv, 'recid', 'H5CBDQ2I9\S(T&@'
EXEC Castell_SetValue @gmnv, 'contact', 'John Smith'
EXEC Castell_SetValue @gmnv, 'address1', '123 Sunset Blvd.'
EXEC Castell_SetValue @gmnv, 'state', 'CA'
EXEC Castell_SetValue @gmnv, 'phone1', '3102111212'
EXEC Castell_SetValue @gmnv, 'key1', 'Lead'
EXEC Castell_SetValue @gmnv, 'email', 'jsmith23@smithcoincllc.com'
EXEC Castell_SetValue @gmnv, 'website', 'www.smithcoincllc.com'
EXEC Castell_SetValue @gmnv, 'user', 'DOUG'
EXEC Castell_SetValue @gmnv, 'userdef01', 'some value'
EXEC Castell_SetValue @gmnv, 'NOTES', 'I like to enter notes and more motes and type and type and type'

EXEC Castell_WriteContact @gmnv
EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT
print @recid

EXEC Castell_Delete @gmnv