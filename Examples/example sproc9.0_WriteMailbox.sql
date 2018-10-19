
declare @gmnv varchar(20),
		@recid varchar(15),
		@mailbody varchar(max)
		
set nocount OFF
--load e-mail content from external source (such as a knowledgebase entry)
select @mailbody=cast(cast(notes as varbinary(max))as varchar(max)) from INFOMINE where recid='KSDHW13%Z@/>,;Q'

EXEC Castell_Create @gmnv OUTPUT

EXEC Castell_SetValue @gmnv, 'accountno', 'B8082734758&7AM(,joe'
EXEC Castell_SetValue @gmnv, 'user', 'DOUG'
EXEC Castell_SetValue @gmnv, 'contact', 'Joe Smith'
EXEC Castell_SetValue @gmnv, 'direction', 'O' -- O or I for OUTGOING or INCOMING
EXEC Castell_SetValue @gmnv, 'maildate', '09/22/2018' --Optional. If omitted, current date used
EXEC Castell_SetValue @gmnv, 'mailtime', '11:00:00' --Optional. If omitted, current time used
EXEC Castell_SetValue @gmnv, 'mailto', 'Joe Smith <joe@joeunrealsmith.com>'
EXEC Castell_SetValue @gmnv, 'mailfrom', 'Doug Castell <doug@castellcomputers.com>'
EXEC Castell_SetValue @gmnv, 'subject', 'See the attached readme'
--EXEC Castell_SetValue @gmnv, 'folder', 'Sent' -- Optional. If omitted, SENT/FILED main folder level will be selected based on direction NV pair
--EXEC Castell_SetValue @gmnv, 'folder2', 'Testing' -- Optional. If omitted, the most recently-created SENT/FILED sub-folder will be used.
EXEC Castell_SetValue @gmnv, 'attachment1', 'c:\users\doug\desktop\readme.txt'

EXEC Castell_WriteMailbox @gmnv, @mailbody
EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT
print @recid

EXEC dbo.Castell_Delete @gmnv

