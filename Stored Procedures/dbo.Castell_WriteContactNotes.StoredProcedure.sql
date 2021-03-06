DROP PROCEDURE [dbo].[Castell_WriteContactNotes]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO






CREATE PROCEDURE [dbo].[Castell_WriteContactNotes] (@gmnv varchar(20))
    
AS
DECLARE @recid     varchar(15),
		@c1recid   varchar(15),
		@accountno varchar(20),
		@notes     varchar(8000),
		@user      varchar(8),
		@valid     int,
		@createddate datetime
		

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

EXEC Castell_GetValue @gmnv, 'user', @user OUTPUT

IF @user IS NULL
    RETURN -130

EXEC Castell_GetValue @gmnv, 'accountno', @accountno OUTPUT

IF @accountno IS NULL
    RETURN -170

EXEC Castell_GetValue @gmnv, 'notes', @notes OUTPUT

EXEC Castell_GetValue @gmnv, 'recid', @recid OUTPUT

set @createddate = GETDATE()

EXEC Castell_GetValue @gmnv, 'createddate', @createddate OUTPUT


if @recid is null --make a new note
		BEGIN
			If @notes is not NULL
				BEGIN
					set @recid = (select dbo.Castell_RecId('MASTER'))
					set @c1recid = (select recid from CONTACT1 where ACCOUNTNO=@accountno)
					INSERT INTO NOTES (CREATEDDATE,USERID,MODIFIEDDATE,MODIFIEDBY,LOPRECID,ACCOUNTNO,NOTE,RECTYPE,EXT,recid)
					VALUES (@createddate,@user,@createddate,@user,@c1recid,@accountno,@notes,'C1','html',@recid)
					EXEC Castell_UpdateSyncLog 'notes', @recid, @user, 'N'
				END
		END
else
		begin
				UPDATE notes SET note = @notes, MODIFIEDBY=@user, MODIFIEDDATE=GETDATE()  WHERE recid = @recid
		end

		









GO
