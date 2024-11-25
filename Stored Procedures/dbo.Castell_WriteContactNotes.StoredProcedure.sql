SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_WriteContactNotes]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_WriteContactNotes] AS' 
END
GO










ALTER PROCEDURE [dbo].[Castell_WriteContactNotes] (@gmnv varchar(20))
    
AS
DECLARE @recid     varchar(15),
		@c1recid   varchar(15),
		@accountno varchar(20),
		@notes     varchar(max),
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
