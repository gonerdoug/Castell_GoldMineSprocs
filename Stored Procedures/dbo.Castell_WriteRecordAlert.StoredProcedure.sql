SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[Castell_WriteRecordAlert] (@accountno varchar(20), @AlertCode varchar(3), @LogHistory varchar(1), @User varchar(8)) 
AS 
SET NOCOUNT ON

declare 
@currstatus varchar (2),
@gmnv varchar(20),
@AlertVal varchar(15),
@CSRecid varchar(15)


if (@accountno<>'' and @accountno is not null) and (@AlertCode<>'' and @AlertCode is not null) and (@User<>'' and @User is not null)
	BEGIN
		--Get the current phone/curtaining values from the status field
		select @currstatus=left(status,2) from contact1 where accountno=@accountno

		--Set record alert status flag
		update dbo.contact1 set status=@currstatus+'1' where accountno=@accountno

		--remove any existing record alert on this record for the same code
		delete from contsupp where rectype='A' and accountno=@accountno and notes like '~'+@ALERTCODE+'~%'
		if @LogHistory<>'1' 
			BEGIN
				set @LogHistory=''
			END

		set @AlertVal='~'+@ALERTCODE+'~'+@user+@LogHistory

		--Add record alert
		EXEC dbo.Castell_Create @gmnv OUTPUT

		EXEC dbo.Castell_SetValue @gmnv, 'User', @User
		EXEC dbo.Castell_SetValue @gmnv, 'RecType', 'A'
		EXEC dbo.Castell_SetValue @gmnv, 'Accountno', @accountno 
		EXEC dbo.Castell_SetValue @gmnv, 'Notes', @AlertVal

		EXEC dbo.Castell_WriteContSupp @gmnv

		Exec dbo.Castell_GetValue @gmnv,'RECID', @CSRecid OUTPUT 
		EXEC dbo.Castell_EraseName @gmnv, 'recid'
		EXEC dbo.Castell_Delete @gmnv
	END
