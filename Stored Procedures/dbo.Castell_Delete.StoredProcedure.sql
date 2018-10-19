DROP PROCEDURE [dbo].[Castell_Delete]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[Castell_Delete] (@gmnv varchar(20))
    
AS
DECLARE @SQL AS NVARCHAR(200)
SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NOT NULL
    BEGIN
		SET @SQL = 'DROP TABLE ' + '##gmnv' + @gmnv 
		EXECUTE sp_executesql @SQL
    END
    --EXEC Castell_EraseAll @gmnv
ELSE RETURN -120

    


GO
