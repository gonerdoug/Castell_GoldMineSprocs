SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_EraseName]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_EraseName] AS' 
END
GO








ALTER PROCEDURE [dbo].[Castell_EraseName] (@gmnv varchar(20), @name varchar(20))
    
AS

SET NOCOUNT ON

/*returns -120 if gmnv does not exist*/

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

SELECT @name = UPPER(@name)
EXEC('DELETE FROM ##gmnv' + @gmnv + ' WHERE UPPER(fieldname) = '' + @name + ''')






GO
