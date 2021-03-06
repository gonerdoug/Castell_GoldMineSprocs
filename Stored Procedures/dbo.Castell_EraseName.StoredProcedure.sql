DROP PROCEDURE [dbo].[Castell_EraseName]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[Castell_EraseName] (@gmnv varchar(20), @name varchar(20))
    
AS

SET NOCOUNT ON

/*returns -120 if gmnv does not exist*/

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

SELECT @name = UPPER(@name)
EXEC('DELETE FROM ##gmnv' + @gmnv + ' WHERE UPPER(fieldname) = '' + @name + ''')




GO
