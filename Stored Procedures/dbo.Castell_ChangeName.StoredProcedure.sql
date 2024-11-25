SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_ChangeName]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_ChangeName] AS' 
END
GO





ALTER PROCEDURE [dbo].[Castell_ChangeName] (@gmnv varchar(20), @oldname varchar(20), @newname varchar(20))
    
AS

SET NOCOUNT ON

DECLARE @upcount int
DECLARE @cntcmd varchar(40)

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

SELECT @oldname = UPPER(@oldname)
SELECT @newname = UPPER(@newname)

SELECT @upcount 'upcount' INTO #counttemp /*create the temp table to store rowcount
							                         results of update*/
DELETE FROM #counttemp 
SELECT @cntcmd = ' insert #counttemp select @@ROWCOUNT'

EXEC('UPDATE ##gmnv' + @gmnv + ' SET fieldname = ''' + @newname + ''' WHERE UPPER(fieldname) = ''' + @oldname + ''''
     + @cntcmd)

SELECT @upcount = upcount FROM #counttemp

RETURN @upcount









GO
