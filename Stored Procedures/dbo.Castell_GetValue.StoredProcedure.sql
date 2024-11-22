SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_GetValue]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_GetValue] AS' 
END
GO









ALTER PROCEDURE [dbo].[Castell_GetValue] (@gmnv varchar(20), @name varchar(20), @value varchar(max) OUTPUT)
    
AS

/*RETURNS -120 IF TABLE DOES NOT EXIST*/
SET NOCOUNT ON

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

SELECT @name = UPPER(@name)

SELECT @value 'value' INTO #temptable
DELETE FROM #temptable

INSERT INTO #temptable
EXEC('SELECT value FROM ##gmnv' + @gmnv + ' WHERE UPPER(fieldname) = ''' + @name + '''')

SELECT @value = value FROM #temptable










GO
