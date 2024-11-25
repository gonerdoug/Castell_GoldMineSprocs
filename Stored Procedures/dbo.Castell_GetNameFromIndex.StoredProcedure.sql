SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_GetNameFromIndex]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_GetNameFromIndex] AS' 
END
GO









ALTER PROCEDURE [dbo].[Castell_GetNameFromIndex] (@gmnv varchar(20), @indexval int, @name varchar(20) OUTPUT)

AS

SET NOCOUNT ON

DECLARE @namefield varchar(20)
DECLARE @indexfield  int
DECLARE @retval int

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
   BEGIN
        SELECT @retval = -120
        GOTO Error
   END

SELECT @namefield 'name', @indexval 'indexval' INTO #tempgmnv
DELETE FROM #tempgmnv 

INSERT INTO #tempgmnv
EXEC ('SELECT fieldname, indexval FROM ##gmnv' + @gmnv)

DECLARE fieldcursor CURSOR 
FOR SELECT name, indexval FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @namefield, @indexfield

SELECT @retval = 0

WHILE @@fetch_status = 0
    BEGIN
        IF @indexfield = @indexval
            GOTO TheEnd

        FETCH NEXT FROM fieldcursor INTO @namefield, @indexfield
    END

CLOSE fieldcursor
DEALLOCATE fieldcursor

Error:
RETURN @retval

TheEnd:
CLOSE fieldcursor
DEALLOCATE fieldcursor
SELECT @name = @namefield








GO
