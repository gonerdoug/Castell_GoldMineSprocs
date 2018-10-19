DROP PROCEDURE [dbo].[Castell_GetValueFromIndex]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO





CREATE PROCEDURE [dbo].[Castell_GetValueFromIndex] (@gmnv varchar(20), @indexval int, @value varchar(8000) OUTPUT)
    
AS

SET NOCOUNT ON

DECLARE @valuefield varchar(8000)
DECLARE @indexfield  int

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
   RETURN -120
   

SELECT @valuefield 'value', @indexval 'indexval' INTO #tempgmnv
DELETE FROM #tempgmnv 

INSERT INTO #tempgmnv
EXEC ('SELECT value, indexval FROM ##gmnv' + @gmnv)

DECLARE fieldcursor CURSOR 
FOR SELECT value, indexval FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @valuefield, @indexfield

WHILE @@fetch_status = 0
    BEGIN
        IF @indexfield = @indexval
            GOTO TheEnd
           
        FETCH NEXT FROM fieldcursor INTO @valuefield, @indexfield
    END



TheEnd:
CLOSE fieldcursor
DEALLOCATE fieldcursor
SELECT @value = @valuefield






GO
