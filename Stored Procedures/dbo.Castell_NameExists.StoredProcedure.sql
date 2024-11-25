SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_NameExists]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_NameExists] AS' 
END
GO









ALTER PROCEDURE [dbo].[Castell_NameExists] (@gmnv varchar(20), @name varchar(20))
    
AS

SET NOCOUNT ON

DECLARE @namefield varchar(20)
DECLARE @indexval  int
DECLARE @retval int

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
   BEGIN
        SELECT @retval = -120
        GOTO TheEnd
   END

SELECT @namefield 'name', @indexval 'indexval' INTO #tempgmnv
DELETE FROM #tempgmnv 

INSERT INTO #tempgmnv
EXEC ('SELECT name, indexval FROM ##gmnv' + @gmnv)

DECLARE fieldcursor CURSOR 
FOR SELECT name, indexval FROM #tempgmnv

OPEN fieldcursor

FETCH NEXT FROM fieldcursor INTO @namefield, @indexval

SELECT @retval = 0

WHILE @@fetch_status = 0
    BEGIN
        IF UPPER(@namefield) = UPPER(@name)
            BEGIN
                SELECT @retval = @indexval
                GOTO Done
            END
        FETCH NEXT FROM fieldcursor INTO @namefield, @indexval
    END

Done:
CLOSE fieldcursor
DEALLOCATE fieldcursor

TheEnd:
RETURN @retval









GO
