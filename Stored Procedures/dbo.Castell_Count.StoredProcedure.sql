SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_Count]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_Count] AS' 
END
GO








ALTER PROCEDURE [dbo].[Castell_Count] (@gmnv varchar(20))
    
AS

SET NOCOUNT ON

DECLARE @retval int

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
   BEGIN
        SELECT @retval = -120
        GOTO TheEnd
   END

SELECT @retval 'numnames' INTO #temptable
DELETE FROM #temptable

INSERT INTO #temptable
EXEC ('SELECT count(*) FROM ##gmnv' + @gmnv)

SELECT @retval = numnames FROM #temptable

TheEnd:
RETURN @retval





GO
