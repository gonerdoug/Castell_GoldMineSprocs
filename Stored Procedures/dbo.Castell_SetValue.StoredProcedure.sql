SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_SetValue]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_SetValue] AS' 
END
GO









ALTER PROCEDURE [dbo].[Castell_SetValue](@gmnv varchar(20), @fieldname varchar(20), @value varchar(MAX))

AS

SET NOCOUNT ON

/*RETURNS -120 IF TABLE DOES NOT EXIST*/
DECLARE @dummy varchar(20)
DECLARE @maxindex int
DECLARE @smax varchar(4)

IF (SELECT object_id('tempdb..##gmnv' + @gmnv )) IS NULL
    RETURN -120

SELECT @dummy 'fieldname', @maxindex 'indexval' INTO #tempbuffer
DELETE FROM #tempbuffer 

INSERT INTO #tempbuffer
EXEC('SELECT fieldname, indexval FROM ##gmnv' + @gmnv + ' WHERE fieldname = "' + @fieldname + '"')


IF (SELECT fieldname FROM #tempbuffer) IS NULL
    BEGIN

        /*get the highest index in the gmnv currently*/
        SELECT @maxindex 'maxindex' INTO #tempindexbuf
        DELETE FROM #tempindexbuf
        INSERT INTO #tempindexbuf
            EXEC('SELECT MAX(indexval) FROM ##gmnv' + @gmnv)

        /*if the gmnv is empty, assign the index to be 1*/
        SELECT @maxindex = CASE maxindex WHEN NULL THEN 1
                                         ELSE maxindex + 1
                           END
        FROM #tempindexbuf

        SELECT @smax = CONVERT(varchar,@maxindex)

        EXEC('INSERT ##gmnv' + @gmnv + ' VALUES("' + @smax + '","' + @fieldname + '","' + @value + '")')

    END
ELSE
    BEGIN
        SELECT @maxindex = indexval FROM #tempbuffer  
        EXEC("UPDATE ##gmnv" + @gmnv + ' SET value = "' + @value + '" WHERE fieldname = "' 
             + @fieldname + '"')
    END

RETURN isnull(@maxindex, '0')











GO
