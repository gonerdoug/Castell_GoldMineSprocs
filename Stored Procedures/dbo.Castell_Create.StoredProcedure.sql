DROP PROCEDURE [dbo].[Castell_Create]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO




CREATE PROCEDURE [dbo].[Castell_Create] (@gmnv varchar(20) OUTPUT)
AS

/* RETURNS -120 IF THE TABLE ALREADY EXISTS*/
SET NOCOUNT ON

DECLARE @dDate datetime
DECLARE @sTime varchar(8)

SELECT @dDate = getdate()

SELECT @sTime = RIGHT(REPLICATE('0', 8) + 
                CONVERT(varchar,(3600 * DATEPART(hour, @dDate)) + (60 * DATEPART(minute, @dDate)) + 
                     DATEPART(second, @dDate)) + CONVERT(varchar,DATEPART(ms, @dDate)), 8)

IF (SELECT object_id('tempdb..##gmnv' + @sTime )) IS NULL
    BEGIN
        EXEC ('DECLARE @index int DECLARE @name varchar(20) DECLARE @value varchar(8000) SELECT @index "indexval", @name "fieldname", @value "value" INTO ##gmnv' + @sTime)
        EXEC ('DELETE FROM ##gmnv' + @sTime)
        SELECT @gmnv = @sTime
    END
ELSE
    RETURN -120








GO
