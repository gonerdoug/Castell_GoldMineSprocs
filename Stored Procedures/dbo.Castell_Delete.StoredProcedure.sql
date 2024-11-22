SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_Delete]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_Delete] AS' 
END
GO








ALTER PROCEDURE [dbo].[Castell_Delete] (@gmnv varchar(20))
    
AS

SET NOCOUNT ON

IF (SELECT object_id('##gmnv' + @gmnv )) IS NOT NULL
    BEGIN
		EXEC( 'DROP TABLE ' + '##gmnv' + @gmnv )
    END

ELSE RETURN -120

    




GO
