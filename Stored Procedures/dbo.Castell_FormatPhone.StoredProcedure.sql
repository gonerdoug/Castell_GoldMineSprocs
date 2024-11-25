SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Castell_FormatPhone]') AND type in (N'P', N'PC'))
BEGIN
EXEC dbo.sp_executesql @statement = N'CREATE PROCEDURE [dbo].[Castell_FormatPhone] AS' 
END
GO








ALTER PROCEDURE [dbo].[Castell_FormatPhone] (@phone varchar(25) OUTPUT)

AS

SET NOCOUNT ON

DECLARE @retval int

SELECT @retval = 1

/* 3101234567 */
IF (DATALENGTH(@phone) = 10 AND @phone NOT LIKE '(%' AND @phone NOT LIKE '%)%' 
    AND @phone NOT LIKE '%-%')

	SELECT @phone = '(' + SUBSTRING(@phone,1,3) + ')' + SUBSTRING(@phone, 4, 3) +
			 '-' + SUBSTRING(@phone,7,4)
/* (310)1234567 */
ELSE IF (DATALENGTH(@phone) = 12 AND @phone LIKE '(%' AND SUBSTRING(@phone,5,1) = ')'
	AND @phone NOT LIKE '%-%')
		
	SELECT @phone = SUBSTRING(@phone, 1, 8) + '-' + SUBSTRING(@phone, 9, 4)
	
/* 310-123-4567 OR 310/123-4567*/
ELSE IF (DATALENGTH(@phone) = 12 AND @phone NOT LIKE '(%' AND @phone NOT LIKE '%)%' AND
		 SUBSTRING(@phone, 4,1) IN ('-', '/') AND SUBSTRING(@phone,8,1) = '-')

		SELECT @phone = '(' + SUBSTRING(@phone, 1,3) + ')' + SUBSTRING(@phone, 5, 8)

/* 310 123 4567 */
ELSE IF (DATALENGTH(@phone) = 12 AND @phone NOT LIKE '(%' AND @phone NOT LIKE '%)%' AND
         SUBSTRING(@phone, 4, 1) = ' ' AND SUBSTRING(@phone, 8, 1) = ' ')

         SELECT @phone = '(' + SUBSTRING(@phone, 1,3) + ')' + SUBSTRING(@phone, 5, 3) +
                         '-' + SUBSTRING(@phone, 9, 4)
ELSE SELECT @retval = 0

RETURN @retval


                         

 







GO
