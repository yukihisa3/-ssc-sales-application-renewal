****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　販売管理システム　　　　　　　　　　　*
*    モジュール名　　：　日付＋指定日付後算出サブルーチン　　　*
*    作成日／更新日　：　97/09/16                              *
*    作成者／更新者　：　NAV                                   *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKTADDDT.
 AUTHOR.                T.T.
 DATE-WRITTEN.          97/09/16.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-6500.
 OBJECT-COMPUTER.       K-6500.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(02).
     03  CHK-02              PIC  9(02).
     03  CHK-03              PIC  9(02).
     03  CHK-04              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
 01  SSKTDTCK-YMD.
     03  SSKTDTCK-Y          PIC  9(02).
     03  SSKTDTCK-M          PIC  9(02).
     03  SSKTDTCK-D          PIC  9(02).
 01  SSKTDTCK-HIZUKE.
     03  SSKTDTCK-ADDDATE    PIC  9(02).
 01  SSKTDTCK-HENKAN.
     03  SSKTDTCK-HEN-Y      PIC  9(02).
     03  SSKTDTCK-HEN-M      PIC  9(02).
     03  SSKTDTCK-HEN-D      PIC  9(02).
 01  SSKTDTCK-RET            PIC  9(01).
*
******************************************************************
 PROCEDURE    DIVISION       USING     SSKTDTCK-YMD
                                       SSKTDTCK-HIZUKE
                                       SSKTDTCK-HENKAN
                                       SSKTDTCK-RET.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE     0         TO   SSKTDTCK-RET.
     DIVIDE   SSKTDTCK-Y     BY   4
              GIVING    CHK-01    REMAINDER      CHK-02.
     IF       CHK-02    =    0
              MOVE      29        TO   WK-MATUBI (2)
     ELSE
              MOVE      28        TO   WK-MATUBI (2)
     END-IF.
*基準日付のセット
     MOVE     SSKTDTCK-YMD        TO   SSKTDTCK-HENKAN.
*基準日付＋指定日付の加算
     MOVE     SSKTDTCK-ADDDATE    TO   CHK-03.
     MOVE     SSKTDTCK-D          TO   CHK-04.
     ADD      CHK-03              TO   CHK-04.
*加算後，日付の判定
     IF       CHK-04         >    WK-MATUBI (SSKTDTCK-M)
              ADD       1         TO   SSKTDTCK-HEN-M
              IF    SSKTDTCK-HEN-M  >  12
                    ADD     1     TO   SSKTDTCK-HEN-Y
                    MOVE   01     TO   SSKTDTCK-HEN-M
              END-IF
              COMPUTE  SSKTDTCK-HEN-D  =
                       CHK-04 - WK-MATUBI(SSKTDTCK-M)
     ELSE
              MOVE  CHK-04        TO   SSKTDTCK-HEN-D
     END-IF.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*-----------------<< PROGRAM END >>----------------------------*
