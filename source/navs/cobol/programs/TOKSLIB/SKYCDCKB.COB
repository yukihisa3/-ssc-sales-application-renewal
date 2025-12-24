****************************************************************
*    顧客名　　　　　：　_サカタのタネ　特販部　殿向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　ＪＡＮコード変換                      *
*    作成日／更新日　：　1999/01/11                            *
*    作成者／更新者　：　NAV-ASSIST                            *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SKYCDCKB.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2000/01/11.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
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
****  チェックデジット計算
 01  WK-STEP1                 PIC 9(03)  VALUE  ZERO.
 01  WK-STEP2                 PIC 9(03)  VALUE  ZERO.
 01  WK-STEP3                 PIC 9(03)  VALUE  ZERO.
 01  WK-STEP4                 PIC 9(03)  VALUE  ZERO.
 01  WK-STEP5                 PIC 9(03)  VALUE  ZERO.
 01  WK-CHKDG-AREA.
   02  WK-CHKDG.
     03  WK-CHKDG-13          PIC 9(01).
     03  WK-CHKDG-12          PIC 9(01).
     03  WK-CHKDG-11          PIC 9(01).
     03  WK-CHKDG-10          PIC 9(01).
     03  WK-CHKDG-9           PIC 9(01).
     03  WK-CHKDG-8           PIC 9(01).
     03  WK-CHKDG-7           PIC 9(01).
     03  WK-CHKDG-6           PIC 9(01).
     03  WK-CHKDG-5           PIC 9(01).
     03  WK-CHKDG-4           PIC 9(01).
     03  WK-CHKDG-3           PIC 9(01).
     03  WK-CHKDG-2           PIC 9(01).
   02  WK-CHKDG-1             PIC 9(01).
 01  WK-BAISU-RR.
   02  WK-BAISU-1             PIC 9(02)  VALUE  ZERO.
   02  WK-BAISU-2             PIC 9(01)  VALUE  ZERO.
*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
 01  LINK-IN-JANCD         PIC 9(12).
 01  LINK-OUT-RTCD         PIC 9(01).
 01  LINK-OUT-CHKD         PIC 9(01).
*
******************************************************************
 PROCEDURE    DIVISION       USING     LINK-IN-JANCD
                                       LINK-OUT-RTCD
                                       LINK-OUT-CHKD.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     IF       LINK-IN-JANCD  =  ZERO
              MOVE    1      TO      LINK-OUT-RTCD
              GO             TO      000-PROG-CNTL-EXIT
     ELSE
              MOVE    ZERO   TO      LINK-OUT-RTCD
     END-IF.
     INITIALIZE   WK-CHKDG-AREA.
     MOVE     LINK-IN-JANCD  TO    WK-CHKDG.
*
     COMPUTE  WK-STEP1  = WK-CHKDG-2 + WK-CHKDG-4  + WK-CHKDG-6
                        + WK-CHKDG-8 + WK-CHKDG-10 + WK-CHKDG-12.
     COMPUTE  WK-STEP2  = WK-STEP1   *   3.
     COMPUTE  WK-STEP3  = WK-CHKDG-3 + WK-CHKDG-5  + WK-CHKDG-7
                        + WK-CHKDG-9 + WK-CHKDG-11 + WK-CHKDG-13.
     COMPUTE  WK-STEP4  = WK-STEP2   + WK-STEP3.
     MOVE     WK-STEP4    TO           WK-BAISU-RR.
     COMPUTE  WK-STEP5  = 10         - WK-BAISU-2.
     MOVE     WK-STEP5    TO           LINK-OUT-CHKD.
*****DISPLAY "CHKD-SUB = " WK-STEP5  UPON  CONS.
 000-PROG-CNTL-EXIT.
     EXIT     PROGRAM.
*-----------------<< PROGRAM END >>----------------------------*
