****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　販売管理システム　　　　　　　　　　　*
*    モジュール名　　：　モジュラス１１ＣＤ算出チェックメイン　*
*    作成日／更新日　：　2013/03/18                            *
*    作成者／更新者　：　NAV TAKEI                             *
*                        LO-ERR    :  0:正常終了               *
*                                     1:チェックデジットエラー *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSKTCD11.
 AUTHOR.                NAV TAKEI.
 DATE-WRITTEN.          13/03/18.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
******************************************************************
 DATA                      DIVISION.
******************************************************************
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WORK-AREA.
     03  IN-DATA             PIC  9(09).
     03  WK-CD               PIC  9(04).
     03  WK-CALC             PIC  9(01).
     03  WK-CALC-2           PIC  9(02).
     03  WK-DIV-1            PIC  9(03).
     03  WK-DIV-2            PIC  9(03).
     03  WK-KETA             PIC  9(01).
     03  CALC-CD             PIC  9(01).
     03  WK-DENNO            PIC  9(09).
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  WK-DENCHK.
         05  WK-DENCHK1      PIC  9(08).
         05  WK-DENCHK2      PIC  9(01).

*--------------------------------------------------------------*
*LINKAGE                SECTION.
*--------------------------------------------------------------*
**** LINK-AREA.
***  03  LINK-IN.
 01  LI-DATA         PIC  9(09).
***  03  LINK-OUT.
 01  LO-CD           PIC  9(01).
 01  LO-ERR          PIC  9(01).
*
******************************************************************
 PROCEDURE                 DIVISION.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     INITIALIZE         LI-DATA  LO-CD  LO-ERR.
     ACCEPT  IN-DATA  FROM   CONS.
     MOVE    IN-DATA  TO     LI-DATA.
     CALL  "SBMCCD11"    USING  LI-DATA  LO-CD  LO-ERR.
     DISPLAY  "CD="   LO-CD  UPON  CONS.
     STOP  RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
